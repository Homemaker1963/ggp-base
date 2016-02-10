(ns playjure.strategies.minimax
  (:require [playjure.utils :refer :all])
  (:import
    [org.ggp.base.player.gamer.statemachine StateMachineGamer]
    [com.google.common.cache LoadingCache]))


(def next-move (atom nil))
(def original-roles (atom nil))
(def our-role (atom nil))
(def all-roles (atom nil))
(def need-more-iterations (atom nil))
(def finished-searching (atom nil))
(def cache (atom nil))

(def minimax-bottom-value 1)

(defn use-cached-if-possible [^LoadingCache cache state depth body]
  (let [cached (.getIfPresent cache state)]
    (if (or (not cached)
            (> depth (third cached)))
       (force body)
       cached)))

(defn write-through-cache [^LoadingCache cache state value]
  (.put cache state value)
  value)


(defn safe-inc [n]
  (cond
    (nil? n) nil
    (= infinity n) infinity
    :else (inc n)))

(defn create-joint-move [choices]
  (map choices @original-roles))


(declare minimax-search)

(defn minimax-turn [state-machine current-state choices turn-roles depth]
  (cond
    (thread-interrupted)
    [minimax-bottom-value nil nil]

    (empty? turn-roles)
    (minimax-search state-machine
                    (.getNextState state-machine
                                   current-state
                                   (create-joint-move choices))
                    depth)

    :else
    (let [[[role eval-fn] & remaining-roles] turn-roles
          moves (.getLegalMoves state-machine current-state role)
          make-move (fn [move]
                      (let [[value _ child-depth]
                            (minimax-turn state-machine
                                          current-state
                                          (assoc choices role move)
                                          remaining-roles
                                          depth)]
                        [value move child-depth]))
          results (map make-move moves)]
      (eval-fn results))))

(defn minimax-search [state-machine current-state depth]
  (use-cached-if-possible
    @cache current-state depth
    (delay
      (cond
        (.isTerminal state-machine current-state)
        (let [score (.getGoal state-machine current-state @our-role)]
          (.put @cache current-state [score nil infinity])
          [score nil infinity])

        (zero? depth)
        (let [score minimax-bottom-value]
          (reset! need-more-iterations true)
          (.put @cache current-state [score nil 0])
          [score nil 0])

        :else
        (let [[score move child-depth] (minimax-turn state-machine
                                                     current-state
                                                     {}
                                                     @all-roles
                                                     (dec depth))
              our-depth (safe-inc child-depth)]
          (when our-depth
            (.put @cache current-state [score move our-depth]))
          [score move our-depth])))))


(defn iterate-minimax [state-machine starting-state]
  (loop [depth 1]
    (if-not (thread-interrupted)
      (do
        (println "Searching depth" depth)
        (reset! need-more-iterations false)
        (let [[score move] (minimax-search state-machine
                                           starting-state
                                           depth)]
          (if-not (thread-interrupted)
            (do
              (dosync (reset! next-move [score move]))
              (println "    Best so far:" move score))))
        (if @need-more-iterations
          (if (= 100 (first @next-move))
            (do
              (reset! finished-searching true)
              (println "Found an ideal move, no need to search further."))
            (recur (inc depth)))
          (do
            (reset! finished-searching true)
            (println "Finished searching the entire tree, we're done here.")))))))

(defn eval-results [sort-fn results]
  (let [[best-score best-move _] (first (sort-by first sort-fn results))
        child-depths (map third results)
        effective-depth (when-not (some nil? child-depths)
                          (first (sort-by identity < child-depths)))]
    [best-score best-move effective-depth]))

(def eval-max (partial eval-results >))
(def eval-min (partial eval-results <))


(defn get-minimax-roles [gamer]
  (let [roles (-> gamer .getStateMachine .getRoles)
        our-role (.getRole gamer)
        other-roles (remove #(= our-role %) roles)]
    (into [[our-role eval-max]]
          (map #(vector % eval-min) other-roles))))


(defn select-move [gamer end-time]
  (reset! next-move nil)
  (reset! finished-searching false)
  (letfn [(time-left [end-time]
            (- end-time (System/currentTimeMillis)))
          (wait-til-done []
            (when (and (> (time-left end-time) response-cutoff)
                       (not @finished-searching))
              (Thread/sleep check-interval)
              (recur)))]
    (let [state-machine (.getStateMachine gamer)
          starting-state (.getCurrentState gamer)
          our-moves (.getLegalMoves state-machine starting-state @our-role)
          worker (future (iterate-minimax state-machine starting-state))]
      (wait-til-done)
      (future-cancel-sanely worker)
      (second @next-move))))

(defn start-game [^StateMachineGamer gamer end-time]
  (reset! cache (fresh-cache))
  (reset! our-role (.getRole gamer))
  (reset! original-roles (-> gamer .getStateMachine .getRoles))
  (reset! all-roles (get-minimax-roles gamer))
  (select-move gamer end-time))

