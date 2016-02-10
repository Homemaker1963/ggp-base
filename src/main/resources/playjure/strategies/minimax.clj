(ns playjure.strategies.minimax
  (:require [playjure.utils :refer :all])
  (:import
    [org.ggp.base.player.gamer.statemachine StateMachineGamer]
    [com.google.common.cache LoadingCache]))


(defmacro use-cached-if-possible [^LoadingCache cache state & body]
  `(let [^LoadingCache cache# ~cache]
     (if-let [cached# (.getIfPresent cache# ~state)]
       cached#
       (do ~@body))))

(defmacro write-through-cache [^LoadingCache cache state & body]
  `(let [^LoadingCache cache# ~cache
         result# (do ~@body)]
     (.put cache# ~state result#)
     result#))


(def next-move (atom nil))
(def original-roles (atom nil))
(def our-role (atom nil))
(def all-roles (atom nil))
(def need-more-iterations (atom nil))
(def finished-searching (atom nil))
(def minimax-bottom-value 1)


(defn create-joint-move [choices]
  (map choices @original-roles))


(declare minimax-search)
(defn minimax-turn [state-machine current-state choices turn-roles depth cache]
  (cond
    (thread-interrupted)
    [-1 nil]

    (empty? turn-roles)
    (minimax-search state-machine
                    (.getNextState state-machine
                                   current-state
                                   (create-joint-move choices))
                    depth
                    cache)

    :else
    (let [[[role eval-fn] & remaining-roles] turn-roles
          moves (.getLegalMoves state-machine current-state role)
          make-move (fn [move]
                      (let [[value _] (minimax-turn state-machine
                                                    current-state
                                                    (assoc choices role move)
                                                    remaining-roles
                                                    depth
                                                    cache)]
                        [value move]))
          results (map make-move moves)]
      (eval-fn results))))

(defn minimax-search [state-machine current-state depth cache]
  (use-cached-if-possible
    cache current-state
    (cond
      (.isTerminal state-machine current-state)
      [(.getGoal state-machine current-state @our-role) nil]

      (zero? depth)
      (do
        (reset! need-more-iterations true)
        [minimax-bottom-value nil])

      :else
      (write-through-cache
        cache current-state
        (minimax-turn state-machine
                      current-state
                      {}
                      @all-roles
                      (dec depth)
                      cache)))))

(defn iterate-minimax [state-machine starting-state]
  (loop [depth 1]
    (if-not (thread-interrupted)
      (do
        (println "Searching depth" depth)
        (reset! need-more-iterations false)
        (let [[score move] (minimax-search state-machine
                                           starting-state
                                           depth
                                           (fresh-cache))]
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


(defn eval-max [results]
  (first (sort-by first > results)))

(defn eval-min [results]
  (first (sort-by first < results)))


(defn get-minimax-roles [gamer]
  (let [roles (-> gamer .getStateMachine .getRoles)
        our-role (.getRole gamer)
        other-roles (remove #(= our-role %) roles)]
    (into [[our-role eval-max]]
          (map #(vector % eval-min) other-roles))))


(defn start-game [^StateMachineGamer gamer end-time]
  (reset! our-role (.getRole gamer))
  (reset! original-roles (-> gamer .getStateMachine .getRoles))
  (reset! all-roles (get-minimax-roles gamer)))

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
          our-moves (.getLegalMoves state-machine starting-state @our-role)]
      (if (= 1 (count our-moves))
        (first our-moves) ; If we only have one move, just take it.
        (let [worker (future (iterate-minimax state-machine starting-state))]
          (wait-til-done)
          (future-cancel-sanely worker)
          (second @next-move))))))

