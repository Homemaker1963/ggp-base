(ns playjure.strategies.minimax
  (:require [playjure.utils :refer :all]
            [slingshot.slingshot :refer [try+ throw+]])
  (:import
    [org.ggp.base.player.gamer.statemachine StateMachineGamer]
    [com.google.common.cache LoadingCache]))


(def next-move (atom nil))
(def original-roles (atom nil))
(def our-role (atom nil))
(def all-roles (atom nil))
(def finished-searching (atom nil))
(def cache (atom nil))
(def cache-hits (atom nil))
(def previous-expected-value (atom nil))

(def minimax-bottom-value 1)

(defn safe-inc [n]
  (cond
    (nil? n) nil
    (= infinity n) infinity
    :else (inc n)))

(defn create-joint-move [choices]
  (map choices @original-roles))


(declare minimax-search)
(declare minimax-turn)

(defn can-use-cache [^LoadingCache cache state depth bound]
  (let [[cache-score cache-move cache-depth cache-exact :as cached]
        (.getIfPresent cache state)]
    (and cached
         (<= depth cache-depth)
         (or cache-exact
             (<= bound cache-score)))))

(defn put-through [^LoadingCache cache state result]
  (.put cache state result)
  result)


(defn process-moves [moves make-move
                     [within-bounds update-best update-bounds :as role-info]
                     depth initial-bounds]
  (loop [move (first moves)
         moves (rest moves)
         best nil
         child-depths []
         bounds initial-bounds]
    (if (not move)
      ; If we have no more moves, we've iterated through the entire set of
      ; possibilities without pruning.  We return the best we've got so far,
      ; but we need to modify a couple of things:
      ;
      ;   1. This node is exact, because we exhausted all the moves.
      ;   2. The depth is the minimum of all the child depths.
      (let [[best-score best-move best-depth best-exact] best]
        [best-score best-move (apply min child-depths) true])

      ; Otherwise we process another move.
      (let [[score move child-depth exact :as result] (make-move move bounds)]
        (if (not (within-bounds score bounds))
          [score move child-depth false]
          (let [new-best (update-best result best)
                new-bounds (update-bounds bounds new-best)]
            (recur (first moves)
                   (rest moves)
                   new-best
                   (conj child-depths child-depth)
                   new-bounds)))))))

(defn minimax-turn [state-machine current-state choices turn-roles depth bounds]
  (when (thread-interrupted)
    (throw+ {:type :thread-interrupted}))
  (cond
    (empty? turn-roles)
    (minimax-search state-machine
                    (.getNextState state-machine current-state
                                   (create-joint-move choices))
                    depth
                    bounds)

    :else
    (let [[[role & role-info] & remaining-roles] turn-roles
          moves (.getLegalMoves state-machine current-state role)
          make-move (fn [move bounds]
                      (let [[value _ child-depth exact]
                            (minimax-turn state-machine
                                          current-state
                                          (assoc choices role move)
                                          remaining-roles
                                          depth
                                          bounds)]
                        [value move child-depth exact]))]
      (process-moves moves make-move role-info depth bounds))))

(defn minimax-search [state-machine current-state depth [_ max-bound :as bounds]]
  (when (thread-interrupted)
    (throw+ {:type :thread-interrupted}))
  (cond
    ; If we can use the cache here, go ahead and do it.
    (can-use-cache @cache current-state depth max-bound)
    (do
      (swap! cache-hits inc)
      (.get @cache current-state))

    ; If we've hit a terminal state, we can put it in the cache with an infinite
    ; depth and we're done.
    (.isTerminal state-machine current-state)
    (put-through @cache current-state
                 [(.getGoal state-machine current-state @our-role)
                  nil infinity true])

    ; If we hit the iterative-deepening limit, we cache and return:
    ;   score   1
    ;   move    nil
    ;   depth   0
    ;   exact   true
    (zero? depth)
    (put-through @cache current-state [minimax-bottom-value nil 0 true])

    ; Otherwise we need to search further down the tree.  Find the result, cache
    ; it, and return it.
    :else
    (let [[score move child-depth exact] (minimax-turn state-machine
                                                       current-state
                                                       {}
                                                       @all-roles
                                                       (dec depth)
                                                       bounds)]
      (put-through @cache current-state
                   [score move (safe-inc child-depth) exact]))))


(defn iterate-minimax [state-machine starting-state]
  (try+
    (loop [depth 1]
      (do
        (println "Searching depth" depth)
        (let [[score move result-depth exact :as result]
              (minimax-search state-machine
                              starting-state
                              depth
                              [-infinity infinity])]
          (dosync (reset! next-move result))

          (println "    Best move:" (str move))
          (println "    Expected value:" score)
          (println "    Depth of result:" result-depth)
          (println "    Exact result:" exact)
          (println "    Cache hits:" @cache-hits)
          (println "    Approx. cache size:" (.size @cache))

          (cond
            (and exact (= infinity result-depth))
            (do
              (reset! finished-searching true)
              (println "Finished searching the entire tree, we're done here."))

            ; (= 100 score)
            ; (do
            ;   (reset! finished-searching true)
            ;   (println "Found an ideal move, no need to search further."))

            :else
            (recur (inc depth))))))
    (catch [:type :thread-interrupted] _
      (println "Time ran out as we were searching."))))


(defn within-bounds-max
  "Returns true if the score is within the bounds."
  [score [_ best-max]]
  (< score best-max))

(defn within-bounds-min
  "Returns true if the score is within the bounds."
  [score [best-min _]]
  (> score best-min))

(defn update-best-max [[new-score :as new-result] [old-score :as old-result]]
  (cond
    (nil? old-result) new-result
    (> new-score old-score) new-result
    :else old-result))

(defn update-best-min [[new-score :as new-result] [old-score :as old-result]]
  (cond
    (nil? old-result) new-result
    (< new-score old-score) new-result
    :else old-result))

(defn update-bounds-max [[min-bound max-bound] [score & _]]
  [(max score min-bound)
   max-bound])

(defn update-bounds-min [[min-bound max-bound] [score & _]]
  [min-bound
   (min score max-bound)])


(defn get-minimax-roles [gamer]
  (let [roles (-> gamer .getStateMachine .getRoles)
        our-role (.getRole gamer)
        other-roles (remove #(= our-role %) roles)]
    (into [[our-role within-bounds-max update-best-max update-bounds-max]]
          (map #(vector % within-bounds-min update-best-min update-bounds-min)
               other-roles))))


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
      (let [[score move] @next-move]
        (println "Previous expected value: " @previous-expected-value)
        (when (< score @previous-expected-value)
          (println "

          #######  ##     ##     ######  ##     ## #### ########
         ##     ## ##     ##    ##    ## ##     ##  ##     ##
         ##     ## ##     ##    ##       ##     ##  ##     ##
         ##     ## #########     ######  #########  ##     ##
         ##     ## ##     ##          ## ##     ##  ##     ##
         ##     ## ##     ##    ##    ## ##     ##  ##     ##
          #######  ##     ##     ######  ##     ## ####    ##
                   "))
        (println "Choosing:" (str move) "with expected value" score)
        (reset! previous-expected-value score)
        move))))


(defn start-game [^StateMachineGamer gamer end-time]
  (reset! cache (fresh-cache))
  (reset! our-role (.getRole gamer))
  (reset! original-roles (-> gamer .getStateMachine .getRoles))
  (reset! all-roles (get-minimax-roles gamer))
  (reset! previous-expected-value -1)
  (reset! cache-hits 0)
  (select-move gamer end-time)
  )

