(ns playjure.strategies.minimax
  (:require [playjure.utils :refer :all]
            [playjure.heuristics :as heur]
            [slingshot.slingshot :refer [try+ throw+]])
  (:import
    [org.ggp.base.player.gamer.statemachine StateMachineGamer]
    [com.google.common.cache LoadingCache]))


(def current-gamer (atom nil))
(def next-move (atom nil))
(def original-roles (atom nil))
(def our-role (atom nil))
(def all-roles (atom nil))
(def finished-searching (atom nil))
(def cache (atom nil))
(def cache-hits (atom nil))
(def previous-expected-value (atom nil))
(def heuristic (atom nil))
(def debug (= (System/getenv "DEBUG") "1"))
(def debug-log "debug.log")
(def ^:dynamic *log-indent* 0)

(defmacro inc-log-indent [& body]
  `(binding [*log-indent* (inc *log-indent*)]
     ~@body))

(defn log-indented [& args]
  (when debug
    (let [indent (apply str (repeat *log-indent* "    "))
          content (apply str indent (map stringify args))
          line (str content \newline)]
      (spit debug-log line :append true))))


(defn create-joint-move [choices]
  (map choices @original-roles))


(declare minimax-search)
(declare minimax-turn)

(defn update-bounds-from-cache [[score move cache-depth exact :as cached]
                                [min-bound max-bound :as bounds]
                                depth]
  (let [[new-min new-max :as new-bounds]
        (cond
          (not cached) bounds
          (<= cache-depth depth) bounds
          (= true exact) [score score]
          (= :lower-bound exact) [(max min-bound score) max-bound]
          (= :upper-bound exact) [min-bound (min max-bound score)]
          :else (throw+ {:type :everything-is-broken}))]
    (if (>= new-min new-max)
      (throw+ {:type :gtfo :cached cached})
      new-bounds)))


(defn process-moves [moves make-move
                     [role-name within-bounds update-best update-bounds
                      :as role-info]
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
      (let [[best-score best-move best-depth best-exact] best
            result [best-score best-move (apply min child-depths) best-exact]]
        (log-indented "Exhaustively searched moves for " role-name
                      ", best result: " (map str result))
        result)

      ; Otherwise we process another move.
      (do
        (log-indented "Next move to try for " role-name " is " (str move)
                      ", current best: " (map str best)
                      ", current bounds: " bounds)
        (let [[score move child-depth exact :as result] (make-move move bounds)]
          (if (not (within-bounds score bounds))
            (let [return-depth (apply min (conj child-depths child-depth))
                  return [score move return-depth false]]
              (log-indented "Move had score of " score
                            " which was not within bounds " bounds
                            ".  Pruning and returning result: " (map str return))
              return)
            (let [new-best (update-best result best)
                  new-bounds (update-bounds bounds new-best)]
              (log-indented "Got result of " (map str result)
                            " but couldn't prune with score " (first result)
                            " for " role-name
                            " against " bounds
                            ".  Continuing...")
              (recur (first moves)
                     (rest moves)
                     new-best
                     (conj child-depths child-depth)
                     new-bounds))))))))

(defn minimax-turn [state-machine current-state choices turn-roles depth bounds]
  (die-when-interrupted)
  (cond
    (empty? turn-roles)
    (minimax-search state-machine
                    (.getNextState state-machine current-state
                                   (create-joint-move choices))
                    depth
                    bounds)

    :else
    (let [[[role & role-info] & remaining-roles] turn-roles
          moves (sort-by str (.getLegalMoves state-machine current-state role))
          make-move (fn [move bounds]
                      (let [[value _ child-depth exact]
                            (inc-log-indent
                              (minimax-turn state-machine
                                            current-state
                                            (assoc choices role move)
                                            remaining-roles
                                            depth
                                            bounds))]
                        [value move child-depth exact]))]
      (log-indented "Searching node " (str role) " as " (str (first role-info)))
      (process-moves moves make-move role-info depth bounds))))

(defn minimax-search [state-machine current-state depth bounds]
  (die-when-interrupted)
  (log-indented "Searching state >>>" (str current-state) "<<< to depth " depth)

  (try+
    (let [[min-bound max-bound :as bounds]
          (update-bounds-from-cache (.getIfPresent @cache current-state)
                                    bounds
                                    depth)]
      (cond
        ; If we've hit a terminal state, we can put it in the cache with an infinite
        ; depth and we're done.
        (.isTerminal state-machine current-state)
        (let [result (put-through @cache current-state
                                  [(.getGoal state-machine current-state @our-role)
                                   nil infinity true])]
          (log-indented "Found a terminal state, returning: " (map str result))
          result)

        ; If we hit the iterative-deepening limit, we cache and return:
        ;   score   (heuristic)
        ;   move    nil
        ;   depth   0
        ;   exact   true
        (zero? depth)
        (let [result (put-through
                       @cache current-state
                       [(@heuristic @current-gamer current-state @our-role)
                        nil 0 true])]
          (log-indented "Hit the ID depth limit, returning: " (map str result))
          result)

        ; Otherwise we need to search further down the tree.  Find the result, cache
        ; it, and return it.
        :else
        (let [[score move child-depth _]
              (inc-log-indent (minimax-turn state-machine
                                            current-state
                                            {}
                                            @all-roles
                                            (dec depth)
                                            bounds))
              exact (cond
                      (<= score min-bound) :upper-bound
                      (<= max-bound score) :lower-bound
                      :else true)
              result (put-through @cache current-state
                                  [score move (safe-inc child-depth) exact])]
          (log-indented "Passing result back up: " (map str result))
          result)))
    (catch [:type :gtfo] {:keys [cached]}
      (log-indented "Using cached value: " (map str cached))
      (swap! cache-hits inc)
      cached)))


(defn iterate-minimax [state-machine starting-state]
  (try+
    (loop [depth 1]
      (println "Searching depth" depth)
      (log-indented "\n\nSearching depth: " depth)

      (let [[score move result-depth exact :as result]
            (inc-log-indent (minimax-search state-machine
                                            starting-state
                                            depth
                                            [-infinity infinity]))]
        (reset! next-move result)

        (println "    Best move:" (str move))
        (println "    Expected value:" score)
        (println "    Depth of result:" result-depth)
        (println "    Exact result:" exact)
        (println "    Cache hits:" @cache-hits)
        (println "    Approx. cache size:" (.size @cache))

        (log-indented "Best move: " (str move))
        (log-indented "Expected value: " score)
        (log-indented "Depth of result: " result-depth)
        (log-indented "Exact result: " exact)
        (log-indented "Cache hits: " @cache-hits)
        (log-indented "Approx. cache size: " (.size @cache))

        (cond
          (and exact (= infinity result-depth))
          (do
            (reset! finished-searching true)
            (println "Finished searching the entire tree, we're done here.")
            (log-indented "Finished searching the entire tree, we're done here."))

          ; (= 100 score)
          ; (do
          ;   (reset! finished-searching true)
          ;   (println "Found an ideal move, no need to search further."))

          :else
          (recur (inc depth)))))
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
    (into [[our-role :max within-bounds-max update-best-max update-bounds-max]]
          (map #(vector % :min within-bounds-min update-best-min update-bounds-min)
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
  (when debug (spit debug-log ""))
  (reset! current-gamer gamer)
  (reset! cache (fresh-cache))
  (reset! our-role (.getRole gamer))
  (reset! original-roles (-> gamer .getStateMachine .getRoles))
  (reset! all-roles (get-minimax-roles gamer))
  (reset! previous-expected-value -1)
  (reset! cache-hits 0)
  (reset! heuristic heur/inverse-mobility)
  (select-move gamer end-time)
  )

