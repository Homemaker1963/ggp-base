(ns playjure.strategies.propnet-test
  (:require [com.climate.claypoole :as clay]
            [slingshot.slingshot :refer [try+ throw+]]
            [playjure.utils :refer :all]
            [playjure.heuristics :as heur])
  (:import
    [org.ggp.base.player.gamer.statemachine StateMachineGamer]
    [org.ggp.base.util.statemachine.cache CachedStateMachine]
    [org.ggp.base.util.statemachine.implementation.propnet PlayjurePropnetStateMachine]
    [org.ggp.base.util.propnet.factory OptimizingPropNetFactory]
    [com.google.common.cache LoadingCache]))


(def heuristic (atom nil))
(def next-move (atom nil))
(def cache (atom nil))
(def our-role (atom nil))
(def finished-searching (atom nil))
(def current-gamer (atom nil))

(defonce m (atom nil))

(defrecord DfsNode [role state-machine current-state])

(defn is-terminal [{:keys [role
                           ^CachedStateMachine state-machine
                           current-state]
                    :as node}]
  (.isTerminal state-machine current-state))

(defn state-value
  "Get the value of the current state"
  [{:keys [role
           ^CachedStateMachine state-machine
           current-state]
    :as node}]
  (.getGoal state-machine current-state role))

(defn get-moves [{:keys [role
                         ^CachedStateMachine state-machine
                         current-state]
                  :as node}]
  (.getLegalMoves state-machine current-state role))

(defn get-roles [{:keys [^CachedStateMachine state-machine]
                  :as node}]
  (.getRoles state-machine))

(defn make-move [{:keys [role
                         ^CachedStateMachine state-machine
                         current-state]
                  :as node} move]
  (.getNextState state-machine current-state [move]))


(defn pick-best [[old-score old-depth old-move :as old-result]
                 [new-score new-depth new-move :as new-result]]
  (cond
    (nil? old-result) new-result
    (> new-score old-score) [new-score (min old-depth new-depth) new-move]
    :else [old-score (min old-depth new-depth) old-move]))

(defn choose-child [children]
  (loop [[best-score best-depth :as best-child] nil
         [[score depth move :as child] & children] children]
    (cond
      (nil? child)
      best-child

      (= score 100)
      child

      :else
      (recur (pick-best best-child child)
             children))))


(defn dfs-full [node depth threadpool]
  (die-when-interrupted)
  (let [current-state (:current-state node)

        [cached-value cached-depth cached-move :as cached]
        (.getIfPresent @cache current-state)]
    (if (and cached (<= depth cached-depth))
      cached
      (cond
        (is-terminal node)
        (put-through @cache current-state
          [(state-value node) infinity nil])

        (zero? depth)
        (put-through @cache current-state
          [(@heuristic @current-gamer current-state @our-role) 0 nil])

        :else
        (let [children
              ((if threadpool (partial clay/upmap threadpool) map)
               (fn [move]
                 (let [[child-value child-depth child-move]
                       (dfs-full (assoc node :current-state
                                        (make-move node move))
                                 (dec depth)
                                 nil)]
                   [child-value (safe-inc child-depth) move]))
               (get-moves node))]
          (put-through @cache current-state
            (choose-child children)))))))

(defn iterative-deepening-dfs [start-node threadpool]
  (try+
    (loop [depth 1]
      (die-when-interrupted)
      (println "Searching depth" depth)
      (let [[result-score result-depth result-move :as result]
            (dfs-full start-node depth threadpool)]
        (reset! next-move result)
        (println "    Best move:" (str result-move))
        (println "    Expected value:" result-score)
        (println "    Depth of result:" result-depth)
        (println "    Approx. cache size:" (.size @cache))
        (if-not (or (= infinity result-depth)
                    (= result-score 100))
          (recur (inc depth))
          (do
            (reset! finished-searching true)
            (println "Finished searching the entire tree, we're done here.")))))
    (catch [:type :thread-interrupted] _
      (println "Time ran out as we were searching.")))
  true)

(defn select-move [gamer end-time]
  (reset! next-move nil)
  (reset! finished-searching false)
  (clay/with-shutdown! [threadpool (clay/threadpool (clay/ncpus))]
    (timed-run end-time @finished-searching
      (iterative-deepening-dfs
        (->DfsNode (.getRole gamer)
                   (.getStateMachine gamer)
                   (.getCurrentState gamer))
        threadpool)
      (let [[score _ move] @next-move]
        (println "Choosing:" (str move) "with expected value" score)
        move))))


(defn start-game [^StateMachineGamer gamer end-time]
  (println "Starting game with iterative-deepening depth-first search")
  (reset! m (new PlayjurePropnetStateMachine))
  (.initialize @m (-> gamer
                    .getMatch
                    .getGame
                    .getRules))
  (reset! cache (fresh-cache))
  (reset! heuristic (heur/select-heuristic))
  (reset! current-gamer gamer)
  (reset! our-role (.getRole gamer))
  (select-move gamer end-time))

(defonce current-state (atom nil))

(defn p-initial-state []
  (.getInitialState @m))

(defn p-reset-state []
  (reset! current-state (p-initial-state)))

(defn p-render-state []
  (.resetFromState @m @current-state)
  (.renderToFile (. @m propNet) "graph.dot"))

(defn p-get-moves []
  (.getLegalJointMoves @m @current-state))

(defn p-make-move [move]
  (reset! current-state (.getNextState @m @current-state move))
  (p-render-state))


(defn p-goals []
  (.getGoals @m @current-state))

(defn p-is-terminal []
  (.isTerminal @m @current-state))


; vim: lw+=put-through lw+=timed-run :