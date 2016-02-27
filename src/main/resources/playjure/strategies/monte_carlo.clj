(ns playjure.strategies.monte-carlo
  (:require [clojure.core.match :refer [match]]
            [clojure.pprint :refer [pprint]])
  (:import
    [org.ggp.base.player.gamer.statemachine StateMachineGamer]))


(def ^:dynamic *gamer* nil)
(def ^:dynamic *state-machine* nil)
(def ^:dynamic *our-role* nil)
(def ^:dynamic *role-indices* nil)

(defn find-move
  "Return the move that ROLE did in the JOINT-MOVE."
  [joint-move role]
  ; GGP-Base's joint-move representation is bad, so we need this util to pull
  ; moves out of it
  (nth joint-move (get *role-indices* role)))

(defn random-joint-move [state]
  (.getRandomJointMove *state-machine* state))

(defn get-current-state []
  (.getCurrentState *gamer*))


(defn update-scores [scores action score]
  (update scores action (fnil (partial + score) 0)))

(defn update-counts [counts action]
  (update counts action (fnil inc 0)))


(defn make-move [state joint-move]
  (.getNextState *state-machine* state joint-move))

(defn depth-charge [state]
  (let [result (.performDepthCharge *state-machine* state
                                    (make-array Integer/TYPE 1))
        score (.getGoal *state-machine* result *our-role*)]
    score))

(defn simulate-move [[scores counts]]
  (let [current-state (get-current-state)
        move (random-joint-move current-state)
        our-move (find-move move *our-role*)
        next-state (make-move current-state move)
        score (depth-charge next-state)]
    [(update-scores scores our-move score)
     (update-counts counts our-move)]))


(defn pick-random-move [^StateMachineGamer gamer]
  (rand-nth (.getLegalMoves (.getStateMachine gamer)
                            (.getCurrentState gamer)
                            (.getRole gamer))))

(defn stringify-map [m]
  (into {} (map (fn [[k v]]
                  [(str k) (float v)])
                m)))

(defn calculate-results [scores counts]
  (into {} (map (fn [[action score]]
                  [action (/ score (get counts action))])
                scores)))

(defn choose-move [results]
  (first (first (sort-by second > results))))


(defn select-move [gamer end-time]
  (binding [*gamer* gamer
            *our-role* (.getRole gamer)
            *state-machine* (.getStateMachine gamer)
            *role-indices* (.getRoleIndices (.getStateMachine gamer))]
    (let [[scores counts] (time (nth (iterate simulate-move [{} {}])
                                     600))
          results (calculate-results scores counts)
          move (choose-move results)]
      (println "SCORES")
      (pprint (stringify-map scores))
      (println "COUNTS")
      (pprint (stringify-map counts))
      (println "RESULTS")
      (pprint (stringify-map results))
      (println "Choosing move: " (str move))
      move)))

(defn start-game [^StateMachineGamer gamer end-time]
  nil)


