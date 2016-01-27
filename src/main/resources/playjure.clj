(ns gamer_namespace
  (:require [clojure.tools.nrepl.server :as nrepl]
            [clojure.pprint :as pprint])
  (:import [org.ggp.base.player.gamer.statemachine StateMachineGamer]
           [org.ggp.base.util.statemachine.implementation.prover ProverStateMachine]))


(def nrepl-server (ref nil))
(def current-gamer (ref nil))

; NREPL -----------------------------------------------------------------------
(defn start-nrepl []
  (when (= (System/getenv "NREPL") "1")
    (println "Starting NREPL...")
    (dosync
      (when-not @nrepl-server
        (ref-set nrepl-server (nrepl/start-server :port 7888))))))


; DFS -------------------------------------------------------------------------
(declare dfs)

(defn is-terminal [state-machine current-state]
  (.isTerminal state-machine current-state))

(defn state-value [role state-machine current-state]
  (.getGoal state-machine current-state role))

(defn get-moves [role state-machine current-state]
  (.getLegalMoves state-machine current-state role))

(defn make-move [role state-machine current-state move]
  (.getNextState state-machine current-state [move]))

(defn find-best-move [role state-machine current-state]
  (let [moves (get-moves role state-machine current-state)
        results (map (fn [move]
                       (dfs role state-machine
                            (make-move role state-machine
                                       current-state move)))
                     moves)]
    (apply max results)))


(defn dfs [role state-machine current-state]
  (if (is-terminal state-machine current-state) 
    (state-value role state-machine current-state)
    (find-best-move role state-machine current-state)))

(defn run-dfs [role state-machine current-state]
  (let [moves (get-moves role state-machine current-state)
        results (map (fn [move]
                       [(dfs role state-machine
                             (make-move role state-machine
                                        current-state move)) move])
                     moves)]
    (->> results
      (sort (fn [[v1 _] [v2 _]]
              (> v1 v2)))
      first
      second)))


; Actual Player ---------------------------------------------------------------

(defn start-game [gamer timeout]
  (start-nrepl))

(defn select-move [gamer timeout]
  (let [state-machine (.getStateMachine gamer)
        current-state (.getCurrentState gamer)
        role          (.getRole gamer)
        move          (run-dfs role state-machine current-state)]
    move))

(defn stop-game [gamer])
(defn abort-game [gamer])


(defn Playjure []
  (dosync
    (ref-set current-gamer
             (proxy [StateMachineGamer] []
               (getInitialStateMachine []
                 (ProverStateMachine.))

               (stateMachineSelectMove [timeout]
                 (select-move this timeout))

               (stateMachineMetaGame [timeout]
                 (start-game this timeout))

               (stateMachineAbort []
                 (abort-game this))

               (stateMachineStop []
                 (stop-game this)))))
  @current-gamer)

