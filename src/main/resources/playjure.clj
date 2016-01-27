(ns gamer_namespace
  (:require [clojure.tools.nrepl.server :as nrepl])
  (:import [org.ggp.base.player.gamer.statemachine StateMachineGamer]
           [org.ggp.base.util.statemachine.implementation.prover ProverStateMachine]))

(def nrepl-server (ref nil))
(def current-gamer (ref nil))

(defn start-nrepl []
  (when (= (System/getenv "NREPL") "1")
    (println "Starting NREPL...")
    (dosync
      (when-not @nrepl-server
        (ref-set nrepl-server (nrepl/start-server :port 7888))))))


(defn select-move [gamer timeout]
  (let [state-machine (.getStateMachine gamer)
        current-state (.getCurrentState gamer)
        role          (.getRole gamer)
        random-move   (.getRandomMove state-machine
                                      current-state
                                      role)]
    random-move))


(defn Playjure []
  (dosync
    (ref-set
      current-gamer
      (proxy [StateMachineGamer] []
        (getInitialStateMachine []
          (ProverStateMachine.))

        (stateMachineSelectMove [timeout]
          (select-move this timeout))

        (stateMachineMetaGame [timeout]
          (start-nrepl)
          (println "Playjure metagame called"))

        (stateMachineAbort []
          (println "Playjure abort called"))

        (stateMachineStop []
          (println "Playjure stop called")))))
  @current-gamer)

