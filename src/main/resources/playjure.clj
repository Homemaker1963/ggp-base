(ns gamer_namespace
  (:import [org.ggp.base.player.gamer.statemachine StateMachineGamer]
           [org.ggp.base.util.statemachine.implementation.prover ProverStateMachine]))

(defn Playjure []
  (proxy [StateMachineGamer] []
    (getInitialStateMachine []
      (ProverStateMachine.))

    (stateMachineSelectMove [timeout]
      (let [state-machine (.getStateMachine this)
            current-state (.getCurrentState this)
            role          (.getRole this)
            random-move   (.getRandomMove state-machine
                                          current-state
                                          role)]
        random-move))

    (stateMachineMetaGame [timeout]
      (println "Playjure metagame called"))

    (stateMachineAbort []
      (println "Playjure abort called"))

    (stateMachineStop []
      (println "Playjure stop called"))))
