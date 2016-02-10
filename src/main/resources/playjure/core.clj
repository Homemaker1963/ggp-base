(ns playjure.core
  (:require [playjure.strategies.depth-first-search :as dfs]
            [playjure.strategies.minimax :as minimax])
  (:import
    [org.ggp.base.player.gamer.statemachine StateMachineGamer]
    [org.ggp.base.util.statemachine.implementation.prover ProverStateMachine]
    [org.ggp.base.util.statemachine.cache CachedStateMachine]))


(defmacro if-single-player [gamer then else]
  `(if (= 1 (count (-> ~gamer .getStateMachine .getRoles)))
     ~then
     ~else))


(defn start-game [^StateMachineGamer gamer timeout]
  (println "

                    _===__
                   //-==;=_~
                  ||('   ~)      ___      __ __ __------_
             __----\\|    _-_____////     --__---         -_
            / _----_---_==__   |_|     __=-                \\
           / |  _______     ----_    -=__                  |
           |  \\_/      -----___| |       =-_              _/
           |           \\ \\     \\\\\\\\      __ ---__       _ -
           |            \\ /     ^^^         ---  -------
            \\_         _|-
             \\_________/         A challenger appears!
           _/   -----  -_.
          /_/|  || ||   _/--__
          /  |_      _-       --_
         /     ------            |
        /      __------____/     |
       |      /           /     /
     /      /            |     |
    (     /              |____|
    /\\__/                 |  |
   (  /                  |  /-__
   \\  |                  (______)
    \\\\\\)
           ")
  (if-single-player gamer
    (dfs/start-game gamer timeout)
    (minimax/start-game gamer timeout)))

(defn select-move [^StateMachineGamer gamer timeout]
  (println "\nSelecting a move...")
  (if-single-player gamer
    (dfs/select-move gamer timeout)
    (minimax/select-move gamer timeout)))

(defn stop-game [^StateMachineGamer gamer]
  (System/gc))

(defn abort-game [^StateMachineGamer gamer]
  (System/gc))


(defn make-player []
  (proxy [StateMachineGamer] []
    (getInitialStateMachine []
      (CachedStateMachine. (ProverStateMachine.)))

    (stateMachineSelectMove [timeout]
      (let [move (select-move this timeout)]
        (println "Performing:" (str move))
        move))

    (stateMachineMetaGame [timeout]
      (println "Starting metagame time...")
      (time (start-game this timeout)))

    (stateMachineAbort []
      (abort-game this))

    (stateMachineStop []
      (stop-game this))))

