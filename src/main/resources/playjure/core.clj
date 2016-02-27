(ns playjure.core
  (:require [playjure.strategies.depth-first-search :as dfs]
            [playjure.strategies.minimax :as minimax]
            [playjure.strategies.monte-carlo :as mc]
            [playjure.strategies.monte-carlo-tree :as mct])
  (:import
    [org.ggp.base.player.gamer.statemachine StateMachineGamer]
    [org.ggp.base.util.statemachine.implementation.prover ProverStateMachine]
    [org.ggp.base.util.statemachine.cache CachedStateMachine]))


(def single-player-strategy {:start dfs/start-game
                             :move dfs/select-move})

(def multiplayer-strategy {:start mct/start-game
                           :move mct/select-move})


(defn select-strategy [gamer]
  (if (= 1 (count (-> gamer .getStateMachine .getRoles)))
    single-player-strategy
    multiplayer-strategy))


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
  ((:start (select-strategy gamer)) gamer timeout))

(defn select-move [^StateMachineGamer gamer timeout]
  (println "\nSelecting a move...")
  ((:move (select-strategy gamer)) gamer timeout))


(defn stop-game [^StateMachineGamer gamer]
  (println "Game stopping, suggesting GC...")
  (System/gc))

(defn abort-game [^StateMachineGamer gamer]
  (println "Game aborted, suggesting GC...")
  (System/gc))


(defn make-player []
  (proxy [StateMachineGamer] []
    (getInitialStateMachine []
      (CachedStateMachine. (ProverStateMachine.)))

    (stateMachineSelectMove [timeout]
      (let [move (select-move this timeout)]
        move))

    (stateMachineMetaGame [timeout]
      (println "Starting metagame time...")
      (time (start-game this timeout)))

    (stateMachineAbort []
      (abort-game this))

    (stateMachineStop []
      (stop-game this))))

