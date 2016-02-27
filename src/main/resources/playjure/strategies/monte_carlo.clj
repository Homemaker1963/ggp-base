(ns playjure.strategies.monte-carlo
  (:require [clojure.core.match :refer [match]])
  (:import
    [org.ggp.base.player.gamer.statemachine StateMachineGamer]))


(defn pick-random-move [^StateMachineGamer gamer]
  (rand-nth (.getLegalMoves (.getStateMachine gamer)
                            (.getCurrentState gamer)
                            (.getRole gamer))))

(defn select-move [gamer end-time]
  (pick-random-move gamer))


(defn start-game [^StateMachineGamer gamer end-time]
  nil)


