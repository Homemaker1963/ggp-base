(ns gamer_namespace
  (:require [clojure.tools.nrepl.server :as nrepl]
            [clojure.pprint :as pprint])
  (:import 
    [org.ggp.base.util.statemachine StateMachine MachineState]
    [org.ggp.base.player.gamer.statemachine StateMachineGamer]
    [org.ggp.base.util.statemachine.implementation.prover ProverStateMachine]
    [org.ggp.base.util.statemachine.cache CachedStateMachine]))


; (set! *warn-on-reflection* true)
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

(defn is-terminal [^CachedStateMachine state-machine current-state]
  (.isTerminal state-machine current-state))

(defn state-value [role ^CachedStateMachine state-machine current-state]
  (.getGoal state-machine current-state role))

(defn get-moves [role ^CachedStateMachine state-machine current-state]
  (.getLegalMoves state-machine current-state role))

(defn make-move [role ^CachedStateMachine state-machine current-state move]
  (.getNextState state-machine current-state [move]))


(defn dfs-full [role ^CachedStateMachine state-machine current-state depth]
  (cond
    (is-terminal state-machine current-state) 
    [(state-value role state-machine current-state) (list)]

    (zero? depth)
    [-1 (list)]

    :else
    (let [moves (get-moves role state-machine current-state)
          next-depth (dec depth)]
      (loop [best -1
             best-path nil
             move (first moves)
             moves (rest moves)]
        (let [next-state (make-move role state-machine current-state move)
              [score path] (dfs-full role state-machine next-state next-depth)
              [new-best new-path :as prev] (if (> best score)
                                             [score path]
                                             [best best-path])]
          (cond
            (= score 100) [100 (cons move path)]
            (empty? moves) [new-best (cons move path)]
            :else (recur new-best
                         new-path
                         (first moves)
                         (rest moves))))))))


; Actual Player ---------------------------------------------------------------
(def solution (ref nil))

(defn start-game [^StateMachineGamer gamer timeout]
  (let [state-machine (.getStateMachine gamer)
        current-state (.getCurrentState gamer)
        role          (.getRole gamer)
        path          (loop [depth 1]
                        (let [[score path] (dfs-full role state-machine current-state depth)]
                          (if (pos? score)
                            path
                            (recur (inc depth)))))]
    (dosync (ref-set solution path))))

(defn select-move [gamer timeout]
  (dosync
    (let [next-move (first @solution)]
      (alter solution rest)
      next-move)))

(defn stop-game [gamer])
(defn abort-game [gamer])

(defn Playjure []
  (dosync
    (ref-set current-gamer
             (proxy [StateMachineGamer] []
               (getInitialStateMachine []
                 (CachedStateMachine. (ProverStateMachine.)))

               (stateMachineSelectMove [timeout]
                 (select-move this timeout))

               (stateMachineMetaGame [timeout]
                 (time (start-game this timeout)))

               (stateMachineAbort []
                 (abort-game this))

               (stateMachineStop []
                 (stop-game this)))))
  @current-gamer)

