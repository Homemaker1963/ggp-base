(ns playjure.propnet
  (:require [playjure.interop :as i])
  (:import
    [org.ggp.base.util.game GameRepository LocalGameRepository]
    [org.ggp.base.util.match Match]
    [org.ggp.base.util.statemachine.implementation.prover ProverStateMachine]
    [org.ggp.base.util.statemachine.implementation.propnet PlayjurePropnetStateMachine]))

(def counter (atom 0))
(def seen (atom nil))

(defn list-games []
  (let [repo (GameRepository/getDefaultRepository)]
    (dorun (map println (sort (.getGameKeys repo))))))

(defn get-game-gdl [game-name]
  (let [repo (GameRepository/getDefaultRepository)
        game (.getGame repo game-name)
        match (new Match "" -1 1000 1000 game "")]
    (-> match .getGame .getRules)))


(defn get-initial-state [prover propnet]
  [(.getInitialState prover)
   (.getInitialState propnet)])

(defn assert-states-match [prover propnet [prover-state propnet-state]]
  (assert (= (.isTerminal prover prover-state)
             (.isTerminal propnet propnet-state)))
  (if (.isTerminal prover prover-state)
    (assert (= (.getGoals prover prover-state)
               (.getGoals propnet propnet-state)))
    (assert (= (set (.getLegalJointMoves prover prover-state))
               (set (.getLegalJointMoves propnet propnet-state)))))
  :success)

(defn dfs [prover propnet [prover-state propnet-state :as state] depth]
  (if (contains? @seen prover-state)
    depth
    (do
      (assert-states-match prover propnet state)
      (swap! counter inc)
      (swap! seen #(conj % prover-state))
      (when (zero? (rem @counter 1000))
        (println "Checked" @counter "states"))
      (if-not (.isTerminal prover prover-state)
        (let [moves (.getLegalJointMoves prover prover-state)]
          (apply max (map (fn [move]
                            (dfs prover propnet
                                 [(.getNextState prover prover-state move)
                                  (.getNextState propnet propnet-state move)]
                                 (inc depth)))
                          moves)))
        depth))))


(defmacro with-reasoners [prover propnet game-name & body]
  `(let [~prover (new ProverStateMachine)
         ~propnet (new PlayjurePropnetStateMachine)
         game-desc# (get-game-gdl ~game-name)]
     (.initialize ~prover game-desc#)
     (.initialize ~propnet game-desc#)
     ~@body))

(defmacro timed [& body]
  `(let [start# (System/currentTimeMillis)]
     ~@body
     (- (System/currentTimeMillis) start#)))


(defn exercise-reasoner [reasoner n]
  (let [initial-state (.getInitialState reasoner)]
    (dotimes [_ n]
      (i/depth-charge reasoner initial-state))))

(defn benchmark-game [game-name n]
  (System/gc)
  (Thread/sleep 2000)
  (println "Benchmarking" game-name "...")
  (with-reasoners prover propnet game-name
    (let [_ (println "    Benchmarking Prover ...")
          prover-time (timed (exercise-reasoner prover n))
          _ (System/gc)
          _ (Thread/sleep 2000)
          _ (println "    Benchmarking Propnet ...")
          propnet-time (timed (exercise-reasoner propnet n))]
      (println "Ran" n "depth charges each.")
      (println "     Prover took:"
               (float (/ prover-time n))
               "ms per charge.")
      (println "    Propnet took:"
               (float (/ propnet-time n))
               "ms per charge.")))
  (println))


(defn run-test [game-name]
  (reset! counter 0)
  (reset! seen #{})
  (with-reasoners prover propnet game-name
    (dfs prover propnet (get-initial-state prover propnet) 0)))

