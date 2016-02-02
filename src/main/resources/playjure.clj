(ns gamer_namespace
  (:require [clojure.tools.nrepl.server :as nrepl]
            [clojure.pprint :as pprint])
  (:import
    [org.ggp.base.util.statemachine StateMachine MachineState]
    [org.ggp.base.player.gamer.statemachine StateMachineGamer]
    [org.ggp.base.util.statemachine.implementation.prover ProverStateMachine]
    [org.ggp.base.util.statemachine.cache CachedStateMachine]
    [com.google.common.cache CacheBuilder]
    [com.google.common.cache CacheLoader]))


; (set! *warn-on-reflection* true)
(defonce nrepl-server
  (when (= "1" (System/getenv "NREPL"))
    (nrepl/start-server :port 7888)))

(def current-gamer (ref nil))

(defrecord Node [role state-machine current-state])

(def solution (atom [-1 []]))

(defn swap-solution [[old-score _ :as old-solution]
                     [new-score _ :as new-solution]]
  (if (< old-score new-score)
    new-solution
    old-solution))

(defn thread-interrupted []
  (.isInterrupted (Thread/currentThread)))


; Guava Cache -----------------------------------------------------------------
(defn fresh-cache []
  (-> (CacheBuilder/newBuilder)
    (.maximumSize 1000000)
    (.build (proxy [CacheLoader] []
              ; TODO: this is ugly, fix it
              (load [k] true)))))

(defmacro when-not-cached [cache state & body]
  `(let [cache# ~cache
         state# ~state]
     (when-not (.getIfPresent cache# state#)
       (.put cache# state# true)
       ~@body)))


; DFS -------------------------------------------------------------------------

(defn is-terminal [{:keys [role state-machine current-state] :as node}]
  (.isTerminal state-machine current-state))

(defn state-value [{:keys [role state-machine current-state] :as node}]
  (.getGoal state-machine current-state role))

(defn get-moves [{:keys [role state-machine current-state] :as node}]
  (.getLegalMoves state-machine current-state role))

(defn make-move [{:keys [role state-machine current-state] :as node} move]
  (.getNextState state-machine current-state [move]))


(defn dfs-full [node path cache depth]
  (when-not-cached
    cache (:current-state node)
    (cond
      (thread-interrupted) nil
      (is-terminal node) (dosync (swap! solution swap-solution
                                        [(state-value node) path]))
      (zero? depth) nil

      :else
      (dorun
        (map (fn [move]
               (dfs-full (assoc node :current-state (make-move node move))
                         (conj path move)
                         cache
                         (dec depth)))
             (get-moves node))))))


; Actual Player ---------------------------------------------------------------

(defn iterative-deepening-dfs [start-node]
  (loop [depth 1]
    (when-not (thread-interrupted)
      (println "Searching depth" depth)
      (dfs-full start-node [] (fresh-cache) depth)
      (let [[score _] @solution]
        (when-not (pos? score)
          (recur (inc depth)))))))

(defn start-game [^StateMachineGamer gamer end-time]
  (dosync (reset! solution [-1 []]))
  (let [start-node (->Node (.getRole gamer)
                           (.getStateMachine gamer)
                           (.getCurrentState gamer))
        worker (future (iterative-deepening-dfs start-node))
        current-time (System/currentTimeMillis)
        wait-ms (int (* 0.9 (- end-time current-time)))]
    (deref worker wait-ms nil)
    (future-cancel worker)))

(defn pick-random-move [gamer]
  (-> (->Node (.getRole gamer)
              (.getStateMachine gamer)
              (.getCurrentState gamer))
    get-moves
    rand-nth))

(defn select-move [gamer timeout]
  (dosync
    (let [[value path] @solution]
      (if (empty? path)
        (pick-random-move gamer)
        (let [[next-move & remaining-moves] path]
          (reset! solution [value remaining-moves])
          next-move)))))

(defn stop-game [gamer])
(defn abort-game [gamer])

(defn Playjure []
  (dosync
    (ref-set current-gamer
             (proxy [StateMachineGamer] []
               (getInitialStateMachine []
                 (CachedStateMachine. (ProverStateMachine.)))

               (stateMachineSelectMove [timeout]
                 (let [move (select-move this timeout)]
                   (println "Performing:" (str move))
                   move))

               (stateMachineMetaGame [timeout]
                 (time (start-game this timeout)))

               (stateMachineAbort []
                 (abort-game this))

               (stateMachineStop []
                 (stop-game this)))))
  @current-gamer)

