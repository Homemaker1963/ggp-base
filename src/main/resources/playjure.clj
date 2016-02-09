(ns gamer_namespace
  (:require [clojure.tools.nrepl.server :as nrepl]
            [clojure.pprint :as pprint]
            [com.climate.claypoole :as clay])
  (:import
    [org.ggp.base.util.statemachine StateMachine MachineState]
    [org.ggp.base.player.gamer.statemachine StateMachineGamer]
    [org.ggp.base.util.statemachine.implementation.prover ProverStateMachine]
    [org.ggp.base.util.statemachine.cache CachedStateMachine]
    [com.google.common.cache CacheBuilder]
    [com.google.common.cache LoadingCache]
    [com.google.common.cache CacheLoader]))


(set! *warn-on-reflection* true)

(def solution (atom nil))

(defn swap-solution [[old-score _ :as old-solution]
                     [new-score _ :as new-solution]]
  (if (< old-score new-score)
    new-solution
    old-solution))


; NREPL -----------------------------------------------------------------------
(defonce nrepl-server
  (when (= "1" (System/getenv "NREPL"))
    (nrepl/start-server :port 7888)))


; Java and GGP-Base Interop ---------------------------------------------------
(defn thread-interrupted []
  (.isInterrupted (Thread/currentThread)))


(defn is-terminal [{:keys [role
                           ^CachedStateMachine state-machine
                           current-state]
                    :as node}]
  (.isTerminal state-machine current-state))

(defn state-value [{:keys [role
                           ^CachedStateMachine state-machine
                           current-state]
                    :as node}]
  (.getGoal state-machine current-state role))

(defn get-moves [{:keys [role
                         ^CachedStateMachine state-machine
                         current-state]
                  :as node}]
  (.getLegalMoves state-machine current-state role))

(defn get-roles [{:keys [^CachedStateMachine state-machine]
                  :as node}]
  (.getRoles state-machine))

(defn make-move [{:keys [role
                         ^CachedStateMachine state-machine
                         current-state]
                  :as node} move]
  (.getNextState state-machine current-state [move]))

(defn pick-random-move [^StateMachineGamer gamer]
  (rand-nth (.getLegalMoves (.getStateMachine gamer)
                            (.getCurrentState gamer)
                            (.getRole gamer))))


; Guava Cache -----------------------------------------------------------------
(defn fresh-cache []
  (-> (CacheBuilder/newBuilder)
    (.maximumSize 10000)
    (.build
      (proxy [CacheLoader] []
        (load [k]
          (throw (Exception. "Use .getIfPresent/.put directly.")))))))

(defmacro when-not-cached [^LoadingCache cache state & body]
  `(let [^LoadingCache cache# ~cache
         state# ~state]
     (when-not (.getIfPresent cache# state#)
       (.put cache# state# true)
       ~@body)))


; Single Player ---------------------------------------------------------------
;
; Single-player games use iterative deepening DFS to try to find a solution
; during the "metagaming" warm-up period.

(def check-interval 200)
(def response-cutoff 1500)

(defrecord DfsNode [role state-machine current-state])

(defn dfs-full [node path cache depth threadpool]
  (when-not-cached
    cache (:current-state node)
    (cond
      (thread-interrupted) nil
      (is-terminal node) (dosync (swap! solution swap-solution
                                        [(state-value node) path]))
      (zero? depth) nil

      :else
      (dorun
        ((if threadpool (partial clay/upmap threadpool) map)
         (fn [move]
           (dfs-full (assoc node :current-state (make-move node move))
                     (conj path move)
                     cache
                     (dec depth)
                     nil))
         (get-moves node))))))

(defn iterative-deepening-dfs [start-node threadpool]
  (loop [depth 1]
    (when-not (thread-interrupted)
      (println "Searching depth" depth)
      (dfs-full start-node [] (fresh-cache) depth threadpool)
      (let [[score _] @solution]
        (when-not (= 100 score)
          (recur (inc depth))))))
  true)


(defn single-player-start-game [^StateMachineGamer gamer end-time]
  (dosync (reset! solution [-1 []]))
  (letfn [(time-left [end-time]
            (- end-time (System/currentTimeMillis)))
          (done-searching []
            (let [[score _] @solution]
              (= score 100)))]
    (clay/with-shutdown! [threadpool (clay/threadpool (clay/ncpus))]
      (let [start-node (->DfsNode (.getRole gamer)
                                  (.getStateMachine gamer)
                                  (.getCurrentState gamer))
            worker (future (iterative-deepening-dfs start-node threadpool))]
        (loop []
          (when (and (> (time-left end-time) response-cutoff)
                     (not (done-searching))
                     (nil? (deref worker check-interval nil)))
            (recur)))
        (future-cancel worker)))))

(defn single-player-select-move [gamer timeout]
  (dosync
    (let [[value path] @solution]
      (if (empty? path)
        (pick-random-move gamer)
        (let [[next-move & remaining-moves] path]
          (reset! solution [value remaining-moves])
          next-move)))))


; GGP Player Implementation ---------------------------------------------------

(defmacro if-single-player [gamer then else]
  `(if (= 1 (count (-> ~gamer .getStateMachine .getRoles)))
     ~then
     ~else))


(defn start-game [^StateMachineGamer gamer timeout]
  (if-single-player gamer
    (single-player-start-game gamer timeout)
    nil))

(defn select-move [^StateMachineGamer gamer timeout]
  (if-single-player gamer
    (single-player-select-move gamer timeout)
    nil))

(defn stop-game [^StateMachineGamer gamer]
  (System/gc))

(defn abort-game [^StateMachineGamer gamer]
  (System/gc))


(defn Playjure []
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


; vim: lispwords+=if-single-player,when-not-cached
