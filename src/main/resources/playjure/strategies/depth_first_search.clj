(ns playjure.strategies.depth-first-search
  (:require [com.climate.claypoole :as clay]
            [playjure.utils :refer :all])
  (:import
    [org.ggp.base.player.gamer.statemachine StateMachineGamer]
    [org.ggp.base.util.statemachine.cache CachedStateMachine]
    [com.google.common.cache LoadingCache]))


(def solution (atom nil))

(defrecord DfsNode [role state-machine current-state])

(defn swap-solution [[old-score _ :as old-solution]
                     [new-score _ :as new-solution]]
  (if (< old-score new-score)
    new-solution
    old-solution))


(defn is-terminal [{:keys [role
                           ^CachedStateMachine state-machine
                           current-state]
                    :as node}]
  (.isTerminal state-machine current-state))

(defn state-value
  "Get the value of the current state"
  [{:keys [role
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


(defmacro when-not-cached [^LoadingCache cache state & body]
  `(let [^LoadingCache cache# ~cache
         state# ~state]
     (when-not (.getIfPresent cache# state#)
       (.put cache# state# true)
       ~@body)))


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


(defn start-game [^StateMachineGamer gamer end-time]
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
        (future-cancel-sanely worker)))))

(defn select-move [gamer timeout]
  (dosync
    (let [[value path] @solution]
      (if (empty? path)
        (pick-random-move gamer)
        (let [[next-move & remaining-moves] path]
          (reset! solution [value remaining-moves])
          next-move)))))

