(ns playjure.strategies.monte-carlo-tree
  (:require [playjure.utils :refer :all]
            [clojure.core.match :refer [match]]
            [clojure.pprint :refer [pprint]])
  (:import
    [org.ggp.base.player.gamer.statemachine StateMachineGamer]))


; Global State ----------------------------------------------------------------
(def ^:dynamic *gamer* nil)
(def ^:dynamic *state-machine* nil)
(def ^:dynamic *our-role* nil)
(def ^:dynamic *roles* nil)

; Move-related Java interop ---------------------------------------------------
(defn all-joint-moves [state]
  (.getLegalJointMoves *state-machine* state))

(defn get-current-state []
  (.getCurrentState *gamer*))

(defn make-move [state joint-move]
  (.getNextState *state-machine* state joint-move))

(defn scores [state]
  (let [goal-values (.getGoals *state-machine* state)]
    (into {} (map vector *roles* goal-values))))

(defn is-terminal [state]
  (.isTerminal *state-machine* state))

(defn depth-charge [state]
  (let [result (.performDepthCharge *state-machine* state
                                    (make-array Integer/TYPE 1))]
    (scores result)))


; Strategy --------------------------------------------------------------------
(defn select-child [node]
  (rand-nth (vec (:children node))))

(defn choose-move [{:keys [scores] :as node}]
  (->> (get scores *our-role*)
    (sort-by second >)
    first
    first))


; MCTS Nodes ------------------------------------------------------------------
(defrecord Node
  [state terminal-results leading-move scores counts total-count children])


(defn get-terminal-results [state]
  (when (is-terminal state)
    (scores state)))


(defn make-node
  ([state] (->Node state
                   (get-terminal-results state)
                   nil {} {} 0 nil))
  ([state move] (->Node (make-move state move)
                        (get-terminal-results state)
                        move {} {} 0 nil)))

(defn expand-node [{:keys [state] :as node}]
  (assoc node :children
         (set (map #(make-node state %)
                   (all-joint-moves state)))))


(defn update-counts [counts moves]
  ; {:x {:move1 10 :move2 20},
  ;  :y {:moveA 5 :moveB 25}}
  (loop [[move & moves] moves
         [role & roles] *roles*
         counts counts]
    (if move
      (recur moves roles
             (update-in counts [role move] (fnil inc 0)))
      counts)))

(defn update-scores [scores counts moves results]
  ; {:x {:move1 23 :move2 0},
  ;  :y {:moveA 50 :moveB 100}}
  (loop [[move & moves] moves
         [role & roles] *roles*
         scores scores]
    (if move
      (let [old-average (get-in scores [role move] 0.0)
            simulation-count (get-in counts [role move] 0)
            new-average (/ (+ (get results role)
                              (* old-average simulation-count))
                         (inc simulation-count))]
        (recur moves roles
               (assoc-in scores [role move] new-average)))
      scores)))


(defn search-leaf [node]
  (let [{:keys [state] :as node} (expand-node node)
        results (depth-charge state)]
    [results node]))

(defn search-node
  [{:keys [terminal-results scores counts total-count children] :as node}]
  (cond
    ; If the thread is interrupted, just give up and return nil back.
    (thread-interrupted) nil

    ; If this is a terminal node, we can just return the cached results and this
    ; node itself, unchanged.
    terminal-results [terminal-results node]

    ; If children is nil, we're at a leaf and need to expand (and depth-charge
    ; down).
    (nil? children) (search-leaf node)

    :else ; Otherwise we recurse down and update ourselves on the way back up.
    (let [child (select-child node)
          [result-scores new-child] (search-node child)
          move (:leading-move new-child)]
      [result-scores
       (assoc node
              :scores (update-scores scores counts move result-scores)
              :counts (update-counts counts move)
              :total-count (inc total-count)
              :children (-> children
                          (disj child)
                          (conj new-child)))])))


(defn search-tree [root-node]
  (second (search-node root-node)))


; Pretty Printing -------------------------------------------------------------
(defn mapmap [keyfn valfn m]
  (into {} (map (fn [[k v]] [(keyfn k) (valfn v)]) m)))

(defn print-node [node]
  (pprint (-> node
            (assoc :children "...children...")
            (update :state str)
            (update :leading-move str)
            (update :scores (partial mapmap str (partial mapmap str identity))) ; good god lemon
            (update :counts (partial mapmap str (partial mapmap str identity))))))


; Game ------------------------------------------------------------------------
(defn select-move [gamer end-time]
  (binding [*gamer* gamer
            *our-role* (.getRole gamer)
            *state-machine* (.getStateMachine gamer)
            *roles* (.getRoles (.getStateMachine gamer))]
    (let [result (atom nil)]
      (timed-run end-time nil
        (->> (make-node (get-current-state))
          (iterate search-tree)
          (take-while identity)
          (map #(reset! result %))
          dorun)
        (let [move (choose-move @result)]
          (println "RESULT")
          (print-node @result)
          (println "Choosing move: " (str move))
          move)))))

(defn start-game [^StateMachineGamer gamer end-time]
  nil)


; vim: lw+=timed-run :
