(ns playjure.strategies.monte-carlo-tree
  (:require [playjure.utils :refer :all]
            [playjure.interop :as i]
            [clojure.core.match :refer [match]]
            [clojure.pprint :refer [pprint]]
            [slingshot.slingshot :refer [try+ throw+]])
  (:import
    [org.ggp.base.player.gamer.statemachine StateMachineGamer]))


; Configuration ---------------------------------------------------------------
(def exploration-factor 20)

; Global State ----------------------------------------------------------------
(def ^:dynamic *gamer* nil)
(def ^:dynamic *state-machine* nil)
(def ^:dynamic *our-role* nil)
(def ^:dynamic *roles* nil)
(def tree (atom nil))

; Pretty Printing -------------------------------------------------------------
(defn mapmap [keyfn valfn m]
  (into {} (map (fn [[k v]] [(keyfn k) (valfn v)]) m)))

(defn print-node [node]
  (if (nil? node)
    (println "NIL")
    (pprint (-> node
              (assoc :children (str "..." (count (:children node)) " children..."))
              (update :state str)
              (update :leading-move (partial map str))
              (update :scores (partial mapmap str (partial mapmap str float))) ; good god lemon
              (update :counts (partial mapmap str (partial mapmap str identity)))))))


; Strategy --------------------------------------------------------------------
(defn select-child-random [node]
  (rand-nth (vec (:children node))))


(defn- select-child-ucbt-single
  [{:keys [scores counts total-count] :as node} role]
  (letfn [(ucbt-value [move]
            (if (zero? (get-in counts [role move]))
              infinity
              (+ (get-in scores [role move])
                 (* exploration-factor
                    (Math/sqrt (/ (Math/log total-count)
                                  (get-in counts [role move])))))))]
    (let [moves (keys (get scores role))]
      (first (sort-by ucbt-value > moves)))))

(defn select-child-ucbt [{:keys [children] :as node}]
  (let [chosen-moves (mapv (partial select-child-ucbt-single node) *roles*)]
    (find-by (partial = chosen-moves)
             :leading-move
             children)))

(defn select-child [node]
  (select-child-ucbt node))


(defn choose-move-score [{:keys [scores] :as node}]
  (->> (get scores *our-role*)
    (sort-by second >)
    first
    first))

(defn choose-move-count [{:keys [counts] :as node}]
  (->> (get counts *our-role*)
    (sort-by second >)
    first
    first))

(defn choose-move [node]
  (choose-move-count node))


; MCTS Nodes ------------------------------------------------------------------
(defrecord Node
  [state terminal-results leading-move scores counts total-count children])


(defn get-terminal-results [state]
  (when (i/is-terminal *state-machine* state)
    (i/get-scores *state-machine* state)))


(defn- make-empty-valmap [state]
  (into {} (for [role *roles*]
             [role
              (into {} (for [move (i/get-legal-moves *state-machine* state role)]
                         [move 0]))])))

(defn- actually-make-node [state leading-move]
  (let [terminal-results (get-terminal-results state)
        empty-value-map (if terminal-results
                          {:terminal true}
                          (make-empty-valmap state))]
    (->Node
      state
      terminal-results
      leading-move
      empty-value-map
      empty-value-map
      0
      nil)))

(defn make-node
  ([state] (actually-make-node state nil))
  ([state move]
   (let [new-state (i/make-move *state-machine* state move)]
     (actually-make-node new-state (vec move)))))

(defn expand-node [{:keys [state terminal-results] :as node}]
  (assoc node :children
         (set (map #(make-node state %)
                   (i/get-all-joint-moves *state-machine* state)))))


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
  (let [{:keys [state] :as expanded-node} (expand-node node)
        results (i/depth-charge *state-machine* state)]
    [results expanded-node]))

(defn search-node
  [{:keys [terminal-results scores counts total-count children] :as node}]
  (die-when-interrupted)
  (cond
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


; Game ------------------------------------------------------------------------
(defn init-tree! []
  (println "Initializing tree...")
  (reset! tree (make-node (i/get-current-state *gamer*))))

(defn update-tree! []
  (println "Updating tree...")
  (let [{:keys [children state]} @tree
        current-state (i/get-current-state *gamer*)]
    (if (= current-state state)
      (println "Skipping tree update, this is (hopefully) the first run...")
      (let [new-root (find-by (partial = current-state)
                              :state
                              children)]
        (if new-root
          (reset! tree new-root)
          (do
            (print-node @tree)
            (println "Trying to find state: " (str current-state))
            (println "Inside the following child states:")
            (dorun (map #(println "    <" (str (:state %)) ">")
                        children))
            (println "Full child nodes:")
            (dorun (map print-node children))
            (throw+ "Couldn't find new root in the tree, something is hosed!")))))))


(defn select-move [gamer end-time]
  (binding [*gamer* gamer
            *our-role* (.getRole gamer)
            *state-machine* (.getStateMachine gamer)
            *roles* (.getRoles (.getStateMachine gamer))]
    (if-not @tree
      (init-tree!)
      (update-tree!))
    (println "Searching...")
    (println "Starting with")
    (let [starting-tree @tree]
      (print-node starting-tree)
      (timed-run end-time nil
        (try+ (->> starting-tree
                (iterate search-tree)
                (map #(when-not (thread-interrupted)
                        (reset! tree %)))
                dorun)
          (catch [:type :thread-interrupted] _
            (println "Time ran out as we were searching.")))
        (let [resulting-tree @tree
              move (choose-move resulting-tree)]
          (println "RESULT")
          (print-node resulting-tree)
          (println "Choosing move: " (str move))
          move)))))

(defn start-game [^StateMachineGamer gamer end-time]
  (println "Starting game with Monte-Carlo Tree Search")
  (reset! tree nil)
  (select-move gamer end-time)
  nil)


; vim: lw+=timed-run :
