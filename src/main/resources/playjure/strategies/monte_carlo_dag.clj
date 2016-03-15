(ns playjure.strategies.monte-carlo-dag
  (:require [playjure.utils :refer :all]
            [playjure.interop :as i]
            [clojure.core.match :refer [match]]
            [clojure.pprint :refer [pprint]]
            [slingshot.slingshot :refer [try+ throw+]])
  (:import
    [org.ggp.base.player.gamer.statemachine StateMachineGamer]))


; Configuration ---------------------------------------------------------------
(def exploration-factor
  (if-let [ef (System/getenv "PLAYJURE_MCT_EXPLORE")]
    (Integer/parseInt ef)
    40))


; Global State ----------------------------------------------------------------
(def ^:dynamic *gamer* nil)
(def ^:dynamic *state-machine* nil)
(def ^:dynamic *our-role* nil)
(def ^:dynamic *roles* nil)

; Pretty Printing -------------------------------------------------------------
(defn print-node [node]
  (if (nil? node)
    (println "NIL")
    (pprint (-> node
              (update :state short-str)
              (update :legal-moves (partial mapmap str (partial map str)))
              (update :children (partial mapmap (partial map str) short-str))
              (update :scores (partial mapmap str (partial mapmap str float))) ; good god lemon
              (update :counts (partial mapmap str (partial mapmap str identity)))))))


; Strategy --------------------------------------------------------------------
(defn select-child-random [dag node]
  (->> (:children node)
    vals
    vec
    rand-nth
    (assoc dag)))


(defn- select-child-ucbt-single
  [{:keys [scores counts total-count legal-moves] :as node} role]
  (letfn [(ucbt-value [move]
            (if (zero? (get-in counts [role move] 0))
              infinity
              (+ (get-in scores [role move] 0.0)
                 (* exploration-factor
                    (Math/sqrt (/ (Math/log total-count)
                                  (get-in counts [role move])))))))]
    (let [moves (get legal-moves role)
          chosen-move (first (sort-by ucbt-value > moves))]
      (assert (not (nil? chosen-move)))
      chosen-move)))

(defn select-child-ucbt [{:keys [children] :as node}]
  (mapv (partial select-child-ucbt-single node) *roles*))

(defn select-child [dag node]
  (let [move (select-child-ucbt node)]
    (assert (not (nil? move)))
    move))


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


; The DAG ---------------------------------------------------------------------
(def dag
  "The DAG for Monte-Carlo DAG Search.

  Implemented as a map of state -> node pairs, where a state means the actual
  GGP state objects.

  Wrapped in an atom to facilitate concurrent access.

  "
  (atom {}))


; MCDS Nodes ------------------------------------------------------------------
(defrecord Node
  ; A Node is a single entry in the DAG.
  ;
  ; `state` is the GGP state, and is also used as its key in the DAG map.
  ;
  ; `terminal-results` is the results if this is a terminal node, nil otherwise.
  ;
  ; `scores` and `counts` are maps of the form:
  ;
  ;   {:role1 {:move1 50 :move2 80 ...}
  ;    :role2 {:move1 90 :move2 8 ...}
  ;    ...}
  ;
  ; Not all moves may be listed in the map -- they are only populated as needed.
  ;
  ; `total-count` is the number of times this node has been chosen to be
  ; simulated.
  ;
  ; `legal-moves` is a map of roles to (non-joint) moves (or nil for terminal
  ; states).
  ;
  ; `children` is a map of (joint) moves to child states.  The child states
  ; start out as nil and are only calculated when they are actually selected for
  ; the first time.  `children` can also be a bare nil if this is a terminal
  ; state.
  [state terminal-results scores counts total-count legal-moves children])


(defn- pair-joint-move-roles [joint-move]
  (map vector *roles* joint-move))

(defn- split-joint-moves [joint-moves]
  (->> joint-moves
    (mapcat pair-joint-move-roles)
    (group-by first)
    (mapmap identity #(set (map second %)))))

(defn- get-terminal-results [state]
  (when (i/is-terminal *state-machine* state)
    (i/get-scores *state-machine* state)))

(defn- make-node [state]
  (let [terminal-results (get-terminal-results state)
        joint-moves (when-not terminal-results
                      (i/get-all-joint-moves *state-machine* state))
        legal-moves (when joint-moves (split-joint-moves joint-moves))]
    (->Node
      state
      terminal-results
      {} ; scores
      {} ; counts
      0 ; total-count
      legal-moves ; legal-moves
      (when joint-moves ; children
        (into {}
              (map vector
                   joint-moves
                   (repeat nil)))))))


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


(defn- search-leaf
  "Expand and search this leaf node of the DAG, returning [results new-dag]."
  [dag state]
  (assert (not (contains? dag state)))
  (let [{:keys [terminal-results] :as node} (make-node state)
        results (or terminal-results
                    (i/depth-charge *state-machine* state))]
    [results (assoc dag state node)]))

(defn- patch-child [dag {:keys [state] :as node} move]
  (let [child (get-in node [:children move])]
    (if-not (nil? child)
      [dag child]
      (let [actual-child (i/make-move *state-machine* state move)]
        [(assoc-in dag [state :children move] actual-child)
         actual-child]))))


(defn search-node
  "Search this node of the DAG, returning [results new-dag]."
  [dag {:keys [state] :as node}]

  (assert (contains? dag state))

  ; Check to make sure time hasn't run out.
  (die-when-interrupted)

  ; If this is a terminal node, we can just return the cached results and the
  ; unchanged DAG.
  (if (:terminal-results node)
    [(:terminal-results node) dag]

    ; Otherwise we need to select the next step.
    (let [move (select-child dag node) ; Pick a move to traverse...

          ; Patch it into the child map if necessary...
          [dag child] (patch-child dag node move)

          ; And search it to get the results.
          [results dag] (if (contains? dag child)
                          (inc-indent (search-node dag (get dag child)))
                          (search-leaf dag child))

          new-dag (-> dag
                    (update-in [state :scores] update-scores
                               (get-in dag [state :counts]) move results)
                    (update-in [state :counts] update-counts move)
                    (update-in [state :total-count] inc))]

      (assert (not (nil? child)))
      (assert (contains? new-dag child))
      (assert (= child (get-in new-dag [state :children move])))

      ; Update the DAG on the way back up the call stack.
      [results new-dag])))


(defn search-dag [dag root-node]
  (second (search-node dag root-node)))


; Game ------------------------------------------------------------------------
(defn init-dag! []
  (println "Initializing DAG...")
  (let [state (i/get-current-state *gamer*)]
    (reset! dag {state (make-node state)})))


(defn reachable-from [dag {:keys [state] :as node}]
  ; Take the {move -> child-state/nil} map and pull out all the non-nil children.
  (let [children (->> node
                   :children ; pull out the [move resulting-state] pairs
                   vals ; just get the state values
                   (filter (complement nil?)) ; discard any that are unexpanded (nil)
                   (map (partial get dag)) ; grab those nodes out of the dag
                   set)] ; just dedupe them
    (apply clojure.set/union #{state}
           (map (partial reachable-from dag) children))))

(defn prune-dag [dag]
  (let [root-node (get dag (i/get-current-state *gamer*))]
    (select-keys dag
                 (reachable-from dag root-node))))


(defn select-move [gamer end-time]
  (binding [*gamer* gamer
            *our-role* (.getRole gamer)
            *state-machine* (.getStateMachine gamer)
            *roles* (.getRoles (.getStateMachine gamer))]
    (println "Updating DAG...")
    (if @dag
      (swap! dag prune-dag)
      (init-dag!))

    (println "Searching...")
    (let [starting-dag @dag
          starting-state (i/get-current-state *gamer*)]
      (println "Starting with")
      (println (count starting-dag) "-node DAG")
      (print-node (get starting-dag starting-state))

      (timed-run end-time nil
        (try+
          (loop [current-dag starting-dag]
            (let [root-node (get current-dag starting-state)
                  next-dag (search-dag current-dag root-node)]
              (when-not (thread-interrupted)
                (reset! dag next-dag)
                (recur next-dag))))
          (catch [:type :thread-interrupted] _
            (println "Time ran out as we were searching.")))
        (let [resulting-dag @dag
              root-node (get resulting-dag starting-state)
              move (choose-move root-node)]
          (println "RESULT")
          (println (count resulting-dag) "-node DAG")
          (print-node root-node)
          (println "Choosing move: " (str move))
          move)))))

(defn start-game [^StateMachineGamer gamer end-time]
  (println "Starting game with Monte-Carlo DAG Search")
  (reset! dag nil)
  (select-move gamer end-time)
  nil)


; vim: lw+=timed-run :
