(ns playjure.heuristics
  (:require [playjure.utils :refer :all])
  (:import
    [org.ggp.base.util.prover.aima.substituter Substituter]
    [org.ggp.base.util.prover.aima.unifier Unifier]
    [org.ggp.base.util.prover.aima.knowledge KnowledgeBase]
    [org.ggp.base.util.gdl.grammar GdlPool]))


(defn count-moves [gamer role]
  (count (.getLegalMoves (.getStateMachine gamer)
                         (.getCurrentState gamer)
                         role)))


(defn average [& nums]
  (int (/ (apply + nums)
          (count nums))))


; Simple ----------------------------------------------------------------------
(defn static [gamer role]
  1)

(defn inverse-mobility [gamer role]
  (int (/ 50 (count-moves gamer role))))

(defn mobility [gamer role]
  (- 51 (inverse-mobility gamer role)))


(defn mix [& heuristics]
  (fn [& args]
    (apply average
           (map #(apply % args)
                heuristics))))


; Goal Distance ---------------------------------------------------------------
(def goal-expansion-limit 3)

(defn make-knowledge-base [gamer]
  (KnowledgeBase. (into #{} (-> gamer .getMatch .getGame .getRules))))

(defn make-constant [v]
  (cond
    (string? v) (GdlPool/getConstant v)
    (number? v) (GdlPool/getConstant (str v))
    :else v))

(defn make-sentence [head & args]
  (let [tail (list* (map make-constant args))]
    (GdlPool/getRelation head (or tail (list)))))

(defn find-rules [knowledge-base head]
  (.fetch knowledge-base (make-sentence head)))

(defn unify [sentence rule]
  (when-let [substitution (Unifier/unify (.getHead rule) sentence)]
    (Substituter/substitute rule substitution)))


(defn try-goal [goal-sentence rule]
  (when-let [substitution (Unifier/unify (.getHead rule) goal-sentence)]
    (Substituter/substitute rule substitution)))

(defn find-target-goal [all-goals goal-sentence]
  (->> all-goals
    (map (partial try-goal goal-sentence))
    (filter (complement nil?))
    first))


(declare score-body)
(declare score-element)

(defn score-element [gamer state knowledge-base depth el]
  (print-indented "Scoring element: " (str el))
  (let [result
        (cond
          (= GdlPool/TRUE (.getName el))
          (if (contains? (.getContents state) el)
            0
            1)

          (= depth 0)
          (do
            (print-indented "Hit the depth limit!")
            0)

          :else
          (inc-indent
            (let [relevant-rules (find-rules knowledge-base (.getName el))
                  unified-rules (map (partial unify el) relevant-rules)
                  new-goals (filter (complement nil?) unified-rules)
                  goal-scores (map #(score-body gamer
                                                state
                                                knowledge-base
                                                (dec depth)
                                                %)
                                   new-goals)]
              (apply min goal-scores))))]
    (print-indented "Element value: " result)
    result))

(defn score-body [gamer state knowledge-base depth goal]
  (print-indented "Scoring and summing elements in body of: " (str goal))
  ; goal -> ((goal us 100) condition1 condition2 ...)
  (let [result (inc-indent (->> goal
                 .getBody
                 (map (partial score-element gamer state knowledge-base depth))
                 (apply + )))]
    (print-indented "Result: " result)
    result))


(defn score-goal [gamer state knowledge-base goal]
  (print-indented "Scoring goal: " (str goal))
  (print-indented "for state: " (str state))
  (let [result (inc-indent (score-body gamer state knowledge-base
                                       goal-expansion-limit
                                       goal))]
    (print-indented "Result:" result)
    result))


(defn goal-distance [gamer state role]
  (print-indented "\n\nRunning goal-distance")
  (let [knowledge-base (make-knowledge-base gamer)
        our-goal-sentence (make-sentence GdlPool/GOAL (.getName role) 100)
        all-goal-rules (find-rules knowledge-base GdlPool/GOAL)
        target-goal (find-target-goal all-goal-rules our-goal-sentence)]
    (if (not target-goal)
      (static gamer role)
      (let [distance (score-goal gamer state knowledge-base target-goal)]
        (max 0 (- 30 distance))))))

