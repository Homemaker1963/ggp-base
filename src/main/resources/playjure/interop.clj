(ns playjure.interop
  
  )


(defn get-current-state [gamer]
  (.getCurrentState gamer))

(defn get-all-joint-moves [state-machine state]
  (.getLegalJointMoves state-machine state))

(defn get-legal-moves [state-machine state role]
  (.getLegalMoves state-machine state role))

(defn get-goal-values [state-machine state]
  (.getGoals state-machine state))

(defn get-roles [state-machine]
  (.getRoles state-machine))

(defn is-terminal [state-machine state]
  (.isTerminal state-machine state))


(defn get-scores [state-machine state]
  (into {} (map vector
                (get-roles state-machine)
                (get-goal-values state-machine state))))

(defn depth-charge [state-machine state]
  (let [result (.performDepthCharge state-machine state
                                    (make-array Integer/TYPE 1))]
    (get-scores state-machine result)))


(defn make-move [state-machine state joint-move]
  (.getNextState state-machine state joint-move))
