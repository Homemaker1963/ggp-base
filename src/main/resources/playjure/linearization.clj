(ns playjure.linearization)

; Data Structure --------------------------------------------------------------
;
; Turn a GDL state thing into a linear state.  A linear state simulates
; a turn-taking game despite the fact that GDL games are always simultaneous.
(defrecord LinearState
  ; The underlying GGP state machine and current state are stored so they can be
  ; updated once all players have taken their turns.
  [state-machine
   current-state

   ; role-ordering is a vector storing the (fake, arbitrary) order that the
   ; roles take their turns in.  This never changes for the life of the game.
   role-ordering

   ; role-info is a map of roles to extra arbitrary data that can be accessed
   ; later if needed.
   role-info

   ; role-queue is a list of roles that still need to take a turn before the
   ; underlying real GGP turn can proceed.
   role-queue

   ; actions is a map of roles to actions they've taken so far.  It is used when
   ; role-queue is empty and it's time to actually generate the next GGP state,
   ; after which it is reset.
   actions])


(defn make-linear-game
  ([state-machine starting-state]
   (make-linear-game state-machine
                     starting-state
                     (.getRoles state-machine)))
  ([state-machine starting-state role-ordering]
   (make-linear-game state-machine
                     starting-state
                     role-ordering
                     {}))
  ([state-machine starting-state role-ordering role-info]
   (->LinearState state-machine
                  starting-state
                  role-ordering
                  role-info
                  (apply list role-ordering)
                  {})))


; Roles -----------------------------------------------------------------------
(defn get-role-info [state role]
  ((:role-info state) role))

(defn get-current-role [state]
  (first (:role-queue state)))

(defn get-current-role-info [state]
  (get-role-info state (get-current-role state)))


; Moves -----------------------------------------------------------------------
(defn ^:private create-joint-move [{:keys [actions state-machine] :as state}]
  (map actions (.getRoles state-machine)))


(defn get-moves [{:keys [state-machine current-state]
                  :as state}]
  (.getLegalMoves state-machine current-state (get-current-role state)))


(defn ^:private actually-perform-move [state]
  (let [new-state (.getNextState state-machine current-state
                                 (create-joint-move state))]
    (assoc state
           :current-state new-state
           :actions {}
           :role-queue (apply list role-ordering))))

(defn perform-move [state move]
  (let [current-role (get-current-role state)
        state (-> state
                (assoc-in [:actions current-role] move)
                (update :role-queue rest))]
    (if (empty? (:role-queue state))
      (actually-perform-move state)
      state)))


; Game State ------------------------------------------------------------------
(defn is-terminal [{:keys [^CachedStateMachine state-machine
                           current-state]
                    :as state}]
  (.isTerminal state-machine current-state))

(defn state-value
  ([state]
   (state-value (get-current-role state)))
  ([{:keys [state-machine current-state] :as state} role]
   (.getGoal state-machine current-state role)))
