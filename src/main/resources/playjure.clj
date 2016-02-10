(ns gamer_namespace
  (:require [clojure.tools.nrepl.server :as nrepl]
            [playjure.core])
  (:import
    [org.ggp.base.util.statemachine StateMachine MachineState]
    [org.ggp.base.player.gamer.statemachine StateMachineGamer]
    [org.ggp.base.util.statemachine.implementation.prover ProverStateMachine]
    [org.ggp.base.util.statemachine.cache CachedStateMachine]
    [com.google.common.cache CacheBuilder]
    [com.google.common.cache LoadingCache]
    [com.google.common.cache CacheLoader]))

(set! *warn-on-reflection* true)

(defonce nrepl-server
  (when (= "1" (System/getenv "NREPL"))
    (nrepl/start-server :port 7888)))

(defn Playjure [] (playjure.core/make-player))

