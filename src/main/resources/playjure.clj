(ns gamer_namespace
  (:require [clojure.tools.nrepl.server :as nrepl]
            [playjure.core]))

(set! *warn-on-reflection* false)

(defonce nrepl-server
  (when (= "1" (System/getenv "NREPL"))
    (nrepl/start-server :port 7888)))

(defn Playjure [] (playjure.core/make-player))
(defn Playjure2 [] (playjure.core/make-player))

