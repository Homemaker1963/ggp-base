(ns playjure.utils
  (:require
    [slingshot.slingshot :refer [try+ throw+]]
    [clojure.pprint :refer [pprint]])
  (:import
    [com.google.common.cache LoadingCache]
    [com.google.common.cache CacheBuilder]
    [com.google.common.cache CacheLoader]))


(def check-interval 200)
(def response-cutoff 1500)

(defn future-cancel-sanely [f]
  (if (future-done? f)
    ; If the future was already done, it might have silently thrown an
    ; exception.  We can deref it here to rethrow the exception in the current
    ; thread so we can see what went wrong.  Clojure is a joke.
    @f
    ; Otherwise just cancel it.
    (future-cancel f)))


(defn thread-interrupted []
  (.isInterrupted (Thread/currentThread)))

(defn die-when-interrupted []
  (when (thread-interrupted)
    (throw+ {:type :thread-interrupted})))


(defn fresh-cache []
  (-> (CacheBuilder/newBuilder)
    (.maximumSize 200000)
    (.build
      (proxy [CacheLoader] []
        (load [k]
          (throw (Exception. "Use .getIfPresent/.put directly.")))))))


(def infinity Long/MAX_VALUE)
(def -infinity Long/MIN_VALUE)

(defn third [coll]
  (nth coll 2))



(def ^:dynamic *indent* 0)
(def shut-up true)

(defmacro inc-indent [& body]
  `(binding [*indent* (inc *indent*)]
     ~@body))

(defn stringify [v]
  ; oh my god clojure
  (binding [*print-readably* nil] (pr-str v)))

(defn print-indented [& args]
  (when-not shut-up
    (let [indent (apply str (repeat *indent* "    "))
          content (apply str indent (map stringify args))]
      (println content))))


(defmacro with-result [name expr & body]
  `(let [~name ~expr]
     ~@body
     ~name))

(defn put-through [^LoadingCache cache state result]
  (.put cache state result)
  result)

(defn safe-inc [n]
  (cond
    (nil? n) nil
    (= infinity n) infinity
    :else (inc n)))


(defmacro log [form]
  `(do
     (pprint (quote ~form))
     (pprint ~form)
     (println)))


(defn time-left [end-time]
  (- end-time (System/currentTimeMillis)))

(defn wait-til-done [end-time tripwire]
  (while (and (> (time-left end-time) response-cutoff)
              (not (tripwire)))
    (Thread/sleep check-interval)))


(defmacro timed-run
  "Handle the ugly guts of running for 'as long as needed/necessary'.

  end-time
  The timestamp that we absolutely must end by.

  tripwire-form
  A single form that, when true, will signal that we should stop running (even
  if we still have time left).  Just give `nil` to use all available time.

  worker-form
  A single form that will be executed inside a future.  The future will be
  (sanely) cancelled when time is up (or the tripwire fires).

  body
  Any number of forms that will be executed once the time is up.  The worker
  will have been canceled before this.

  "
  [end-time tripwire-form worker-form & body]
  `(let [worker# (future ~worker-form)
         end-time# ~end-time]
     (wait-til-done end-time# (fn [] ~tripwire-form))
     (future-cancel-sanely worker#)
     ~@body))
