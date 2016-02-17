(ns playjure.utils
  (:require
    [slingshot.slingshot :refer [try+ throw+]])
  (:import
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


