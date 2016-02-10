(ns playjure.utils
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

; Guava Cache -----------------------------------------------------------------
(defn fresh-cache []
  (-> (CacheBuilder/newBuilder)
    (.maximumSize 10000)
    (.build
      (proxy [CacheLoader] []
        (load [k]
          (throw (Exception. "Use .getIfPresent/.put directly.")))))))

