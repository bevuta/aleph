(ns aleph.ResourceLeakDetector
  (:gen-class
   :extends io.netty.util.ResourceLeakDetector))

(def current-leaks)

(defn with-leak-collection [f handle-leaks]
  (with-redefs [current-leaks (atom [])]
    (try
      (f)
      (finally
        (handle-leaks @current-leaks)))))

(defn -needReport [_this]
  true)

(defn -reportTracedLeak [_this resource-type records]
  (swap! current-leaks conj [resource-type records]))

(defn -reportUntracedLeak [_this resource-type]
  (swap! current-leaks conj [resource-type]))
