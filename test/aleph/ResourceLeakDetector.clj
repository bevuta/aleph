(ns aleph.ResourceLeakDetector
  (:gen-class
   :extends io.netty.util.ResourceLeakDetector)
  (:require
   [clojure.string :as str]
   [clojure.test :as test])
  (:import
   (io.netty.buffer AbstractByteBufAllocator)))

(def +max-gc-runs+
  "Maximum number of times the GC will be run to flush a probe through
  the leak detector (which in turns would flush any other pending
  leaks)."
  10)

(def +probe-hint-marker+
  "ALEPH LEAK DETECTOR PROBE")

(defn probe-hint-line? [line]
  (str/starts-with? line (str "Hint: " +probe-hint-marker+)))

(defn remove-probes [leaks]
  (remove (fn [[_ & record-lines]]
            (some probe-hint-line? record-lines))
          leaks))

;; NOTE: Can't use a static probe hint string because the underlying
;; ResourceLeakDetector somehow will discard leaks with the same one.
(let [cnt (atom 0)]
  (defn probe-hint []
    (str +probe-hint-marker+ " " (swap! cnt inc))))

(defn leak-probe! []
  (-> AbstractByteBufAllocator/DEFAULT
      (.buffer 1)
      (.touch (probe-hint))))

(def current-leaks)

(defn flush! []
  (loop [n +max-gc-runs+]
    (System/gc)
    (System/runFinalization)
    ;; Transitively trigger a track() invocation which in turn works
    ;; off the leaked references queue.
    (-> AbstractByteBufAllocator/DEFAULT (.buffer 1) .release)
    (if (zero? n)
      (throw (RuntimeException. "Gave up awaiting ResourceLeakDetector."))
      (or (seq @current-leaks)
          (recur (dec n))))))

(defn with-leak-collection [f handle-leaks]
  (with-redefs [current-leaks (atom [])]
    (f)
    (leak-probe!)
    (let [leaks (flush!)]
      (handle-leaks (remove-probes leaks)))))

;; TODO: When Netty can't lookup the given class for some reason, it
;; will silently fall back to the default implementation. So this
;; check here is actually not 100% accurate.
(defn enabled? []
  (= "aleph.ResourceLeakDetector"
     (System/getProperty "io.netty.customResourceLeakDetector")))

(defn fixture [run-test]
  (with-leak-collection
    run-test
    (fn [leaks]
      ;; TODO: Making a test fail from within a fixture loses
      ;; context. Find a better way.
      (test/is (empty? leaks)))))

(defn -needReport [_this]
  true)

(defn -reportTracedLeak [_this resource-type records]
  (swap! current-leaks conj (cons resource-type (->> records str/split-lines (map str/trim)))))

(defn -reportUntracedLeak [_this resource-type]
  (swap! current-leaks conj [resource-type]))
