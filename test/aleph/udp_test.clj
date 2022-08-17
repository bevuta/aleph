(ns aleph.udp-test
  (:require
   [aleph.netty :as netty]
   [aleph.udp :as udp]
   [aleph.ResourceLeakDetector]
   [clj-commons.byte-streams :as bs]
   [clojure.test :refer [deftest is use-fixtures]]
   [manifold.stream :as s]))

(netty/leak-detector-level! :paranoid)

(when (aleph.ResourceLeakDetector/enabled?)
  (use-fixtures :each aleph.ResourceLeakDetector/fixture))

(defmacro with-server [server & body]
  `(let [server# ~server]
     (try
       ~@body
       (finally
         (.close ^java.io.Closeable server#)))))

(deftest test-echo
  (let [s @(udp/socket {:port 10001, :epoll? true})]
    (s/put! s {:host "localhost", :port 10001, :message "foo"})
    (is (= "foo"
          (bs/to-string
            (:message
              @(s/take! s)))))
    (s/close! s)))
