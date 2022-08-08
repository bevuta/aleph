(def netty-version "4.1.74.Final")

(def netty-modules
  '[transport
    transport-native-epoll
    codec
    codec-http
    handler
    handler-proxy
    resolver
    resolver-dns])

(def other-dependencies
  '[[org.clojure/tools.logging "1.1.0" :exclusions [org.clojure/clojure]]
    [org.clj-commons/dirigiste "1.0.1"]
    [manifold "0.2.4"]
    [org.clj-commons/byte-streams "0.3.1"]
    [org.clj-commons/primitive-math "1.0.0"]
    [potemkin "0.4.5"]])

(defproject aleph (or (System/getenv "PROJECT_VERSION") "0.5.0")
  :description "A framework for asynchronous communication"
  :repositories {"jboss" "https://repository.jboss.org/nexus/content/groups/public/"
                 "sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"}
  :url "https://github.com/clj-commons/aleph"
  :license {:name "MIT License"}
  :dependencies ~(concat
                   other-dependencies
                   (map
                     #(vector (symbol "io.netty" (str "netty-" %)) netty-version)
                     netty-modules))
  :profiles {:dev  {:dependencies [[org.clojure/clojure "1.10.3"]
                                   [criterium "0.4.6"]
                                   [cheshire "5.10.0"]
                                   [org.slf4j/slf4j-simple "1.7.30"]
                                   [com.cognitect/transit-clj "1.0.324"]
                                   [spootnik/signal "0.2.4"]
                                   [me.mourjo/dynamic-redef "0.1.0"]]}
             ;; This is for self-generating certs for testing ONLY:
             :test {:dependencies [[org.bouncycastle/bcprov-jdk15on "1.69"]
                                   [org.bouncycastle/bcpkix-jdk15on "1.69"]]
                    :jvm-opts ["-Dorg.slf4j.simpleLogger.defaultLogLevel=off"]}
             :eager-leak-detection {:aot [aleph.ResourceLeakDetector]
                                    :jvm-opts ["-Dio.netty.customResourceLeakDetector=aleph.ResourceLeakDetector"]}}
  :codox {:src-dir-uri "https://github.com/ztellman/aleph/tree/master/"
          :src-linenum-anchor-prefix "L"
          :defaults {:doc/format :markdown}
          :include [aleph.tcp
                    aleph.udp
                    aleph.http
                    aleph.flow]
          :output-dir "doc"}
  :plugins [[lein-codox "0.10.7"]
            [lein-jammin "0.1.1"]
            [lein-marginalia "0.9.1"]
            [ztellman/lein-cljfmt "0.1.10"]]
  :java-source-paths ["src/aleph/utils"]
  :cljfmt {:indents {#".*" [[:inner 0]]}}
  :test-selectors {:default #(not
                               (some #{:benchmark :stress}
                                 (cons (:tag %) (keys %))))
                   :benchmark :benchmark
                   :stress :stress
                   :leak :leak
                   :all (constantly true)}
  :jvm-opts ^:replace ["-server"
                       "-Xmx2g"
                       "-XX:+HeapDumpOnOutOfMemoryError"
                       "-Dio.netty.allocator.type=unpooled"
                       "-Dio.netty.leakDetection.level=paranoid"
                       #_"-XX:+PrintCompilation"
                       #_"-XX:+UnlockDiagnosticVMOptions"
                       #_"-XX:+PrintInlining"]
  :javac-options ["-target" "1.8" "-source" "1.8"]
  :global-vars {*warn-on-reflection* true})
