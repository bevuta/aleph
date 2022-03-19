(ns aleph.clj-kondo-hooks
   (:require [clj-kondo.hooks-api :as hooks]))

(defmacro def-http-method [method]
  `(defn ~method
     ([url#])
     ([url# options#])))

(defmacro def-derived-map [name params & {:as m}]
  `(do (defrecord ~name ~params)
       (fn ~params
         (hash-map ~@m))))

(defn channel-inbound-handler [{:keys [node]}]
  (let [valid-keys #{:channel-active
                     :channel-inactive
                     :channel-read
                     :channel-read-complete
                     :channel-registered
                     :channel-unregistered
                     :channel-writability-changed
                     :exception-caught
                     :handler-added
                     :handler-removed
                     :user-event-triggered}
        handlers (partition-all 2 (rest (:children node)))]
    (doseq [[handler-key] handlers]
      (let [handler-key-sexpr (hooks/sexpr handler-key)]
        (when-not (contains? valid-keys handler-key-sexpr)
          (hooks/reg-finding! (assoc (meta handler-key)
                                     :type :aleph.netty/invalid-handler-key
                                     :message (str "invalid handler key " handler-key-sexpr))))))
    (let [handler-bindings (map (fn [[handler-key handler-fnspec]]
                                  [(hooks/token-node (symbol (hooks/sexpr handler-key)))
                                   handler-fnspec])
                                handlers)
          new-node (hooks/list-node
                    (list
                     (hooks/token-node 'letfn)
                     (hooks/vector-node
                      (map (fn [[handler-sym handler-fnspec]]
                             (update handler-fnspec :children #(cons handler-sym %)))
                           handler-bindings))
                     (hooks/vector-node
                      (map first handler-bindings))))]
      {:node new-node})))
