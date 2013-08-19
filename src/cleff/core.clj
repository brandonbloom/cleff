(ns cleff.core
  (:require [cleff.protocols :as proto]
            [cleff.trampoline :as trampoline
             :refer (operation-form transform-form computation-form)]))

;;TODO parameterize with effect type
(deftype Effect [] proto/IEffect)

(defn instance []
  (Effect.))

(defn- handler-fn [bindings-sym env specs]
  (letfn [(impl-map [operations]
            (into {} (for [[name args & body] operations]
                       [(list 'quote name)
                        (operation-form bindings-sym env args body)])))
          (effect-map [specs]
            (let [specs* (->> specs (partition-by seq?) (partition 2))]
              (into {} (for [[[effect] operations] specs*]
                         [effect (impl-map operations)]))))]
    (let [[transforms effects] (split-with seq? specs)
          transforms* (concat '[(value [x] x)] transforms)
          transform-map (into {} (for [[name [arg] & body] transforms*]
                                   [name (transform-form bindings-sym env arg body)]))]
      `(let [map# ~(effect-map effects)]
         (reify proto/IHandler
           (proto/-value [this#]
             ~(transform-map 'value))
           (proto/-finally [this#]
             ~(transform-map 'finally))
           (proto/-operation [this# effect# name#]
             (get-in map# [effect# name#])))))))

(defn run-with [handler computation]
  (trampoline/run handler computation))

(defmacro handler [& specs]
  (let [bindings-sym (gensym "bindings__")]
    `(let [~bindings-sym (clojure.lang.Var/getThreadBindingFrame)]
       ~(handler-fn bindings-sym &env specs))))

(defmacro handle-with [handler & body]
  (let [bindings-sym (gensym "bindings__")]
    `(let [~bindings-sym (clojure.lang.Var/getThreadBindingFrame)
           computation# ~(computation-form bindings-sym &env body)]
       (run-with ~handler computation#))))

(defmacro handle [specs & body]
  (let [bindings-sym (gensym "bindings__")]
    `(let [~bindings-sym (clojure.lang.Var/getThreadBindingFrame)
           handler# ~(handler-fn bindings-sym &env specs)
           computation# ~(computation-form bindings-sym &env body)]
       (run-with handler# computation#))))


;;; Test Code

(comment

  (require 'clojure.pprint)

  (defn ppc [form]
    (clojure.pprint/write form :dispatch clojure.pprint/code-dispatch))

  (defn ppme [form]
    (-> form macroexpand ppc))

  (defn choice []
    (instance))

  (def c (choice))

  (ppme '(handler))

  (ppme '(handler (finally [x] x)))

  (ppme '(handler c (decide [] (continue true))))

  (ppme '(handle [] :foo))

  (ppme '(handle [(value [x] [x x])]
           :foo))

)
