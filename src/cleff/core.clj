(ns cleff.core
  (:require [clojure.core.async.impl.ioc-macros :as ioc])
  (:import [java.util.concurrent.atomic AtomicReferenceArray]))


;;; Utilities

(defn clone-state [^AtomicReferenceArray state]
  (let [n (.length state)
        clone (AtomicReferenceArray. n)]
    (doseq [i (range n)]
      (ioc/aset-object clone i (ioc/aget-object state i)))
    clone))


;;; Terminator implementations

(def handler-idx ioc/USER-START-IDX)
(def continuation-idx ioc/USER-START-IDX)

(defn handler-message []
  )

(defn handler-continue []
  )

(defn handler-return []
  )

(defn computation-effect []
  )

(defn computation-value []
  )

(defn computation-return []
  )


;;; Coroutine state machines

(def handler-terminators
  {'message `handler-message
   'continue `handler-continue
   :Return `handler-return})

(def computation-terminators
  {'effect `computation-effect
   'value `computation-value
   :Return `computation-return})

(defn handler-form [operations]
  (let [args-sym (gensym "args__")]
    `(let [[operation# ~args-sym] (~'message)]
       (case operation#
         ~@(concat (for [[operation arglist body] operations]
                     `[operation (let [~arglist ~args-sym]
                                   ~@body)]))
         ;;TODO forward operation up the handler stack
         (throw (Exception. (str "Unexpected operation " operation#)))))))

(defn handler-fn [bindings-sym env operations]
  `(let [state# (~(ioc/state-machine (list (handler-form operations))
                                     0 env handler-terminators))]
     (ioc/aset-object state# ioc/BINDINGS-IDX ~bindings-sym)
     (ioc/run-state-machine state#)
     state#))

(defn computation-fn [bindings-sym env body]
  `(let [state# (~(ioc/state-machine body 0 env computation-terminators))]
     (ioc/aset-object state# ioc/BINDINGS-IDX ~bindings-sym)))


;;; Interpretation trampoline

(defn run [handler computation]
  (loop [stack (list computation)]
    ))

(defn handle-fn [env operations computation-body]
  (let [bindings-sym (gensym "bindings__")]
    `(let [~bindings-sym (clojure.lang.Var/getThreadBindingFrame)
           handler# ~(handler-fn bindings-sym env operations)
           computation# ~(computation-fn bindings-sym env computation-body)]
       (run handler# computation#))))


;;; User Syntax

(defmacro handle [operations & body]
  (handle-fn &env operations body))


;;; Test Code

(comment

  (require 'clojure.pprint)

  (defn ppc [form]
    (clojure.pprint/write form :dispatch clojure.pprint/code-dispatch))

  (defn ppme [form]
    (-> form macroexpand ppc))

  (defmacro foo [& form]
    (ioc/state-machine form 2 &env {:Return handler-return}))

  (ppme
    '(foo (let [[x y z] :bar] [z y x]))
    )

  (ppc (handler-form '[]))

  (handle []
    1)

  (handle [(decide [] (continue true))]
    (let [x (if (decide c) 10 20)
          y (if (decide c) 0 5)]
      (- x y)))

  (handle [(value [x]
             [x])
           (decide []
             (concat (continue true) (continue false)))]
    (let [x (if (decide c) 10 20)
          y (if (decide c) 0 5)]
      (value (- x y))))

)
