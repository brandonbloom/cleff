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
  {'continue `handler-continue
   :Return `handler-return})

(def computation-terminators
  {'effect `computation-effect
   'value `computation-value
   :Return `computation-return})

(defn handler-fn [bindings-sym env body]
  `(let [state# (~(ioc/state-machine body 2 env handler-terminators))]
     (ioc/aset-object state#
                      ioc/BINDINGS-IDX ~bindings-sym
                      )
     ))

(defn computation-fn [bindings-sym env body]
  `(let [state# (~(ioc/state-machine body 2 env computation-terminators))]
     (ioc/aset-object state#
                      ioc/BINDINGS-IDX ~bindings-sym
                      )
     ))


;;; Interpretation trampoline

(defn run [handler computation]
  (loop [stack (list computation)]
    ))

(defn handle-fn [env handler-body computation-body]
  (let [bindings-sym (gensym "bindings__")]
    `(let [~bindings-sym (clojure.lang.Var/getThreadBindingFrame)
           handler# ~(handler-fn bindings-sym env handler-body)
           computation# ~(computation-fn bindings-sym env computation-body)]
       (run handler computation))))


;;; User Syntax

(defn handler-fn [cases]
  )

(defmacro handle [handler-cases]
  )


;;; Test Code

(comment

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
