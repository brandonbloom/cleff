(ns cleff.trampoline
  (:require [cleff.protocols :as proto]
            [clojure.core.async.impl.ioc-macros :as ioc
             :refer (state-machine run-state-machine aget-object aset-all!)])
  (:import [java.util.concurrent.atomic AtomicReferenceArray]))


;;; Utilities

(defn clone-state [^AtomicReferenceArray state]
  (let [n (.length state)
        clone (AtomicReferenceArray. n)]
    (doseq [i (range n)]
      (aset-all! clone i (aget-object state i)))
    clone))


;;; Terminator implementations

(def TRAMPOLINE-IDX       ioc/USER-START-IDX   )
(def COMMUNICATION-IDX (+ ioc/USER-START-IDX 1))
(def HANDLER-IDX       (+ ioc/USER-START-IDX 2))
(def USER-COUNT                              3 )

(defn begin [state blk]
  (aset-all! state ioc/STATE-IDX blk)
  nil)

(defn continue [state blk value]
  (aset-all! state
             ioc/STATE-IDX blk
             ioc/VALUE-IDX value
             TRAMPOLINE-IDX :continue)
  nil)

(defn effect [state blk & args]
  (aset-all! state
             ioc/STATE-IDX blk
             TRAMPOLINE-IDX :effect
             COMMUNICATION-IDX args)
  nil)

(defn run-with [state blk & args]
  (aset-all! state
             ioc/STATE-IDX blk
             TRAMPOLINE-IDX :run-with
             COMMUNICATION-IDX args)
  nil)

(defn return [state value]
  (aset-all! state
             ioc/VALUE-IDX value
             TRAMPOLINE-IDX :return)
  nil)


;;; Coroutine state machines

(def operation-terminators
  {'begin `begin
   'continue `continue
   :Return `return})

(def transform-terminators
  {'begin `begin
   :Return `return})

(def computation-terminators
  {'effect `effect
   'cleff.core/run-with `run-with
   :Return `return})

(defn handler-form [terminators bindings-sym env args body]
  (let [form `(let [~args (~'begin)] ~@body)]
    `(let [state# (~(state-machine (list form) USER-COUNT env terminators))]
       (aset-all! state# ioc/BINDINGS-IDX ~bindings-sym)
       (run-state-machine state#)
       state#)))

(defn operation-form [bindings-sym env args body]
  (handler-form operation-terminators bindings-sym env args body))

(defn transform-form [bindings-sym env arg body]
  (handler-form transform-terminators bindings-sym env arg body))

(defn computation-form [bindings-sym env body]
  `(let [state# (~(state-machine body USER-COUNT env computation-terminators))]
     (aset-all! state# ioc/BINDINGS-IDX ~bindings-sym)))


;;; Interpreter

(defn push-handler [stack handler]
  (let [value-frame (clone-state (proto/-value handler))]
    (aset-all! value-frame HANDLER-IDX handler)
    (if-let [finally-frame (proto/-finally handler)]
      (conj stack finally-frame (clone-state value-frame))
      (conj stack value-frame))))

(defmulti step-method (fn [frame stack]
                        ;(println "step:" (aget-object frame TRAMPOLINE-IDX))
                        (aget-object frame TRAMPOLINE-IDX)))

(defmethod step-method :effect [frame stack]
  (let [[effect operation & args] (aget-object frame COMMUNICATION-IDX)]
    ;(println "Effect:" (pr-str effect operation args))
    (loop [continuation (list frame)
           [frame* & stack*] stack]
      (if frame*
        (let [frame* (clone-state frame*)
              handler (aget-object frame* HANDLER-IDX)]
          (if-let [op-frame (and handler (proto/-operation handler effect operation))]
            (conj stack* (aset-all! (clone-state op-frame)
                                    ioc/VALUE-IDX args
                                    COMMUNICATION-IDX (conj continuation frame*)))
            (recur (conj continuation frame*) stack*)))
        (throw (Exception. (str "No handler for " operation " on " effect)))))))

(defmethod step-method :run-with [frame stack]
  (let [[handler computation] (aget-object frame COMMUNICATION-IDX)]
    ;(println "Run With:" (pr-str handler computation))
    (-> stack
        (conj frame)
        (push-handler handler)
        (conj computation))))

(defmethod step-method :continue [frame stack]
  (let [value (aget-object frame ioc/VALUE-IDX)
        ;_ (println "Continue:" (pr-str value))
        continuation (aget-object frame COMMUNICATION-IDX)
        stack* (into (conj stack frame) (map clone-state continuation))]
    (aset-all! (peek stack*) ioc/VALUE-IDX value)
    stack*))

(defmethod step-method :return [frame stack]
  (let [value (aget-object frame ioc/VALUE-IDX)]
    ;(println "return:" (pr-str value))
    (if-let [frame* (peek stack)]
      (do
        (aset-all! frame* ioc/VALUE-IDX value)
        stack)
      (reduced value))))

(defn step [stack]
  ;(println "stack depth:" (count stack))
  (let [[frame & stack*] stack]
    (run-state-machine frame)
    (step-method frame stack*)))

(defn run [handler computation]
  (loop [i 0 stack (-> nil (push-handler handler) (conj computation))]
    (when (> i 50) (throw (Exception. "ITERATION LIMIT"))) ;TODO delete me
    (if (reduced? stack)
      @stack
      (recur (inc i) (step stack)))))
