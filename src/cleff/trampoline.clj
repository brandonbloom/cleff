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

(def trampoline-idx ioc/USER-START-IDX)
(def communication-idx (+ trampoline-idx 1))

(defn operation-message [state blk]
  (aset-all! state ioc/STATE-IDX blk)
  nil)

(defn operation-continue [state blk value]
  (aset-all! state
             ioc/STATE-IDX blk
             ioc/VALUE-IDX value
             trampoline-idx :continue)
  nil)

(defn operation-return [state value]
  (aset-all! state
             ioc/VALUE-IDX value
             trampoline-idx :return)
  nil)

(defn computation-effect [state blk & message]
  (aset-all! state
             ioc/STATE-IDX blk
             trampoline-idx :effect
             communication-idx message)
  nil)

(defn computation-value [state blk value]
  (aset-all! state
             ioc/STATE-IDX blk
             trampoline-idx :value
             communication-idx value)
  nil)

(defn computation-return [state value]
  (aset-all! state
             ioc/VALUE-IDX value
             trampoline-idx :return)
  nil)


;;; Coroutine state machines

(def operation-terminators
  {'message `operation-message
   'continue `operation-continue
   :Return `operation-return})

(def computation-terminators
  {'effect `computation-effect
   'value `computation-value
   :Return `computation-return})

(defn operation-fn [bindings-sym env args body]
  ;; XXX Need 'do because destructuring is broken for top-level lets in IOC.
  (let [form `(do (let [~args (~'message)] ~@body))]
    `(let [state# (~(state-machine form 2 env operation-terminators))]
       (aset-all! state# ioc/BINDINGS-IDX ~bindings-sym)
       (run-state-machine state#)
       state#)))

(defn computation-fn [bindings-sym env body]
  `(let [state# (~(state-machine body 2 env computation-terminators))]
     (aset-all! state# ioc/BINDINGS-IDX ~bindings-sym)))


;;; Interpretation

(defn run [handler computation]
  (loop [stack (list computation)]
    ;(println "loop depth:" (count stack))
    (let [frame (peek stack)]
      (run-state-machine frame)
      ;(println "step: " (aget-object frame trampoline-idx))
      (case (aget-object frame trampoline-idx)
        :effect
          (let [[effect operation & args] (aget-object frame communication-idx)
                ;_ (println "Effect:" (pr-str effect operation args))
                frame* (proto/-operation handler effect operation)
                _ (assert frame* "Operation not found") ;TODO handler stack
                frame* (clone-state frame*)]
            (aset-all! frame*
                       ioc/VALUE-IDX args
                       communication-idx frame)
            (recur (-> stack pop (conj frame*))))
        :value
          (let [value (aget-object frame communication-idx)
                ;_ (println "Value:" (pr-str value))
                frame* (proto/-value handler)
                _ (assert frame*) ;TODO pass up handler stack
                frame* (clone-state frame*)]
            (aset-all! frame*
                       ioc/VALUE-IDX [value])
            (recur (-> stack pop (conj frame*))))
        :continue
          (let [value (aget-object frame ioc/VALUE-IDX)
                ;_ (println "Continue:" (pr-str value))
                continuation (aget-object frame communication-idx)
                frame* (aset-all! (clone-state continuation)
                                  ioc/VALUE-IDX value)]
            (recur (conj stack frame*)))
        :return
          (let [value (aget-object frame ioc/VALUE-IDX)
                ;_ (println "return:" (pr-str value))
                stack* (pop stack)
                frame* (peek stack*)]
            (if frame*
              (do
                (aset-all! frame* ioc/VALUE-IDX value)
                (recur stack*))
              value))
        (throw (Exception. (str "Unexpected trampoline command:  "
                                (pr-str (aget-object frame trampoline-idx)))))
        ))))
