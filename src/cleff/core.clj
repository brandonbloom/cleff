(ns cleff.core
  (:require [clojure.core.async.impl.ioc-macros :as ioc
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

(defn handler-message [state blk]
  (aset-all! state ioc/STATE-IDX blk)
  nil)

(defn handler-continue [state blk value]
  (aset-all! state
             ioc/STATE-IDX blk
             ioc/VALUE-IDX value
             trampoline-idx :continue)
  nil)

(defn handler-return [state value]
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
    `(let [[operation# & ~args-sym] (~'message)]
       (case operation#
         ~@(apply concat (for [[operation arglist & body] operations]
                           `[~operation (let [~arglist ~args-sym]
                                          ~@body)]))
         ;;TODO forward operation up the handler stack
         ;;TODO: figure out why this is throwing unconditionally...
         #_(throw (Exception. (str "Unexpected operation " operation#)))))))

(defn handler-fn [bindings-sym env operations]
  `(let [state# (~(state-machine (list (handler-form operations))
                                 2 env handler-terminators))]
     (aset-all! state# ioc/BINDINGS-IDX ~bindings-sym)
     (run-state-machine state#)
     state#))

(defn computation-fn [bindings-sym env body]
  `(let [state# (~(state-machine body 2 env computation-terminators))]
     (aset-all! state# ioc/BINDINGS-IDX ~bindings-sym)))


;;; Interpretation trampoline

(defn run [handler computation]
  (loop [stack (list computation)]
    ;(println "loop depth:" (count stack))
    (let [frame (peek stack)]
      (run-state-machine frame)
      ;(println "step: " (aget-object frame trampoline-idx))
      (case (aget-object frame trampoline-idx)
        :effect
          (let [message (aget-object frame communication-idx)
                ;_ (println "Effect:" (pr-str message))
                handler* (aset-all! (clone-state handler)
                                    ioc/VALUE-IDX message
                                    communication-idx frame)]
            (recur (-> stack pop (conj handler*))))
        :value
          (let [value (aget-object frame communication-idx)
                ;_ (println "Value:" (pr-str value))
                handler* (aset-all! (clone-state handler)
                                    ioc/VALUE-IDX ['value value]
                                    communication-idx frame)]
            (recur (-> stack pop (conj handler*))))
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
    (state-machine form 0 &env {:Return `handler-return}))

  (ppme '(foo :bar))

  (ppme
    '(foo (let [[x y z] :bar] [z y x]))
    )


  (ppc (handler-form '[]))
  (ppc (handler-form '[(decide [] (continue true))]))
  (ppc (handler-form '[(decide [] (continue true))
                       (value [x] x)]))

  (handle []
    :foo)

  (handle [(value [x] [x x])]
    (value :foo))

  (handle [(decide [] (continue true))]
    (let [x (if (effect 'decide) 10 20)
          y (if (effect 'decide) 0 5)]
      (- x y)))
  ;;=> 10

  (handle [(decide [] (continue false))]
    (let [x (if (effect 'decide) 10 20)
          y (if (effect 'decide) 0 5)]
      (- x y)))
  ;;=> 15

  (handle [(value [x]
             [x])
           (decide []
             (concat (continue true) (continue false)))]
    (let [x (if (effect 'decide) 10 20)
          y (if (effect 'decide) 0 5)]
      (value (- x y))))
  ;;=> (10 5 20 15)

  ;; Not working below here: Cleaned up syntax & effect "instances"

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
