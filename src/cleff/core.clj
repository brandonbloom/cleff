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


;;; Protocols

(defprotocol IEffect)

(defprotocol IHandler
  (-value [this])
  (-operation [this effect name]))


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


;;; Interpretation trampoline

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
                frame* (-operation handler effect operation)
                _ (assert frame* "Operation not found") ;TODO handler stack
                frame* (clone-state frame*)]
            (aset-all! frame*
                       ioc/VALUE-IDX args
                       communication-idx frame)
            (recur (-> stack pop (conj frame*))))
        :value
          (let [value (aget-object frame communication-idx)
                ;_ (println "Value:" (pr-str value))
                frame* (-value handler)
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


;;; User Syntax

;;TODO parameterize with effect type
(defn instance []
  (reify IEffect))

;;TODO better specs syntax & parser
(defn handler-fn [bindings-sym env specs]
  (letfn [(impl-map [operations]
            (into {} (for [[name args & body] operations]
                       [(list 'quote name)
                        (operation-fn bindings-sym env args body)])))
          (effect-map [specs]
            (let [specs* (->> specs (partition-by seq?) (partition 2))]
              (into {} (for [[[effect] operations] specs*]
                         [effect (impl-map operations)]))))]
    `(let [map# ~(effect-map specs)]
       (reify IHandler
         (-value [this#]
           (-operation this# ::none '~'value))
         (-operation [this# effect# name#]
           (get-in map# [effect# name#]))))))

(defmacro handler [& specs]
  (let [bindings-sym (gensym "bindings__")]
    `(let [~bindings-sym (clojure.lang.Var/getThreadBindingFrame)]
       ~(handler-fn bindings-sym &env specs))))

(defmacro handle-with [handler & body]
  (let [bindings-sym (gensym "bindings__")]
    `(let [~bindings-sym (clojure.lang.Var/getThreadBindingFrame)
           computation# ~(computation-fn bindings-sym &env body)]
       (run ~handler computation#))))

(defmacro handle [specs & body]
  (let [bindings-sym (gensym "bindings__")]
    `(let [~bindings-sym (clojure.lang.Var/getThreadBindingFrame)
           handler# ~(handler-fn bindings-sym &env specs)
           computation# ~(computation-fn bindings-sym &env body)]
       (run handler# computation#))))


;;; Test Code

(comment

  (require 'clojure.pprint)

  (defn ppc [form]
    (clojure.pprint/write form :dispatch clojure.pprint/code-dispatch))

  (defn ppme [form]
    (-> form macroexpand ppc))

  (defmacro foo [& form]
    (state-machine form 0 &env {:Return `operation-return}))

  (defn choice []
    (instance))

  (def c (choice))

  (ppme '(foo :bar))

  (ppme
    '(foo (let [[x y z] :bar] [z y x]))
    )

  (ppme '(handler))

  (ppme '(handler ::none (value [x] x)))

  (ppme '(handler c (decide [] (continue true))))

  (handle []
    :foo)

  (ppme
  '(handle [::none (value [x] [x x])]
    (value :foo))
    )

  (let [c (choice)]
    (handle [c (decide [] (continue true))]
      (let [x (if (effect c 'decide) 10 20)
            y (if (effect c 'decide) 0 5)]
        (- x y))))
  ;;=> 10

  (let [c (choice)]
    (handle [c (decide [] (continue false))]
      (let [x (if (effect c 'decide) 10 20)
            y (if (effect c 'decide) 0 5)]
        (- x y))))
  ;;=> 15

  (defn choose-all [c]
    (handler
      ::none
      (value [x] [x])
      c
      (decide []
        (concat (continue true) (continue false)))))

  (let [c (choice)]
    (handle-with (choose-all c)
      (let [x (if (effect c 'decide) 10 20)
            y (if (effect c 'decide) 0 5)]
        (value (- x y)))))
  ;;=> (10 5 20 15)

  ;; Not working below here: Need handler stacks

  (let [c1 (choice) c2 (choice)]
    (handle-with (choose-all c1)
      (handle-with (choose-all c2)
        (let [x (if (effect c1 'decide) 10 20)
              y (if (effect c2 'decide) 0 5)]
          (value (- x y))))))
  ;;=> ((10 5) (20 15))

  (let [c1 (choice) c2 (choice)]
    (handle-with (choose-all c1)
      (handle-with (choose-all c2)
        (let [x (if (effect c2 'decide) 10 20)
              y (if (effect c1 'decide) 0 5)]
          (value (- x y))))))
  ;;=> ((10 20) (5 15))

)
