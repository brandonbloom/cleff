(ns cleff.core
  (:require [cleff.protocols :as proto]
            [cleff.trampoline :as trampoline
             :refer (operation-fn computation-fn)]))

;;TODO parameterize with effect type
(defn instance []
  (reify proto/IEffect))

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
       (reify proto/IHandler
         (proto/-value [this#]
           (proto/-operation this# ::none '~'value))
         (proto/-operation [this# effect# name#]
           (get-in map# [effect# name#]))))))

(defmacro handler [& specs]
  (let [bindings-sym (gensym "bindings__")]
    `(let [~bindings-sym (clojure.lang.Var/getThreadBindingFrame)]
       ~(handler-fn bindings-sym &env specs))))

(defmacro handle-with [handler & body]
  (let [bindings-sym (gensym "bindings__")]
    `(let [~bindings-sym (clojure.lang.Var/getThreadBindingFrame)
           computation# ~(computation-fn bindings-sym &env body)]
       (trampoline/run ~handler computation#))))

(defmacro handle [specs & body]
  (let [bindings-sym (gensym "bindings__")]
    `(let [~bindings-sym (clojure.lang.Var/getThreadBindingFrame)
           handler# ~(handler-fn bindings-sym &env specs)
           computation# ~(computation-fn bindings-sym &env body)]
       (trampoline/run handler# computation#))))


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
