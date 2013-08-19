(ns cleff.core-test
  (:require [clojure.test :refer :all]
            [cleff.core :refer (instance handler handle handle-with)]))

(deftest choice-test

  (defn choice []
    (instance))

  (is (= (let [c (choice)]
           (handle [c (decide [] (continue true))]
             (let [x (if (effect c 'decide) 10 20)
                   y (if (effect c 'decide) 0 5)]
               (- x y))))
         10))

  (is (= (let [c (choice)]
           (handle [c (decide [] (continue false))]
             (let [x (if (effect c 'decide) 10 20)
                   y (if (effect c 'decide) 0 5)]
               (- x y))))
         15))

  (defn choose-all [c]
    (handler
      (value [x] [x])
      c
      (decide []
        (concat (continue true) (continue false)))))

  (is (= (let [c (choice)]
           (handle-with (choose-all c)
             (let [x (if (effect c 'decide) 10 20)
                   y (if (effect c 'decide) 0 5)]
               (- x y))))
         '(10 5 20 15)))

  (is (= (let [c1 (choice) c2 (choice)]
           (handle-with (choose-all c1)
             (handle-with (choose-all c2)
               (let [x (if (effect c1 'decide) 10 20)
                     y (if (effect c2 'decide) 0 5)]
                 (- x y)))))
         '((10 5) (20 15))))

  (is (= (let [c1 (choice) c2 (choice)]
           (handle-with (choose-all c2)
             (handle-with (choose-all c1)
               (let [x (if (effect c2 'decide) 10 20)
                     y (if (effect c1 'decide) 0 5)]
                 (- x y)))))
         '((10 5) (20 15))))

)
