(ns cliculator.core-test
  (:require [clojure.test :refer :all]
            [clojure.tools.macro :refer [macrolet]]
            [cliculator.core :refer :all]))

(deftest ops
  (testing "Binary ops can be constructed"
    (let [simple-plus {:op :+ :binary true :left 13 :right 42}
          simple-minus {:op :- :binary true :left 999 :right 666}
          simple-times {:op :* :binary true :left 11 :right 21}
          simple-divided {:op :/ :binary true :left 44 :right 22}
          unary-plus {:op :+ :binary false :operand 25}
          unary-minus {:op :- :binary false :operand 24}
          compound-plus {:op :+ :binary true :left 4242 :right simple-times}
          ]
      (is (= (op :+ 13 42) simple-plus))
      (is (= (op :- 999 666) simple-minus))
      (is (= (op :* 11 21) simple-times))
      (is (= (op :/ 44 22) simple-divided))
      (is (= (op :+ 25) unary-plus))
      (is (= (op :- 24) unary-minus))
      (is (= (op :+ 4242 simple-times) compound-plus))))
  (testing "Invalid ops fail validation"
    (macrolet [(test-it [form]
                        `(is (~'thrown-with-msg? IllegalArgumentException #"not a valid binary" ~form)))]
      (test-it (op :? 1 2))
      (test-it (op 42 13 11))
      (test-it (op :+ "foo" 2))
      (test-it (op :- {:op :+} 2)))

    (macrolet [(test-it [form]
                        `(is (~'thrown-with-msg? IllegalArgumentException #"not a valid unary" ~form)))]

      (test-it (op :- "foo"))
      (test-it (op :* 12))
      (test-it (op :+ {:op :+})))))
