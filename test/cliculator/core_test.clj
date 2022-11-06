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

(deftest evaluation
  (testing "Numbers evaluate to themselves"
    (is (= (eval-op 13) 13)))

  (testing "Simple unary operators evaluate correctly"
    (is (= (eval-op (op :+ 11)) (+ 11)))
    (is (= (eval-op (op :- 42)) (- 42))))

  (testing "Simple binary operators evaluate correctly"
    (is (= (eval-op (op :+ 1 2)) (+ 1 2)))
    (is (= (eval-op (op :+ -1 2)) (+ -1 2)))
    (is (= (eval-op (op :- 7 5)) (- 7 5)))
    (is (= (eval-op (op :- 5 7)) (- 5 7)))
    (is (= (eval-op (op :* 2 8)) (* 2 8)))
    (is (= (eval-op (op :* -2 -8)) (* -2 -8)))
    (is (= (eval-op (op :/ 8 4)) (/ 8 4)))
    (is (= (eval-op (op :/ 8 -4)) (/ 8 -4)))
    (is (= (eval-op (op :/ 1 3)) (/ 1 3))))

  (testing "Compound operators evaluate correctly"
    (let [x (op :+ 42 13)
          y (op :* 11 23)
          z (op :/ x y)
          expr (op :+ 13 z)]
      (is (= (eval-op expr) (+ 13 (/ (+ 42 13)
                                     (* 11 23))))))))

(deftest parser
  (testing "Simple ordinary notation can be parsed successfully"
    (is (= (parse :ordinary "42") 42))

    (is (= (parse :ordinary "-42") (op :- 42)))
    (is (= (parse :ordinary "- 42") (op :- 42)))

    (is (= (parse :ordinary "+11") (op :+ 11)))
    (is (= (parse :ordinary "+ 11") (op :+ 11)))

    (is (= (parse :ordinary "13 + 11") (op :+ 13 11)))
    (is (= (parse :ordinary "13 - 11") (op :- 13 11)))
    (is (= (parse :ordinary "13 * 11") (op :* 13 11)))
    (is (= (parse :ordinary "13 / 11") (op :/ 13 11))))

  (testing "Complex ordinary notation can be parsed successfully"
    (is (= (parse :ordinary "13 + 11 - 2") (op :- (op :+ 13 11) 2)))
    (is (= (parse :ordinary "13 + 11 * 2") (op :+ 13 (op :* 11 2))))
    (is (= (parse :ordinary "(13 + 11) * 2") (op :* (op :+ 13 11) 2)))

    (is (= (parse :ordinary "42 - 21 / 7") (op :- 42 (op :/ 21 7))))
    (is (= (parse :ordinary "(42 - 21) / 7") (op :/ (op :- 42 21) 7))))

  (testing "Reverse Polish notation can be parsed successfully"
    (is (= (parse :rpn "42") 42))
    (is (= (parse :rpn "-42") -42))
    (is (= (parse :rpn "42 12 +") (op :+ 42 12)))
    (is (= (parse :rpn "42 12 + 7 -") (op :- (op :+ 42 12) 7)))
    (is (= (parse :rpn "42 -12 + 7 -") (op :- (op :+ 42 -12) 7)))
    (is (= (parse :rpn "42 12 15 13 + + +") (op :+ 42 (op :+ 12 (op :+ 15 13)))))))

(deftest parsing-and-evaluation
  (testing "Ordinary notation expression can be parsed and evaluated correctly"
    (let [parse-and-eval (fn [expr] (eval-op (parse :ordinary expr)))]
      (is (= (parse-and-eval "42") 42))
      (is (= (parse-and-eval "-42") -42))

      (is (= (parse-and-eval "+11") 11))
      (is (= (parse-and-eval "13 / 11") 13/11))

      (is (= (parse-and-eval "13 + 11 - 2") 22))
      (is (= (parse-and-eval "13 + 11 * 2") 35))
      (is (= (parse-and-eval "(13 + 11) * 2") 48))

      (is (= (parse-and-eval "42 - 21 / 7") (- 42 21/7)))
      (is (= (parse-and-eval "(42 - 21) / 7") 3))))
  (testing "Reverse Polish notation expression can be parsed and evaluated correctly"
    (let [parse-and-eval (fn [expr] (eval-op (parse :rpn expr)))]
      (is (= (parse-and-eval "42") 42))
      (is (= (parse-and-eval "-42") -42))
      (is (= (parse-and-eval "42 12 +") 54))
      (is (= (parse-and-eval "42 12 + 7 -") 47))
      (is (= (parse-and-eval "42 -12 + 7 -") 23))
      (is (= (parse-and-eval "42 12 15 13 + + /") 42/40)))))
