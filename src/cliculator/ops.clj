(in-ns 'cliculator.core)

(def unary-op? "Set of known unary operators"
  #{:- :+})

(def binary-op? "Set of known binary operators"
  #{:+ :- :* :/})

(def op? "Set of all known operators"
  (union unary-op? binary-op?))

(defn operand? [thing]
  (match [thing]
    [(_ :guard number?)] true
    [{:op (_ :guard binary-op?)
      :binary true
      :left (_ :guard operand?)
      :right (_ :guard operand?)}] true
    [{:op (_ :guard unary-op?)
      :binary false
      :operand (_ :guard operand?)}] true
    :else false))

(defn op "Constructor for creating an operation of specified type and arguments"
  ([type operand]
   (match [type operand]

     [(_ :guard unary-op?) (_ :guard operand?)]
     {:op type :binary false :operand operand}

     :else (throw (IllegalArgumentException.
                   (format "(%s %s) is not a valid unary operation" type operand)))))
  ([type left right]
   (match [type left right]

     [(_ :guard binary-op?) (_ :guard operand?) (_ :guard operand?)]
     {:op type :binary true :left left :right right}

     :else (throw (IllegalArgumentException.
                   (format "(%s %s %s) is not a valid binary operation" left type right))))))
