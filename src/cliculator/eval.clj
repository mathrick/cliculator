(in-ns 'cliculator.core)

(defn dispatch-op [x]
  (match x
    {:binary true :op op :left _ :right _} [::binary op]
    {:binary false :op op :operand _} [::unary op]
    (_ :guard number?) ::number
    :else :default))

(defmulti eval-op "Evaluate given operation object and return its result" dispatch-op)

(defmethod eval-op ::number [op] op)

(defmethod eval-op '[::binary :+] [{:keys [left right]}]
  (+ (eval-op left) (eval-op right)))
(defmethod eval-op [::binary :-] [{:keys [left right]}]
  (- (eval-op left) (eval-op right)))
(defmethod eval-op [::binary :*] [{:keys [left right]}]
  (* (eval-op left) (eval-op right)))
(defmethod eval-op [::binary :/] [{:keys [left right]}]
  (/ (eval-op left) (eval-op right)))

(defmethod eval-op [::unary :+] [{:keys [operand]}]
  (eval-op operand))
(defmethod eval-op [::unary :-] [{:keys [operand]}]
  (- (eval-op operand)))

(defmethod eval-op :default [op]
  (throw (IllegalArgumentException. (format "%s is not a valid operation object, can't evaluate" op))))
