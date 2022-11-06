(in-ns 'cliculator.core)

(defmulti parse (fn [style expr] style))

(defparser ordinary-notation
  "
<expr> = op
number = <ws> digit digit* <ws>
<digit> = ('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9')+
<op> = binop | unop
<binop> = mul-div | add | sub
add = binop <ws> <'+'> <ws> mul-div
sub = binop <ws> <'-'> <ws> mul-div
<mul-div> = term | mul | div
mul = mul-div <ws> <'*'> <ws> term
div = mul-div <ws> <'/'> <ws> term
<term> = number | unop | mul-div | <'('> binop <')'>
<unop> = neg | pos
neg = <'-'> (number | <'('> term <')'>)
pos = <'+'> (number | <'('> term <')'>)
ws = #'\\s*'
"
  :no-slurp true)

(defmethod parse :ordinary [_ expr]
  (let [binopify (fn [left sym right]
                   (op (keyword sym) left right))
        unopify (fn [sym operand]
                  (op (keyword sym operand)))
        result (insta/parse ordinary-notation expr)
        result (insta/transform {:number (comp clojure.edn/read-string str)
                                 :add (partial op :+)
                                 :sub (partial op :-)
                                 :mul (partial op :*)
                                 :div (partial op :/)
                                 :neg (partial op :-)
                                 :pos (partial op :+)}
                                result)]
    result
    (match result
      (_ :guard insta/failure?) (throw (IllegalArgumentException. (instaparse.failure/pprint-failure result)))
      ([nested] :seq) nested
      :else result)))
