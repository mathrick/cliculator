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

(defparser rpn
  "
<expr> = number | add | sub | mul | div
add = expr expr <ws> <'+'>
sub = expr expr <ws> <'-'>
mul = expr expr <ws> <'*'>
div = expr expr <ws> <'/'>
number = <ws>? #'-?[0-9]+' <ws>?
ws = #'\\s+'
")

(defn- normalise-tree [tree]
  "Common helper to clean up and normalise the parse tree for both ordinary and RPN
  notation, so that only a tree of ops which can be passed directly to eval-op is left"
  (insta/transform {:number (comp clojure.edn/read-string str)
                                 :add (partial op :+)
                                 :sub (partial op :-)
                                 :mul (partial op :*)
                                 :div (partial op :/)
                                 :neg (partial op :-)
                                 :pos (partial op :+)}
                                tree))

(defn parse-it [parser expr]
  (match (->> expr
              (insta/parse parser)
              normalise-tree)
    (fail :guard insta/failure?) (throw (IllegalArgumentException. (instaparse.failure/pprint-failure fail)))
    ([nested] :seq) nested))


(defmethod parse :ordinary [_ expr]
  (parse-it ordinary-notation expr))

(defmethod parse :rpn [_ expr]
  (parse-it rpn expr))
