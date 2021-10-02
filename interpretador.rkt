#lang eopl
;; INTEGRANTES
;; LISETH DAYANA CASTILLO QUIÑONES - COD 1843187
;; ANDRES FELIPE ARRECHEA SAA - COD 1780023
;; JAVIER ANDRES ARARAT - COD 1810221

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>       ::= <globales> <expression>
;;                      <a-program (glo exp)>
;;  <globales>      ::= global <identifier> = ({<expression>}*(,))
;;                      <global-exp (ids rands)>
;;  <expression>    ::= <number> | <caracter> | <cadena> | <bool>
;;                      <data-exp (datum)>
;;                  ::= <lista>
;;                      <list-exp (datum)>
;;                  ::= <vector>
;;                      <vec-exp (datum)>
;;                  ::= <register>
;;                      <reg-exp (datum)>
;;                  ::= <expr-bool>
;;                      <expr-bool (datum)>
;;                  ::= var { <identifier> = <expression> }*(,) in <expression>
;;                      <var-exp (ids rands body)>
;;                  ::= cons { <identifier> = <expression> }*(,) in <expression>
;;                      <cons-exp (ids rands body)>
;;                  ::= rec { <identifier> ( { <identifier> }*(,) ) = <expression> }* in <expression>
;;                      <rec-exp (function-names idss bodies bodyrec)>
;;	            ::= function ( { <identifier> }*(,) )  <expression> 
;;                      <function-exp ( idss bodies )>
;;                  ::= unic { <identifier> = <expression> }*(,) in <expression>
;;                      <unic-exp (ids rands body)>
;;                  ::= sequence {<expression>+(;)} end
;;                      <sequence-exp (seq rands)>
;;                  ::= if <expr-bool> then <expression> else  <expression>  end
;;                      <if-exp (exp-bool exp1 exp2)>
;;                  ::= cond { "[" <expression> <expression> "]" }* else <expression>
;;                      <cond-exp (conds values exp)>
;;                  ::= while <expr-bool> do <expression> done
;;                      <while-exp (exp-bool exp)>
;;                  ::= for <identifier> = <expression> (to | downto) <expression> do <expression> done
;;                      <for-exp (id exp1 exp2 exp3)>
;;                  ::= (<expression> {<expression>}*)
;;                      <app-exp function rands>
;;                  ::= letrec  {identifier ({identifier}*(,)) = <expression>}* in <expression>
;;                     <letrec-exp(function-names idss bodies bodyletrec)>
;;                  ::= set <identifier> = <expression>
;;                     <set-exp (id rhsexp)>
;;                  ::= {(<expression> <primitive> <expression>)}*(,))
;;                      <primapp-exp (prim rands)>
;;  <list>          ::= [{<expression>}*(,)]
;;  <vector>        ::= vector[{<expression>}*(,)]
;;  <register>      ::= register{ { <identifier> = <expression>}+(,) }
;;  <expr-bool>     ::= compare(<expression> <pred-prim> <expression> )
;;                  ::= <oper-bin-bool> (<expr-bool>, <expr-bool>)
;;                  ::= <bool>
;;                  ::= <oper-un-bool (expr-bool)>
;;  <pred-prim>     ::= < | > | <= | >= | == | <>
;;  <oper-bin-bool> ::= and | or | xor
;;  <oper-un-bool>  ::= not
;;  <primitive>     ::= + | - | * | add1 | sub1
;;                  ::= length | concat
;;                  ::= empty? | empty | create-list | list? | head | tail | append | list-position
;;                  ::= vector? | create-vector | ref-vector
;;                  ::= registers? | create-register | ref-register| set-register

;******************************************************************************************


;Especificación Léxica

(define lexical 
'((white-sp
   (whitespace) skip)
  (comment
   ("//" (arbno (not #\newline))) skip)
  (identifier
   ( "$" letter (arbno (or letter digit "?"))) symbol)
  ;(identifier
   ;( "@$" letter (arbno (or letter digit))) symbol)
  (number
   (digit (arbno digit)) number)
  (number
   ("-" digit (arbno digit)) number)
  (number
   (digit (arbno digit) "." (arbno digit)) number)
  (number
   ("-" digit (arbno digit) "." (arbno digit)) number)
    
  (string
   ;("**" (arbno (not #\* #\* )) "**") string)
   ("\"" (arbno (not #\" #\" )) "\"") string)
  (char
   ("'" letter ) symbol)
 )
)


;Especificación Sintáctica (gramática)

(define grammar
  '(
    (program (global expression) a-global-program) 
    (program (expression) a-program) 

    (global ("global" "(" ( separated-list identifier "="  expression ",")  ")") global-exp)
    

    (expression (number) number-lit)
    (expression ("x8" "(" (separated-list number ",") ")") number-oct);ok

    (expression (identifier) id-exp)
    (expression ("@"identifier) ref-id-exp)
    (expression (string) string-lit)
    (expression (char) char-lit)
    (expression (list) list-exp)
    (expression (vector) vec-exp)
    (expression (register) reg-exp)
    (expression (expr-bool) expr-bool-exp)
    (expression (C-VID-VAL) c-vid-val-exp)
    (expression ("print" "(" expression ")") print-exp)
    (expression ("var" "(" ( separated-list identifier "="  expression ",") ")" "in" expression ) var-exp)
    (expression ("cons" "(" ( separated-list identifier "="  expression ",") ")" "in" expression ) cons-exp)
    (expression ("rec" (arbno identifier "(" (separated-list identifier ",") ")"  "=" expression) "in"  expression "end") rec-exp) 
    (expression ("function" "(" (separated-list identifier ",") ")" expression)  function-exp)
    (expression ("unic" (arbno identifier "="  expression "," ) "in" expression ) unic-exp)
    (expression ("sequence" expression (arbno ";" expression) "end") sequence-exp) 
    (expression ("if" expr-bool "then" expression "else" expression "end") if-exp)
    (expression ("cond"  (arbno "[" expr-bool expression "]") "else"  expression ) cond-exp)
    (expression ("while" expr-bool "do" expression "done") while-exp)
    (expression ("for" identifier "=" expression  "to"  expression "do" expression "done") for-expr)
    (expression ("fordownto" identifier "=" expression "downto" expression "do" expression "done") fordownto-expr)
    (expression ("eval" "(" expression (arbno expression) ")") eval-exp)
    (expression ("set" identifier "=" expression) set-exp)
    (expression ("(" expression primitive-binary expression ")")  prim-binary-exp)
    (expression ("[" identifier primitive-unary-operator "]")  prim-unary-com-exp)
    (expression (primitive-unary "(" ( separated-list expression ",") ")")  prim-unary-exp)
    (expression (primitive-unary-list "(" expression ")") prim-unary-list-exp)
    (expression (primitive-unary-list-cre "(" expression  list ")" ) prim-binary-compound-exp )
    (expression (primitive-unary-vector-ref "(" expression number ")" ) prim-unary-vecref-exp )
    (expression (primitive-unary-vector-set "(" expression number expression ")" ) prim-unary-vecset-exp )
    (expression (primitive-unary-register-cre "(" expression "," expression ")" ) prim-unary-regcre-exp )
    (expression (primitive-unary-register-ref "(" expression expression ")" ) prim-unary-regref-exp )
    (expression (primitive-unary-register-set "(" expression expression expression ")" ) prim-unary-regset-exp )
    (expression (primitive-unary-pre "(" expression ")") prim-unary-vecpre-exp)
    (expression ("list-position" "(" expression "," expression ")") list-position-exp)

    (list ("list" "[" (separated-list expression ";")  "]") def-list) 
    (vector ("vector" "[" (separated-list expression ";")  "]") def-vector)
    (register ("register" "{" (separated-list string "=" expression ";")  "}") def-register)
    (C-VID-VAL ("C-VID-VAL") def-c-vid-val)
    
    (expr-bool ("compare" "("  expression pred-prim expression ")") compare-exp-bool)
    (expr-bool (oper-bin-bool "(" expr-bool "," expr-bool ")") oper-bin-exp-bool)
    (expr-bool (bool) bool-exp-bool)
    (expr-bool (oper-un-bool "(" expr-bool ")") oper-un-exp-bool)
   
    (pred-prim (">") pred-prim-greater)
    (pred-prim ("<") pred-prim-less)
    (pred-prim (">=") pred-prim-greater-equal)
    (pred-prim ("<=") pred-prim-less-equal)
    (pred-prim ("==") pred-prim-equal)
    (pred-prim ("<>") pred-prim-different)

    (oper-bin-bool ("and") obb-and)
    (oper-bin-bool ("or") obb-or)
    (oper-bin-bool ("xor") obb-xor)
    (oper-un-bool ("not") obb-not)

    (bool ("true") true-bool)
    (bool ("false") false-bool)

    (primitive-binary ("+") primitive-sum)
    (primitive-binary ("~") primitive-subt)
    (primitive-binary ("*") primitive-mult)
    (primitive-binary ("/") primitive-div)
    (primitive-binary ("+_oct") primitive-sum-oct)
    (primitive-binary ("~_oct") primitive-subt-oct)
    (primitive-binary ("*_oct") primitive-mult-oct)
    (primitive-binary ("/_oct") primitive-div-oct)
    (primitive-binary ("%_oct") primitive-mod-oct)
    (primitive-binary ("concat") primitive-concat)
    

    (primitive-unary-operator ("++") primitive-add1)
    (primitive-unary-operator ("--") primitive-sub1)

    ;; primitivas para listas
    (primitive-unary-list ("head") primitive-head)
    (primitive-unary-list ("tail") primitive-tail)
    (primitive-unary-list-cre ("create-list") primitive-cre-lis)
    

    ;; primitivas para vector
    (primitive-unary-vector-ref ("ref-vector") primitive-ref-vector)
    (primitive-unary-vector-set ("set-vector") primitive-set-vector)
    
    
    ;; primiivas para registros
    (primitive-unary-register-cre ("create-register") primitive-cr-register)
    (primitive-unary-register-ref ("ref-register") primitive-ref-register)
    (primitive-unary-register-set ("set-register") primitive-set-register)

    ;; predicados
    (primitive-unary-pre ("vector?") primitive-vector?)
    (primitive-unary-pre ("register?") primitive-register?)
    (primitive-unary-pre ("list?") primitive-list?)
    (primitive-unary-pre ("empty?") primitive-empty?)

    ;;---
    (primitive-unary ("create-vector") primitive-cr-vector)
    (primitive-unary ("append") primitive-append)
    (primitive-unary ("length") primitive-lenght)
    (primitive-unary ("empty") primitive-empty)
  )
)
	
;Tipos de datos para la sintaxis abstracta de la gramática

;Construidos automáticamente:

(sllgen:make-define-datatypes lexical grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes lexical grammar)))


;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser lexical grammar))

;;El Analizador Léxico (Scanner)
;
(define just-scan
  (sllgen:make-string-scanner lexical grammar))



;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      lexical
      grammar)))


;*******************************************************************************************
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-global-program  (global body) global)
      (a-program (body)
                 (eval-expression body (init-env)))
      )

    ))
; videos del 08-03-21 (DD-MM-AA)
; eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada
(define eval-expression
  (lambda (exp env)
     (cases expression exp
      (number-lit (datum) datum)
      (number-oct (datoct) datoct)
      (string-lit (texto) texto)
      (char-lit (letra) letra)
      (id-exp (id) (apply-env env id))
      (expr-bool-exp (exp-bool) (eval-exp-bool exp-bool env))
      (print-exp (exp) (cons (eval-expression exp env) '()))
      ;(list-exp (valores) (map (eval-expression valores env) valores))

      (prim-binary-exp (arg1 prim arg2)                  
                       (apply-primitive
                        prim
                        (eval-expression arg1 env)
                        (eval-expression arg2 env)))
       
      (var-exp (ids rands body)
                (let
                    ((args (eval-rands rands env)))
                    (eval-expression body
                                  (extend-env ids args env))))
       
      (if-exp (exp-bool expr-t expr-f)
               (if(eval-exp-bool exp-bool env)
                   (eval-expression expr-t env)
                   (eval-expression expr-f env)))
      (cond-exp
        (exp-bool exp else-exp)
        (eval-cond exp-bool exp else-exp env))

      (function-exp (ids body)
                (closure ids body env))
       
      (eval-exp (rator rands)
                (let ((function (eval-expression rator env))
                     (args (eval-rands rands env)))
                 (if (functionval? function)
                     (apply-function function args)
                     (eopl:error 'eval-expression
                                 "Attempt to apply non-function ~s" function))))

      (rec-exp (function-names idss bodies letrec-body)
               (eval-expression letrec-body
                   (extend-env-recursively function-names idss bodies env)))
       
      (else #t)
       
      )
    ))


; Evaluar la declaraciones global
(define eval-global
  (lambda (glb)
    (cases global glb
      (global-exp (vars values) vars))))


; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

;apply-primitive: <primitiva> <list-of-expression> -> numero
(define apply-primitive
  (lambda (prim arg1 arg2)
    (cases primitive-binary prim
      
      (primitive-sum  () (+ arg1 arg2))
      (primitive-subt () (- arg1 arg2))
      (primitive-mult () (* arg1 arg2))
      (primitive-div  () (/ arg1 arg2))
      (primitive-sum-oct () (+_oct arg1 arg2 ))
      (primitive-subt-oct ()(-_oct arg1 ))
      (primitive-mult-oct () (*_oct arg1 arg2))
     ; (primitive-div-oct  () (/-oct arg1 arg2))
      
      (else #t))))



(define eval-exp-bool (lambda (e-bool env)
                        (cases expr-bool e-bool
                          (compare-exp-bool (exp1 prim exp2) (
                                                              (cases pred-prim prim
                                                                (pred-prim-greater () >)
                                                                (pred-prim-less () <)
                                                                (pred-prim-greater-equal () >=)
                                                                (pred-prim-less-equal () <=)
                                                                (pred-prim-equal () equal?)
                                                                (pred-prim-different () diferent?)
                                                                )
                                                              (eval-expression exp1 env) (eval-expression exp2 env) ))
                          
                          (oper-bin-exp-bool (oper exp1 exp2) (
                                                               (cases oper-bin-bool oper
                                                                 (obb-and () y)
                                                                 (obb-or () o)
                                                                 (obb-xor () xor)
                                                                 )
                                                              (eval-exp-bool exp1 env) (eval-exp-bool exp2 env)))
                          
                          (bool-exp-bool (val-bool) (cases bool val-bool
                                                      (true-bool ()#t)
                                                      (false-bool ()#f))) 

                          
                          (oper-un-exp-bool (oper exp) (not (eval-exp-bool exp env) ))
                          )))


(define diferent? (lambda (a b)
                    (if (equal? a b)
                        #f
                        #t)))

(define y (lambda (x y)
              (and x y)))

(define o (lambda (x y)
              (or x y)))

(define xor (lambda (x y)
              (and (or x y) (not (equal? x y)))
              
              ))

(define extraer-valores (lambda(lst)
                          (if
                             (null? lst)
                             '()
                             
                             (cons (cases expression (car lst)
                               (number-lit (datum) datum)
                             (else #f)
                             
                                ) (extraer-valores (cdr lst))))))

(define eval-cond (lambda (lst-exp-bool lst-exp else-exp env)
                    (if (null? lst-exp)
                        (eval-expression else-exp env)
                        (if (eqv? (eval-exp-bool (car lst-exp-bool) env) #t)
                             (eval-expression (car lst-exp) env)
                             (eval-cond (cdr lst-exp-bool ) (cdr lst-exp) else-exp env))
                        )
                    )
  )

;*******************************************************************************************
;Procedimientos
(define-datatype functionval functionval?
  (closure
   (ids (list-of symbol?))
   (body expression?)
   (env environment?)))

;apply-function: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-function
  (lambda (function args)
    (cases functionval function
      (closure (ids body env)
               (eval-expression body (extend-env ids args env))))))


;*******************************************************************************************
;Ambientes

; Ambiente inicial
(define init-env
  (lambda ()
    (extend-env
     '($x $y $z)
     '(4 2 5)
     (empty-env))))


;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env environment?))
  (recursively-extended-env-record (function-names (list-of symbol?))
                                   (idss (list-of (list-of symbol?)))
                                   (bodies (list-of expression?))
                                   (env environment?)))

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))


;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (function-names idss bodies old-env)
    (recursively-extended-env-record
     function-names idss bodies old-env)))

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'empty-env "No binding for ~s" sym))
      (extended-env-record (syms vals old-env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (apply-env old-env sym))))
      (recursively-extended-env-record (function-names idss bodies old-env)
                                       (let ((pos (list-find-position sym function-names)))
                                         (if (number? pos)
                                             (closure (list-ref idss pos)
                                                      (list-ref bodies pos)
                                                      env)
                                             (apply-env old-env sym)))))))




;****************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))


;interfaz bignum======================================================================================
(define zero (lambda () '()))
(define is-zero? (lambda (n) (null? n)))

(define successor
  (lambda (n)
    ;(write n );imprime lista
    ;(display "-\n");salto de linea 
    (letrec
     ((base 8) ;numero base
      (succerssor-aux2
       (lambda (x)
         (cond
           ((null? x) '(1)) ;si x es vacio agrega 1 (al final, recursividad)
           ;((null? x) zero)
           ((< (car x) (- base 1))(cons (+ (car x) 1) (cdr x)));si el primer elem, es menor a la base, le aumenta 1 y lo une con el resto de la lista
           (else ;para cualquier otro caso, si ese elemento es igual a base - 1, entonces  agrega 0 al inicio y aplica recursividad al resto de la lista 
            (cons 0 (succerssor-aux2 (cdr x)))
            )))))
     ;ejecucion
     (succerssor-aux2 n) )))

(define predecessor
    (lambda (n)
      ;;seguimiento en consola
      ;(write n );imprime lista
      ;(display ".\n");salto de linea 
      (letrec
        ((base 8));variable base
        ;ejecucion
        (cond
          [(null? n) (zero)];
          [(equal? n '(1)) (zero)];si el ultimo elemento es 1, entonces retorna vacio o zero
          [(zero? (car n)); si el primer elemento es 0, entonces agrega (base-1) con la recursividad sobre la lista
               (cons (- base 1)
                     (predecessor (cdr n))
               )
           ]
          [else ;para cualquier otro caso, resta 1 al elemento y lo une con el resto de la lista
               (cons (- (car n) 1)
                     (cdr n))]) )))


(define +_oct
  (lambda (x y)
    (if (is-zero? y)
        x
        (successor (+_oct (successor x) (predecessor y))))))




(define -_oct
  (lambda (x y)
    (if (is-zero? y)
        x
        (predecessor (-_oct  x (predecessor y))))))


(define *_oct
  (lambda (x y)
    (if (is-zero? x)
        (zero)
        (+_oct (*_oct (predecessor x) y) y))
    ))
    
(define potencia
  (lambda (x y)
    (if (is-zero? y)
        (successor y)
        (*_oct (potencia x (predecessor y)) x))))

(define factorial
  (lambda (n)
    (if (is-zero? n)
        (successor n)
        (*_oct n (factorial (predecessor n))))))


;==========================================================================================



(define invertir-lista (lambda (lista)
                         (if (null? lista)
                             '()
                             (append  (invertir-lista (cdr lista)) (cons (car lista) '()))
                             )))

(define octal->decimal-aux (lambda (octal inicio)
                        (if (null? octal)
                            0
                            (+(* (car octal) (expt 8 inicio )) (octal->decimal-aux (cdr octal) (+ inicio 1)))
                            )
                            ))

(define octal->decimal (lambda (octal)
                        (octal->decimal-aux (invertir-lista octal) 0)))








