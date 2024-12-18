#lang eopl

(define especificacion-lexica
  '(
    (espacio-blanco (whitespace) skip)
    (comentario ("*" (arbno (not #\newline)) "*") skip)
    (identificador (letter (arbno (or letter digit "?" "$"))) symbol)
    (numero (digit (arbno digit)) number)
    (numero ("-" (arbno digit)) number)
    (numero (digit (arbno digit)"." digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)"." digit (arbno digit)) number)
    )
  )


(define especificacion-gramatical
  '(
    (programa (expresion) a-program)
    (expresion (numero) lit-exp)
    (expresion (identificador) var-exp)
    ;; Expresiones booleanas
    (bool-expresion ("true") true-exp)
    (bool-expresion ("false") false-exp)
    (bool-oper ("not" expresion) not-exp)
    (bool-oper ("and" "(" expresion "," expresion ")") and-exp)
    (bool-oper ("or" "(" expresion "," expresion ")") or-exp)
    (bool-prim ("<" "(" expresion "," expresion ")") less-exp)
    (bool-prim (">" "(" expresion "," expresion ")") greater-exp)
    (bool-prim ("<=" "(" expresion "," expresion ")") less-equal-exp)
    (bool-prim (">=" "(" expresion "," expresion ")") greater-equal-exp)
    (bool-expresion (bool-prim "(" (separated-list expresion "," ) ")") bool-prim-exp)
    (bool-expresion (bool-oper "(" (separated-list bool-expresion "," ) ")") bool-oper-exp)

    ;; Definiciones de variables
    (expresion ("var" "(" (separated-list identificador "=" expresion ",") ")" "in" expresion "end") var-definition-exp)
    (expresion ("let" "(" (separated-list identificador "=" expresion ",") ")" "in" expresion "end") let-definition-exp)
    (expresion ("letrec" "(" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion ) ")" "in" expresion "end") letrec-definition-exp)
    (expresion ("set" identificador "=" expresion) set-exp)
    (expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)
  
    ;; Definición de una cláusula elseif
   (expresion ("if" "(" bool-expresion ")" "then" expresion (arbno "elseif" bool-expresion "then" expresion) "else" expresion "end") if-exp)
  
    (expresion ("proc" "(" (separated-list identificador ",") ")" expresion) proc-exp)
    (expresion ("apply" identificador "(" (separated-list expresion ",") ")") apply-exp)
    (expresion ("ok") ok-exp)
    
    ;; Definiciones de objetos
    (expresion ("meth" "(" identificador "," (separated-list identificador ",") ")" expresion "end") meth-exp)
    (expresion ("object" "{" (arbno identificador "=>" expresion ) "}") object-exp)
    (expresion ("for" identificador "=" expresion "to" expresion "do" expresion "end") for-exp)
    (expresion ("get" identificador "." identificador) get-exp)
    (expresion ("send" identificador "." identificador "(" (separated-list identificador ",") ")") send-exp)
    (expresion ("update" identificador "." identificador ":=" expresion) update-exp)
    (expresion ("clone" "(" expresion (separated-list identificador ",") ")") clone-exp)

    ;; Operaciones primitivas
    (expresion (primitiva "(" (separated-list expresion ",") ")") prim-exp)
    (primitiva ("+" ) add-prim)
    (primitiva ("-" ) minus-prim)
    (primitiva ("*" ) mult-prim)
    (primitiva ("is") is-prim)
    (primitiva ("%") mod-prim)
    (primitiva ("&") concat-prim)
    )
  )

;; Creamos los datatypes automaticamente
(sllgen:make-define-datatypes especificacion-lexica especificacion-gramatical)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes especificacion-lexica especificacion-gramatical)))

(define scan&parse
  (sllgen:make-string-parser especificacion-lexica especificacion-gramatical))

(define just-scan
  (sllgen:make-string-scanner especificacion-lexica especificacion-gramatical))

#|
;;Evaluar programa
(define evaluar-programa
  (lambda (pgm)
    (cases programa pgm
      (a-program (exp) (evaluar-expresion exp ambiente-inicial))
      ))
  )
|#
#|
;; ambientes
(define-datatype ambiente ambiente?
  (ambiente-vacio)
  (ambiente-extendido-ref
   (lids (list-of symbol?))
   (lvalue vector?)
   (old-env ambiente?)))

(define ambiente-extendido
  (lambda (lids lvalue old-env)
    (ambiente-extendido-ref lids (list->vector lvalue) old-env)))  

;;Implementación ambiente extendido recursivo

(define ambiente-extendido-recursivo
  (lambda (procnames lidss cuerpos old-env)
    (let
        (
         (vec-clausuras (make-vector (length procnames)))
         )
      (letrec
          (
           (amb (ambiente-extendido-ref procnames vec-clausuras old-env))
           (obtener-clausuras
            (lambda (lidss cuerpos pos)
              (cond
                [(null? lidss) amb]
                [else
                 (begin
                   (vector-set! vec-clausuras pos
                                (closure (car lidss) (car cuerpos) amb))
                   (obtener-clausuras (cdr lidss) (cdr cuerpos) (+ pos 1)))]
                )
              )
            )
           )
        (obtener-clausuras lidss cuerpos 0)
        )
      )
    )
  )

(define apply-env
  (lambda (env var)
    (deref (apply-env-ref env var))))

(define apply-env-ref
  (lambda (env var)
    (cases ambiente env
      (ambiente-vacio () (eopl:error "No se encuentra la variable " var))
      (ambiente-extendido-ref (lid vec old-env)
                              (letrec
                                  (
                                   (buscar-variable (lambda (lid vec pos)
                                                      (cond
                                                        [(null? lid) (apply-env-ref old-env var)]
                                                        [(equal? (car lid) var) (a-ref pos vec)]
                                                        [else
                                                         (buscar-variable (cdr lid) vec (+ pos 1)  )]
                                                        )
                                                      )
                                                    )
                                   )
                                (buscar-variable lid vec 0)
                                )                 
                              )
      )
    )
  )

(define ambiente-inicial
  (ambiente-extendido '(x y z) '(4 2 5)
                      (ambiente-extendido '(a b c) '(4 5 6)
                                          (ambiente-vacio))))


;; Evaluar expresión
(define evaluar-expresion
  (lambda (exp env)
    (cases expresion exp
      (lit-exp (dato) dato)
      (var-exp (var) (apply-env env id))
      ;; Booleanos
      (true-exp () #true)
      (false-exp () #false)
      (not-exp (exp)
      (let ((valor (evaluar-expresion exp env)))
        (if (boolean? valor)
            (not valor)
            (eopl:error "La expresión en 'not' no es booleana: " valor))))
      
(prim-exp (prim args)
                (let
                    (
                     (lista-numeros (map (lambda (x) (evaluar-expresion x env)) args))
                     )
                  (evaluar-primitiva prim lista-numeros)
                  )
                )
      ;; Condicionales
      (if-exp (condiciones-expresiones)
        (evaluar-condiciones condiciones-expresiones env))
    )
  )
)




(define (evaluar-condiciones cond-exp-list amb)
  (cond
    [(null? cond-exp-list)
     (error "No se proporcionó una cláusula 'else' en la expresión 'if'.")]
    [(equal? (caar cond-exp-list) 'else)
     (evaluar-expresion (cadar cond-exp-list) amb)]
    [else
     (if (evaluar-expresion (caar cond-exp-list) amb)
         (evaluar-expresion (cadar cond-exp-list) amb)
         (evaluar-condiciones (cdr cond-exp-list) amb))]))
|#
