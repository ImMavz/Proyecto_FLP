#lang eopl

; Definición de estructuras de datos
;; Definición de datatypes para representar expresiones
(define-datatype program program?
  (a-program
   (exp expression?)))

(define-datatype expression expression?
  (num-exp 
   (num number?))
  
  (string-exp
   (str string?))
  
  (bool-exp
   (bool boolean?))
  
  (var-exp
   (var identifier?))
  
  (var-definition-exp
   (vars (list-of identifier?))
   (inits (list-of expression?))
   (body expression?))
  
  (let-definition-exp
   (vars (list-of identifier?))
   (inits (list-of expression?))
   (body expression?))
  
  (primitive-exp
   (prim primitive?)
   (rands (list-of expression?)))
  
  (conditional-exp
   (test boolean-expression?)
   (then-exp expression?)
   (else-exps (list-of expression?))
   (final-else-exp expression?))
  
  (proc-exp
   (vars (list-of identifier?))
   (body expression?))
  
  (apply-exp
   (rator identifier?)
   (rands (list-of expression?)))
  
  (sequence-exp
   (exps (list-of expression?)))
  
  (set-exp
   (var identifier?)
   (val expression?)))

(define-datatype boolean-expression boolean-expression?
  (bool-literal
   (bool boolean?))
  
  (bool-primitive-exp
   (prim bool-primitive?)
   (rands (list-of expression?)))
  
  (bool-oper-exp
   (oper bool-operator?)
   (rands (list-of boolean-expression?))))

;; Definición de tipos para primitivas
(define-datatype primitive primitive?
  (add-prim)
  (sub-prim)
  (mult-prim)
  (mod-prim)
  (concat-prim))

(define-datatype bool-primitive bool-primitive?
  (less-than)
  (greater-than)
  (less-equal)
  (greater-equal)
  (equal))

(define-datatype bool-operator bool-operator?
  (not-op)
  (and-op)
  (or-op))

(define (identifier? x)
  (symbol? x))


;; Especificación de la gramática
(define the-grammar
  '((program (expression) a-program)
    
    ;; Tipos de expresiones
    (expression 
     (number) num-exp
     (string) string-exp
     (boolean) bool-exp
     (identifier) var-exp
     
     ;; Definiciones de variables
     ("var" (arbno identifier "=" expression) "in" expression "end") var-definition-exp
     ("let" (arbno identifier "=" expression) "in" expression "end") let-definition-exp
     ("letrec" (arbno identifier "(" (arbno identifier) ")" "=" expression) "in" expression "end") letrec-definition-exp
     
     ;; Operaciones primitivas
     (primitive "(" (arbno expression) ")") primitive-exp
     
     ;; Operadores booleanos
     (bool-operator "(" (arbno boolean-expression) ")") bool-operator-exp
     
     ;; Condicionales
     ("if" boolean-expression "then" expression 
      (arbno "elseif" boolean-expression "then" expression) 
      "else" expression "end") conditional-exp
     
     ;; Procedimientos
     ("proc" "(" (arbno identifier) ")" expression "end") proc-exp
     ("apply" identifier "(" (arbno expression) ")") apply-exp
     
     ;; Objetos y métodos
     ("object" "{" (arbno identifier "=>" expression) "}") object-exp
     ("get" identifier "." identifier) get-exp
     ("send" identifier "." identifier "(" (arbno identifier) ")") send-exp
     ("update" identifier "." identifier ":=" expression) update-exp
     ("clone" "(" (arbno identifier) ")") clone-exp
     
     ;; Métodos
     ("meth" "(" identifier "," (arbno identifier) ")" expression "end") method-exp
     
     ;; Secuenciación
     ("begin" expression (arbno ";" expression) "end") sequence-exp
     
     ;; Iteración
     ("for" identifier "=" expression "to" expression "do" expression "end") for-exp
     
     ;; Asignación
     ("set" identifier ":=" expression) set-exp)
    
    ;; Expresiones booleanas
    (boolean-expression
     (boolean) bool-literal
     (primitive "(" (arbno expression) ")") bool-primitive-exp
     (bool-operator "(" (arbno boolean-expression) ")") bool-oper-exp)
    
    ;; Primitivas
    (primitive 
     ("+") add-prim
     ("-") sub-prim
     ("*") mult-prim
     ("%") mod-prim
     ("&") concat-prim)
    
    ;; Primitivas booleanas
    (bool-primitive 
     ("<") less-than
     (">") greater-than
     ("<=") less-equal
     (">=") greater-equal
     ("is") equal)
    
    ;; Operadores booleanos
    (bool-operator
     ("not") not-op
     ("and") and-op
     ("or") or-op))    
     )

;; Funciones para parsear según la gramática
;;(define scan&parse
  ;;(sllgen:make-string-parser the-grammar))

;; Función auxiliar para parsear texto
;;(define (parse-obliq-program text)
  ;;(scan&parse text))

;; Definición del ambiente como una lista asociativa
(define-datatype environment environment?
  (empty-env)
  (extend-env 
   (vars (list-of identifier?))
   (vals (list-of ))
   (saved-env environment?)))

;; Función para buscar una variable en el ambiente
(define (apply-env env search-var)
  (cases environment env
    (empty-env () 
      (eopl:error 'apply-env "Variable ~s no encontrada" search-var))
    (extend-env (vars vals saved-env)
      (let loop ((vars-list vars)
                 (vals-list vals))
        (cond 
          ((null? vars-list) 
           (apply-env saved-env search-var))
          ((eq? search-var (car vars-list)) 
           (car vals-list))
          (else 
           (loop (cdr vars-list) (cdr vals-list))))))))


;; Funciones auxiliares para operadores lógicos
(define (my-and . args)
  (if (null? args) 
      #t
      (let loop ((remaining args))
        (cond 
          ((null? remaining) #t)
          ((not (car remaining)) #f)
          (else (loop (cdr remaining)))))))

(define (my-or . args)
  (if (null? args)
      #f
      (let loop ((remaining args))
        (cond 
          ((null? remaining) #f)
          ((car remaining) #t)
          (else (loop (cdr remaining)))))))

(define (my-not x)
  (not x))

(define (init-env)
  (extend-env 
   '(+ - * % is < > <= >= and or not)
   (list 
    (lambda (x y) (+ x y))   ; +
    (lambda (x y) (- x y))   ; -
    (lambda (x y) (* x y))   ; *
    (lambda (x y) (remainder x y))  ; %
    (lambda (x y) (eq? x y)) ; is
    (lambda (x y) (< x y))   ; <
    (lambda (x y) (> x y))   ; >
    (lambda (x y) (<= x y))  ; <=
    (lambda (x y) (>= x y))  ; >=
    my-and    ; and
    my-or     ; or
    my-not)   ; not
   (empty-env)))
;; Función de evaluación principal
(define (eval-expression exp env)
  (cases expression exp
    (num-exp (num) num)
    
    (string-exp (str) str)
    
    (bool-exp (bool) bool)
    
    (var-exp (var) 
     (apply-env env var))
    
    (primitive-exp (prim rands)
     (let ([primitive-func (apply-env env 
                             (cases primitive prim
                               (add-prim () '+)
                               (sub-prim () '-)
                               (mult-prim () '*)
                               (mod-prim () '%)
                               (concat-prim () '&)))])
       (apply primitive-func 
              (map (lambda (rand) (eval-expression rand env)) 
                   rands))))
    
    (let-definition-exp (vars inits body)
     (let* ([evaluated-inits 
             (map (lambda (init) (eval-expression init env)) inits)]
            [new-env (extend-env vars evaluated-inits env)])
       (eval-expression body new-env)))
    
    (conditional-exp (test then-exp else-exps final-else-exp)
      (if (eval-boolean-expression test env)
          (eval-expression then-exp env)
          (let loop ((remaining-else-exps else-exps))
            (if (null? remaining-else-exps)
                (eval-expression final-else-exp env)
                (let ((current-elseif (car remaining-else-exps)))
                  (let ((elseif-test (car current-elseif))
                        (elseif-body (cadr current-elseif)))
                    (if (eval-boolean-expression elseif-test env)
                        (eval-expression elseif-body env)
                        (loop (cdr remaining-else-exps)))))))))
  
    ; Casos por implementar
    (else 
     (eopl:error 'eval-expression 
                 "Tipo de expresión no soportado: ~s" 
                 exp))))
  
;; Función para evaluar expresiones booleanas
(define (eval-boolean-expression bool-exp env)
  (cases boolean-expression bool-exp
    (bool-literal (bool) bool)
    
    (bool-primitive-exp (prim rands)
     (let ([primitive-func (apply-env env 
                             (cases bool-primitive prim
                               (less-than () '<)
                               (greater-than () '>)
                               (less-equal () '<=)
                               (greater-equal () '>=)
                               (equal () 'is)))])
       (apply primitive-func 
              (map (lambda (rand) (eval-expression rand env)) 
                   rands))))
    
    (bool-oper-exp (oper rands)
     (let ([operator-func (cases bool-operator oper
                            (not-op () my-not)
                            (and-op () my-and)
                            (or-op () my-or))])
       (if (eq? operator-func not)
           (not (eval-boolean-expression (car rands) env))
           (apply operator-func 
                  (map (lambda (rand) 
                         (eval-boolean-expression rand env)) 
                       rands)))))
    
    (else 
     (eopl:error 'eval-boolean-expression 
                 "Expresión booleana no soportada: ~s" 
                 bool-exp))))

#|
;; Función principal de interpretación
(define (interpret program)
  (cases program program
    (a-program (exp)
      (eval-expression exp (init-env)))))

; Función para crear un nuevo entorno
(define (extend-env env vars values)
  ; Lógica para extender el entorno
  )

; Funciones primitivas
(define (primitive-add x y) 
  ; Implementación de suma
  )

|#

; Ejemplo de programa
