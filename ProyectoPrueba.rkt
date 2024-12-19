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
      (bool-prim ("<") less-prim)
      (bool-prim (">") greater-prim)
      (bool-prim ("<=") less-equal-prim)
      (bool-prim (">=") greater-equal-prim)
      (bool-oper ("not") not-oper)
      (bool-oper ("and") and-oper)
      (bool-oper ("or") or-oper)
      (bool-prim ("is") is-prim)
      ;; Definiciones de bool-expresion
      (bool-expresion ("true") true-exp)
      (bool-expresion ("false") false-exp)
      (bool-expresion (bool-prim "(" (separated-list expresion ",") ")") bool-prim-exp)
      (bool-expresion (bool-oper "(" (separated-list bool-expresion ",") ")") bool-oper-exp)
      ;; Definiciones de variables
      (expresion ("var"  (separated-list identificador "=" expresion ",") "in" expresion "end") var-definition-exp)
      (expresion ("let"  (separated-list identificador "=" expresion ",")  "in" expresion "end") let-definition-exp)
      (expresion ("letrec"  (arbno identificador "(" (separated-list identificador ",") "=" expresion ) ")" "in" expresion "end") letrec-definition-exp)
      (expresion ("set" identificador "=" expresion) set-exp)
      (expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)
    
      ;; Definición de una cláusula if-elseif
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


  ;;Evaluar programa
  (define evaluar-programa
    (lambda (pgm)
      (cases programa pgm
        (a-program (exp) (evaluar-expresion exp ambiente-inicial))
        ))
    )


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


;; Definición de la función make-object
(define make-object
  (lambda (field-names field-values)
    (vector field-names field-values)))

;; Definición de la función object-get-field
(define object-get-field
  (lambda (obj field-id)
    (let ((field-names (vector-ref obj 0))
          (field-values (vector-ref obj 1)))
      (let loop ((names field-names)
                 (values field-values))
        (if (null? names)
            (eopl:error "Campo no encontrado: " field-id)
            (if (equal? (car names) field-id)
                (car values)
                (loop (cdr names) (cdr values))))))))

;; Definición de la función object-set-field!
(define object-set-field!
  (lambda (obj field-id new-val)
    (let ((field-names (vector-ref obj 0))
          (field-values (vector-ref obj 1)))
      (let loop ((names field-names)
                 (values field-values))
        (if (null? names)
            (eopl:error "Campo no encontrado: " field-id)
            (if (equal? (car names) field-id)
                (begin
                  (vector-set! field-values (vector-length field-names) new-val)
                  'ok)
                (loop (cdr names) (cdr values))))))))

;; Evaluar expresión
;; Función auxiliar para evaluar expresiones booleanas
(define evaluar-bool-expresion
  (lambda (exp env)
    (cases bool-expresion exp
      (true-exp () #true)
      (false-exp () #false)
      (bool-prim-exp (prim args)
                     (let ((lista-vals (map (lambda (x) (evaluar-expresion x env)) args)))
                       (evaluar-bool-primitiva prim lista-vals)))
      (bool-oper-exp (oper args)
                     (let ((lista-vals (map (lambda (x) (evaluar-bool-expresion x env)) args)))
                       (evaluar-bool-operacion oper lista-vals))))))

(define evaluar-bool-operacion
    (lambda (operador lista-bool)
        (cases bool-oper operador
            (not-oper () (not (car lista-bool)))
            (and-oper () (and (car lista-bool) (cadr lista-bool)))
            (or-oper () (or (car lista-bool) (cadr lista-bool)))
            (else (eopl:error "Operador booleano desconocido" operador))
        )
    )
)


(define evaluar-primitiva-booleano
  (lambda (prim lista-valores)
    (cases bool-prim prim
      (greater-prim () (> (car lista-valores) (cadr lista-valores)))
      (greater-equal-prim () (>= (car lista-valores) (cadr lista-valores)))
      (less-prim () (< (car lista-valores) (cadr lista-valores)))
      (less-equal-prim () (<= (car lista-valores) (cadr lista-valores)))
      (is-prim () (equal? (car lista-valores) (cadr lista-valores)))
      (else (eopl:error "Primitiva booleana desconocida" prim))))
) 

;; Evaluador principal de expresiones
(define evaluar-expresion
  (lambda (exp env)
    (cases expresion exp
      ;; Expresiones básicas
      (lit-exp (dato) dato)
      (var-exp (var) (apply-env env var))
      
      ;; Expresiones primitivas
      (prim-exp (prim args)
                (let ((lista-numeros (map (lambda (x) (evaluar-expresion x env)) args)))
                  (evaluar-primitiva prim lista-numeros)))
      
      ;; Condicionales
      (if-exp (condition then-exp elseif-conds elseif-exps else-exp)
        (if (evaluar-expresion condition env)
            (evaluar-expresion then-exp env)
            (evaluar-elseifs elseif-conds elseif-exps else-exp env)))
      
      ;; Definiciones
      (var-definition-exp (ids exps cuerpo)
                         (let* ((valores (map (lambda (e) (evaluar-expresion e env)) exps))
                               (nuevo-amb (ambiente-extendido ids valores env)))
                           (evaluar-expresion cuerpo nuevo-amb)))
      
      (let-definition-exp (ids exps cuerpo)
                         (let* ((valores (map (lambda (e) (evaluar-expresion e env)) exps))
                               (nuevo-entorno (ambiente-extendido ids valores env)))
                           (evaluar-expresion cuerpo nuevo-entorno)))
      
      (letrec-definition-exp (proc-names param-lists proc-bodies letrec-body)
                            (let ((nuevo-env (ambiente-extendido-recursivo 
                                            proc-names param-lists proc-bodies env)))
                              (evaluar-expresion letrec-body nuevo-env)))
      
      ;; Asignación y secuenciación
      (set-exp (id exp)
               (begin
                 (vector-set! (apply-env-ref env id)
                         (evaluar-expresion exp env))
                 'ok))
      
      (begin-exp (exp1 exps)
                 (let loop ((exp1 exp1)
                           (exps exps))
                   (if (null? exps)
                       (evaluar-expresion exp1 env)
                       (begin
                         (evaluar-expresion exp1 env)
                         (loop (car exps) (cdr exps))))))
      
      ;; Procedimientos
      (proc-exp (ids body)
                (closure ids body env))
      
      (apply-exp (proc-id args)
                 (let* ((proc (apply-env env proc-id))
                        (args-vals (map (lambda (arg) 
                                        (evaluar-expresion arg env)) 
                                      args)))
                   (if (procval? proc)
                       (cases procval proc
                         (closure (ids body saved-env)
                                 (evaluar-expresion body
                                                  (ambiente-extendido ids args-vals saved-env))))
                       (eopl:error "No es un procedimiento: " proc))))
      
      ;; Valores básicos
      (ok-exp () 'ok)

      
      
      ;; Objetos y métodos
      (meth-exp (self-id params body)
                (closure (cons self-id params) body env))
      
      (object-exp (field-names field-exprs)
                  (let ((field-values (map (lambda (expr) 
                                           (evaluar-expresion expr env))
                                         field-exprs)))
                    (make-object field-names field-values)))
      
      ;; Ciclo for
      (for-exp (var init-exp final-exp body)
               (let ((init-val (evaluar-expresion init-exp env))
                     (final-val (evaluar-expresion final-exp env)))
                 (let loop ((i init-val))
                   (if (> i final-val)
                       'ok
                       (begin
                         (evaluar-expresion body 
                                          (ambiente-extendido (list var) 
                                                            (list i) 
                                                            env))
                         (loop (+ i 1)))))))
      
      ;; Operaciones con objetos
      (get-exp (obj-id field-id)
               (let ((obj (apply-env env obj-id)))
                 (object-get-field obj field-id)))
      
      (send-exp (obj-id method-id args)
                (let* ((obj (apply-env env obj-id))
                       (method (object-get-field obj method-id))
                       (arg-vals (map (lambda (arg) 
                                      (evaluar-expresion arg env))
                                    args)))
                  (if (procval? method)
                      (cases procval method
                        (closure (ids body saved-env)
                                (evaluar-expresion body
                                                 (ambiente-extendido ids 
                                                                   (cons obj arg-vals)
                                                                   saved-env))))
                      (eopl:error "No es un método: " method))))
      
      (update-exp (obj-id field-id new-exp)
                  (let* ((obj (apply-env env obj-id))
                         (new-val (evaluar-expresion new-exp env)))
                    (object-set-field! obj field-id new-val)
                    'ok))
      
      (clone-exp (source-exp field-names)
                 (let ((source-obj (evaluar-expresion source-exp env)))
                                     (object-clone source-obj field-names))))))
                  
                  ;; Definición de la función object-clone
                  (define object-clone
                    (lambda (source-obj field-names)
                      (let ((source-field-names (vector-ref source-obj 0))
                            (source-field-values (vector-ref source-obj 1)))
                        (let loop ((names source-field-names)
                                   (values source-field-values)
                                   (new-names '())
                                   (new-values '()))
                          (if (null? names)
                              (make-object (reverse new-names) (reverse new-values))
                              (if (member (car names) field-names)
                                  (loop (cdr names) (cdr values) new-names new-values)
                                  (loop (cdr names) (cdr values) (cons (car names) new-names) (cons (car values) new-values))))))))
    
;; Evaluador de primitivas
(define evaluar-primitiva
  (lambda (prim args)
    (cases primitiva prim
      (add-prim () (apply + args))
      (minus-prim () (apply - args))
      (mult-prim () (apply * args))
      (mod-prim () (apply modulo args))
      (concat-prim () (apply string-append args)))))

;; Evaluador de primitivas booleanas
(define evaluar-bool-primitiva
  (lambda (prim args)
    (cases bool-prim prim
      (less-prim () (apply < args))
      (greater-prim () (apply > args))
      (less-equal-prim () (apply <= args))
      (is-prim () (apply equal? args))
      (greater-equal-prim () (apply >= args)))))

;; Evaluador de operadores booleanos


(define evaluar-elseifs
  (lambda (conditions expressions else-exp env)
    (cond
      [(null? conditions) 
       (evaluar-expresion else-exp env)]
      [(evaluar-expresion (car conditions) env)
       (evaluar-expresion (car expressions) env)]
      [else
       (evaluar-elseifs (cdr conditions) (cdr expressions) else-exp env)])))

;;Referencias

(define-datatype referencia referencia?
  (a-ref (pos number?)
         (vec vector?)))


(define-datatype procval procval?
  (closure (lid (list-of symbol?))
           (body expresion?)
           (amb-creation ambiente?)))

;;Extractor de referencias
(define deref
  (lambda (ref)
    (primitiva-deref ref)))

(define primitiva-deref
  (lambda (ref)
    (cases referencia ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define interpretador
  (sllgen:make-rep-loop "-->" evaluar-programa
                        (sllgen:make-stream-parser
                         especificacion-lexica especificacion-gramatical)))
(interpretador)