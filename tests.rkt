#lang racket

#|Samuel Escobar Rivera - 2266363

César David Peñaranda Melo - 2266265

Joseph Herrera Libreros - 2266309

Juan David Cuellar Lopez - 2266087
|#

(require rackunit "ProyectoPrueba.rkt")

(define let-exp1
  (scan&parse "let x = 5 in x end"))

(define let-expect1
5)

(define let-exp2
  (scan&parse "let x = 5 in let y = 3 in +(x, y) end end"))

(define let-expect2
8)

(define let-exp3
  (scan&parse "let x = 5 in let y = 3 in let z = 2 in +(+(x, y), z) end end end"))

(define let-expect3
10)

(define begin-exp1
  (scan&parse "begin let x = 5 in x end; let x = 3 in x end end"))

(define begin-expect1
3)

(define begin-exp2
  (scan&parse "begin let x = 5 in let y = 3 in +(x, y) end end; let x = 3 in x end end"))

(define begin-expect2
3)

(define exp1
  (scan&parse "object {x => 5}")) ;; esto se representa como un structur de un objeto por lo tengo falla debido a que no se puede comparar un structur con una lista

(define expect1
    (list (cons 'x 5))) ;; #(x 5)

(define exp2
  (scan&parse "object {x => 5 y => 3}"))

(define expect2
    (list (cons 'x 5) (cons 'y 3)))

(define exp3
  (scan&parse "if >(5, 3) then 1 else 0 end"))

(define expect3
    1)

(define exp4
  (scan&parse "let x = 2, y = 3 in if >(x, y) then 1 else 0 end end"))

(define expect4
    0)

(define exp5
(scan&parse "if <(5, 3) then 1 else 0 end"))

(define expect5
    0)

(define begin-exp3
  (scan&parse "begin let x = 5, y = 3 in if is(x, y) then 1 else 0 end end end"))

(define begin-expect3
0)

(define var-exp1
  (scan&parse "var x = 5 in begin let y = +(x, 4) in y end end end"))

(define var-expect1
9)

(define for-exp4
  (scan&parse "for x = 1 to 5 do x end")) ;; esto retorna un ok 

(define for-expect4
'ok)


(define test-list-functions
   (test-suite "Test de funciones"

    (check-equal? (evaluar-programa let-exp1) let-expect1)  
    (check-equal? (evaluar-programa let-exp2) let-expect2)
    (check-equal? (evaluar-programa let-exp3) let-expect3)
    (check-equal? (evaluar-programa begin-exp1) begin-expect1)
    (check-equal? (evaluar-programa begin-exp2) begin-expect2)
    (check-equal? (evaluar-programa exp1) expect1)
    (check-equal? (evaluar-programa exp2) expect2)
    (check-equal? (evaluar-programa exp3) expect3)
    (check-equal? (evaluar-programa exp4) expect4)
    (check-equal? (evaluar-programa exp5) expect5)
    (check-equal? (evaluar-programa begin-exp3) begin-expect3)
    (check-equal? (evaluar-programa var-exp1) var-expect1)
    (check-equal? (evaluar-programa for-exp4) for-expect4)
    
    
    )
)



(run-test test-list-functions)
