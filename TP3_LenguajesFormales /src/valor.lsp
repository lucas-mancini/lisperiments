;;;; --------------------------
;;;; Lenguajes Formales (75.14)
;;;; --------------------------
;;;; TP3 - Intérprete de pseudo C 
;;;;
;;;; Autor: Lucas Mancini (86893)

;;;; El tp consiste en ejecutar un programa en C, que viene precargado. La sintaxis
;;;; ya viene en forma de lista, no es necesario implementar el parseado del código.
;;;;
;;;; El diseño a utilizar queda definido por lo siguiente:
;;;;   - una lista para representar la entrada de datos
;;;;   - una lista para representar la salida de datos
;;;;   - una estructura en forma de lista que representa el contenido de la memoria, ej (varA 0 dato 5)
;;;;
;;;; Ver archivo spec.txt con los requerimientos

;;; -------------------------
;;; --- VALOR ---------------
;;; -------------------------

(defun valor (expr mem &optional (operadores NIL) (operandos NIL))
  (if (atom expr)

    ;; expr es átomo
    (if (not (null expr))
      (if (numberp expr)
        (filtrar-salida expr) ; expr es un número
        (if (es-variable expr mem) 
          (filtrar-salida (evaluar expr mem)) ; expr es una variable, buscarla en memoria 
          (format t "ERROR: no se puede evaluar ~a, la variable no está en memoria~%" expr))) ; expr no existe
      
      ;; expr es (), hay que aplicar operadores 
      (if (null operadores)
        ;; devolver resultado, está en operandos
        (filtrar-salida (car operandos)) 
        ;; aplicar primer operador sobre los dos primeros operandos
        (valor expr mem (cdr operadores) (cons (apply (lispificar-op (car operadores)) (list (valor (nth 1 operandos) mem) (valor (nth 0 operandos) mem))) (cddr operandos)))))
    
    ;; expr no es átomo, es una lista, hay que ver si el primer elemento es un operador o un operando y moverlo a la lista correspondiente
    (if (es-operador (car expr))
      ;; si el primero de los operadores tiene menor peso que el operador a apilar, se apila el nuevo operador, sino hay que operar antes 
      (if (null operadores)
        (valor (cdr expr) mem (cons (car expr) operadores) operandos)
        (if (< (peso (car operadores)) (peso (car expr)))
          (valor (cdr expr) mem (cons (car expr) operadores) operandos) ; apilar operador
          (valor (cdr expr) mem (cons (car expr) (cdr operadores)) (cons (apply (lispificar-op (car operadores))
                    (list (valor (nth 1 operandos) mem) (valor (nth 0 operandos) mem))) (cddr operandos)))))
          
      ;; no es operador, hay que apilar el operando
      (valor (cdr expr) mem operadores (cons (car expr) operandos)))))

;;; Transforma los valores de verdad de Lisp a C
(defun filtrar-salida (resultado)
  (cond 
    ((eq resultado T) 1)
    ((eq resultado NIL) 0)
    (T resultado)))

#|
Ejemplos de como funciona "valor".

1_
operadores          operandos            expr
----------          ---------           ------
    ()                  ()            (a + b * c)
    ()                 (a)             (+ b * c)
   (+)                 (a)             (b * c)
   (+)                (a b)             (* c)
  (* +)               (a b)              (c)
  (* +)              (c b a)             ()
   (+)               (b*c a)             ()
    ()             ( a+(b*c) )           ()
                    resultado
2_
operadores          operandos           expr
----------          ---------          ------
    ()                  ()            (a * b + c)
    ()                 (a)             (* b + c)
   (*)                 (a)             (b + c)
   (*)                (b a)             (+ c)
   (+)               ( a*b )             (c)  <-- se aplica * porque tiene mayor peso
   (+)               (c a*b)             ()
    ()             ( (a*b)+c )           ()
                    resultado
|#
