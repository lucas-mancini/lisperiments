;;;; --------------------------
;;;; Lenguajes Formales (75.14)
;;;; --------------------------
;;;; TP4 - Problema de las Reinas 
;;;;
;;;; Autor: Lucas Mancini (86893)

;;;; El tp consiste en implementar una función denominada "reinas", que 
;;;; recibe como parámetro N, el tamaño del tablero, y que halle todas 
;;;; las formas posibles de ubicar N reinas en un tablero de ajedrez de NxN 
;;;; sin que ninguna pieza se amenace. 

;;;; Existe solución siempre para N >= 4.
;;;; El tablero se comienza a numerar desde arriba a la izquiera, y los subíndices
;;;; comienzan desde 0. Los pares son (fila columna).

;;;; NOTA: se permite usar setq e iteración por razones de eficiencia

;;; -----------------------------
;;; --- FUNCIONES PRINCIPALES ---
;;; -----------------------------

;;; Permite conocer si dos reinas se amenazan
(defun amenaza (i j x y)
  (or 
    (= i x) ; misma fila
    (= j y) ; misma columna
    (= (+ i j) (+ x y)) ; diagonal 1
    (= (- i j) (- x y)))) ; diagonal 2

;;; Determina si la pos (i,j) es segura para colocar una reina en el tablero
(defun conflicto (i j tablero)
  (cond
    ((null tablero) NIL)
    ((amenaza i j (caar tablero) (cadar tablero)) T)
    (T (conflicto i j (cdr tablero)))))

;;; Coloca N reinas en un tablero de N x N
(defun reinas (N &optional (tablero NIL) (i 0) (j 0))
  (unless (= j N) ; si no es verdad el predicado, se ejecutan todas las sentencias del unless
    ;; verificar si la pos es válida
    (if (not (conflicto i j tablero))
      (if (= (+ 1 i) N)
        ;; se alcanzó la solución
        (mostrar-tablero (reverse (cons (list i j) tablero)))
        ;; ya hay una reina en esa fila, avanzar a la prox y empezar desde columna 0
        (reinas N (cons (list i j) tablero) (+ 1 i) 0)))
    
    ;; avanzar a la próxima columna
    (reinas N tablero i (+ 1 j)))) ; si j llega a N, la prox iteración no pasa el unless y da NIL
       
;;; ---------------
;;; --- TABLERO ---
;;; --------------- 

;;; Imprime el tablero pasado por parámetro, colocando una X
;;; en los lugares donde hay una reina
(defun mostrar-tablero (tablero)
  (format t "Solución: ~a" tablero)
  (dotimes (row (length tablero))
    (format t "~% ")
    (dotimes (column (length tablero))
      (if (= column (cadr (assoc row tablero)))
        (format t "X ")
        (format t ". "))))
    (format t "~%")
    (format t "~%~%"))

(defun mostrar-borde-horizontal (tablero)
  (dotimes (n (+ 1 (* 2 (length tablero))))
    (format t "-")))
      

