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
;;; --- OPERADORES ----------
;;; -------------------------

;;; Variable global donde se almacena la lista de operadores para traducir
(setq operadores '(+= + -= - *= * /= /))

;;; Traduce de operadores usando la lista pasada
(defun traducir-operador (op &optional (lista-ops operadores))
  (if (null lista-ops)
    NIL
    (if (eq op (car lista-ops))
      (cadr lista-ops)
      (traducir-operador op (cddr lista-ops)))))

;;; Retorna T si hay que convertir el operador en otro
(defun operador-traducible (op)
  (cond
    ((eq op '+=) T)
    ((eq op '-=) T)
    ((eq op '*=) T)
    ((eq op '/=) T)
    (T NIL)))

;;; Retorna T si es un operador válido sobre el cual se puede hacer apply
(defun es-operador (op)
  (cond
    ((eq op '+) T)
    ((eq op '-) T)
    ((eq op '*) T)
    ((eq op '/) T)
    ((eq op '>) T)
    ((eq op '<) T)
    ((eq op '>=) T)
    ((eq op '<=) T)
    ((eq op '==) T)
    (T NIL)))

;;; Devuelve un entero que representa el peso del operador
(defun peso (op)
  (case op
    ('+ 2)
    ('- 2)
    ('* 3)
    ('/ 3)
    ('> 1) 
    ('< 1)
    ('>= 1)
    ('<= 1)
    ('== 1)
    (T 0)))

;;; Transforma el operador de C al usado en Lisp 
(defun lispificar-op (op)
  (cond 
    ((eq op '==) 'equal)
    (T op))) 
