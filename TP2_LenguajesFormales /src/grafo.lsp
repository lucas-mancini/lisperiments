;;;; --------------------------
;;;; Lenguajes Formales (75.14)
;;;; --------------------------
;;;; TP2 - GPS 
;;;;
;;;; Autor: Lucas Mancini (86893)

;;; Nodos del grafo
;;; a: Hipólito Yrigoyen y Bolivar
;;; b: Hipolito Yrigoyen y Av. Paseo Colón
;;; c: Piedras y Av. Belgrano
;;; d: Av. Belgrano y Av. Paseo Colón
;;; e: Piedras y Av. Independencia
;;; f: Av. Independencia y Av. Paseo Colón
;;; g: Piedras y Av. San Juan
;;; h: Av. San Juan y Av. Paseo Colón

;;; El grafo se considera una variable global
(setq grafo 
	'( (a (b c)) (b (a d)) (c (a d e)) (d (b c f))
	   (e (c f g)) (f (d e h)) (g (e h)) (h (f g)) ))

;;; Estructura donde se almacenan las calles que representan aristas
(setq calles
	'((a b "Hipólito Yrigoyen") (b a "Hipólito Yrigoyen")
	  (a c "Av. Julio A. Roca") (c a "Av. Julio A. Roca")
	  (b d "Av. Paseo Colón") (d b "Av. Paseo Colón")
	  (c d "Av. Belgrano") (d c "Av. Belgrano")
	  (c e "Piedras") (e c "Piedras")
	  (d f "Av. Paseo Colón") (f d "Av. Paseo Colón")
	  (e f "Av. Independencia") (f e "Av. Independencia")
	  (e g "Piedras") (g e "Piedras")
	  (f h "Av. Paseo Colón") (h f "Av. Paseo Colón")
	  (g h "Av. San Juan") (h g "Av. San Juan") ))

(defun buscar-calle (nodoA nodoB calles)
  (if (null calles)
	NIL
	(if (and (equal nodoA (caar calles)) (equal nodoB (cadar calles)))
	  (caddar calles)
	  (buscar-calle nodoA nodoB (cdr calles)))))
	
(defun imprimir-calles (camino &optional (calleanterior NIL))
  (if (not (null (cdr camino)))
    (progn
	  (if (equal calleanterior (buscar-calle (car camino) (cadr camino) calles))
		(format t "") ; no imprimir nada, se sigue andando por la misma calle
	    (format t "ir por calle ~a, luego ~%" (buscar-calle (car camino) (cadr camino) calles)))
	  (imprimir-calles (cdr camino) (buscar-calle (car camino) (cadr camino) calles)))
	(format t "llegar a destino.~%~%")))

(defun traducir (camino)
  (format t "Secuencia de nodos: ~a ~%" camino)
  (imprimir-calles camino))
