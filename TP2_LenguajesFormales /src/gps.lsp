;;;; --------------------------
;;;; Lenguajes Formales (75.14)
;;;; --------------------------
;;;; TP2 - GPS 
;;;;
;;;; Autor: Lucas Mancini (86893)

;;;; El tp consiste en, dado un grafo que representa un conjunto de 
;;;; esquinas de una ciudad, determinar:
;;;;   - todos los caminos posibles para ir desde un lugar a otro
;;;;   - cuál es el camino mínimo entre todos ellos
;;;;   - imprimir el resultado en una forma amigable para el usuario 

;;;; Mapa de la región:
;;;; http://maps.google.com.ar/maps?q=Avenida+Paseo+Col%C3%B3n+850,+Ciudad+Aut%C3%B3noma+de+Buenos+Aires&hl=es&ie=UTF8&ll=-34.615533,-58.372765&spn=0.014587,0.022316&sll=-34.602711,-58.393621&sspn=0.015172,0.017831&z=16
;;;; ver archivo: mapa.png con las referencias

(load "grafo")

;;; Retorna T si el atomo pertenece a la lista, de lo contrario
;;; retorna NIL.
;;; ej: (pertenece 'a '(c a b)) -> T 
(defun pertenece (atomo lista)
  (if (null lista)
	NIL
	(if (eq atomo (car lista))
	  T
	  (pertenece atomo (cdr lista)))))

;;; Permite obtener la diferencia entre dos listas.
;;; ej: (diferencia '(a b c) '(c d a f)) -> (b) 
(defun diferencia (listaA listaB)
  (if (null listaA)
	NIL
	(if (pertenece (car listaA) listaB)
	  (diferencia (cdr listaA) listaB)
	  (cons (car listaA) (diferencia (cdr listaA) listaB)))))

;;; Devuelve la lista de vecinos del nodo pasado.
(defun vecinos (nodo grafo)
  (if (null grafo)
	NIL
	(if (eq nodo (caar grafo))
	  (nth 1 (car grafo))
	  (vecinos nodo (cdr grafo))))) 

;;; De la lista de caminos pasados, se queda con el de mínima longitud
;;; ej: (obtener-camino-minimo '((a e d f) (c d e) (a e g h c))) -> (c d e)
(defun obtener-camino-minimo (caminos)
  (if (eq 1 (length caminos))
	(car caminos)
    (if (< (length (car caminos)) (length (obtener-camino-minimo (cdr caminos))))
	  (car caminos)
	  (obtener-camino-minimo (cdr caminos)))))

;;; Función principal que calcula los caminos desde un nodo inicial
;;; a uno final.
;;; i: nodo inicial
;;; f: nodo final
;;; tray: parámetro opcional, modela la trayectoria (caminos potenciales)
;;; res: parámetro opcional, lista que almacena los resultados posibles (caminos que van desde i a f)
(defun gps (i f &optional (tray (list (list i))) (res NIL) )
  (if (null tray)
    (traducir (obtener-camino-minimo res)) ; trayectoria analizada, imprimir resultado
    (if (eq (caar tray) f) 
	  ;; se agrega el camino a la lista de caminos resultado y se sigue analizando la trayectoria
	  (gps i f (cdr tray) (cons (reverse (car tray)) res)) 
 
      ;; hay que extender la trayectoria, aun no se llegó al nodo final
	  ;; ej: si tray es ((d b a) (e b a) (c d)), se puede extender a:
	  ;; ((f d b a) (g d b a) (e b a) (c d))
	  (gps i f 
	  (append ; se descompone el primer elemento de la trayectoria y se une con la cola de la misma
	    (mapcar (lambda (x) (cons x (car tray))) (diferencia (vecinos (caar tray) grafo) (car tray))) 
		(cdr tray))
	  res))))	  

;;; Se encarga de llamar a gps (función principal) y de imprimir
;;; el resultado. 
(defun indicar-ruta (i f)
  (format t "Las indicaciones de como llegar a destino son las siguientes ~%")
  (format t "------------------------------------------------------------ ~%") 
  (gps i f)) 
