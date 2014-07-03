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

(load "valor")
(load "operadores")

;;; -------------------------
;;; --- MANEJO DE MEMORIA ---
;;; -------------------------

;;; Agrega el contenido de una declaración de variable a la memoria, retorna la nueva memoria
;;; ej: ((int a = 2) (int c) (int b = 5 d)) -> se agrega a la memoria (a 2 c 0 b 5 d 0) 
(defun expandir-memoria (listavars mem)
  (if (null listavars)
    mem
    (if (eq (cadr listavars) '=) 
      (expandir-memoria (cdddr listavars) (append mem (list (car listavars) (caddr listavars)))) ; variable inicializada
      (expandir-memoria (cdr listavars) (append mem (list (car listavars) 0)))))) ; variable no inicializada, se carga con cero
     
;;; Busca cuanto vale var en la memoria y retorna ese valor,
;;; si es un número, retorna directamente ese valor
(defun evaluar (var mem)
  (if (null mem)
    NIL 
    (if (eq var (car mem))
      (cadr mem)
      (evaluar var (cddr mem)))))

;;; Asigna el valor a la variable pasada guardándolo en la memoria, 
;;; se retorna la nueva memoria, si la variable no existe retorna la 
;;; misma memoria sin modificaciones
(defun asignar (var valor-nuevo mem)
  (if (null mem)
    NIL
    (if (eq var (car mem))
      (cons var (cons valor-nuevo (asignar var valor-nuevo (cddr mem))))
      (cons (car mem) (cons (cadr mem) (asignar var valor-nuevo (cddr mem)))))))

;;; Determina si la variable está o no en memoria, retornando T o NIL en caso contrario
(defun es-variable (var mem)
  (if (null mem)
    NIL
    (if (eq var (car mem))
      T
      (es-variable var (cddr mem)))))


;;; --------------------
;;; --- ASIGNACIONES ---
;;; --------------------
;;; Tipos posibles de asignaciones:
;;; 1_ (<var> = <exp>)
;;; 2_ (<var> op= <exp>)
;;; 3_ (<var> ++)
;;; 4_ (<var> --)
;;; 5_ (++ <var>)
;;; 6_ (-- <var>)

;;; Devuelve T si es una asiganción, sino NIL
(defun es-asignacion (linea mem)
  (cond
    ((es-variable (car linea) mem) T) ; casos 1,2,3 y 4
    ((eq (car linea) '++) T) ; caso 5
    ((eq (car linea) '--) T) ; caso 6
    (T NIL)))

;;; Realiza la operación de asignación y retorna la memoria con el resultado final
(defun cargar-asignacion (linea mem)
  (if (es-variable (car linea) mem)
    (cond 
        ;; caso 1
        ((eq (nth 1 linea) '=) (asignar (car linea) (valor (cddr linea) mem) mem))

        ;; caso 2: se transforma (<var> op= <exp>) en (<var> = <var> op <exp>), con una llamada recursiva 
        ((operador-traducible (nth 1 linea)) (cargar-asignacion (list (car linea) '= (car linea) (traducir-operador (nth 1 linea)) (nth 2 linea)) mem)) 

        ;; caso 3
        ((eq (nth 1 linea) '++) (cargar-asignacion (list (car linea) '= (car linea) '+ 1) mem))

        ;; caso 4
        ((eq (nth 1 linea) '--) (cargar-asignacion (list (car linea) '= (car linea) '- 1) mem))
    )
    (cargar-asignacion (reverse linea) mem))) ; transforar el caso 5 en 3 y 6 en 4
      
 
;;; ----------
;;; --- IF ---
;;; ----------

;;; Encapsula la lógica de ver que líneas colocar delante de prg para resolver el if 
(defun ejecutar-if (prg ent mem sal)
  (if (not (eq (valor (nth 1 (car prg)) mem) 0)) 
    (ejecutar (append (nth 2 (car prg)) (cdr prg)) ent mem sal) ; T, se ejecuta lo que viene después del if
    (if (eq (length (car prg)) 5)
      (ejecutar (append (nth 4 (car prg)) (cdr prg)) ent mem sal) ; F, pero hay else, hay que ejecutar mas lineas
      (ejecutar (cdr prg) ent mem sal)))) ; F, pero no hay else, no se hace nada


;;; -------------
;;; --- WHILE ---
;;; -------------

;;; Encapsula la lógica de ver que líneas colocar delante de prg para resolver el while
(defun ejecutar-while (prg ent mem sal)
  (if (eq (valor (nth 1 (car prg)) mem) 0)
    (ejecutar (cdr prg) ent mem sal) ; F, no se hace nada
    (ejecutar (append (nth 2 (car prg)) prg) ent mem sal))) ; T, ejecutar lo que viene después del while, antes de prg


;;; -----------------------------
;;; --- FUNCIONES PRINCIPALES ---
;;; -----------------------------

;;; Función que se encarga de ejecutar el main
;;; prg: lista con las sentencias del programa
;;; ent: entrada de datos
;;; mem: memoria con las variables ya cargadas
;;; sal: parámetro opcional, representa la salida (lo que va imprimiendo el programa)
(defun ejecutar (prg ent mem &optional (sal NIL))
  (if (null prg)
    (print sal)
    (cond 
      ((eq (caar prg) 'printf) (ejecutar (cdr prg) ent mem (append sal (list (valor (cadar prg) mem))))) ; printf modifica la salida
      ((eq (caar prg) 'scanf) (ejecutar (cdr prg) (cdr ent) (asignar (nth 1 (car prg)) (car ent) mem) sal)) ; scanf lee de la entrada 
      ((es-asignacion (car prg) mem) (ejecutar (cdr prg) ent (cargar-asignacion (car prg) mem) sal))
      ((eq (caar prg) 'if) (ejecutar-if prg ent mem sal)) 
      ((eq (caar prg) 'while) (ejecutar-while prg ent mem sal))
      (T (format t "ERROR: sentencia no soportada~%")))))

;;; Función que corre un programa en pseudo C, punto de entrada 
;;; prg: lista con el programa a correr
;;; ent: entrada de datos
;;; mem: parámetro opcional, representa las variables que están en memoria
(defun run (prg ent &optional (mem NIL))
  (if (null prg)
    NIL
    (if (eq (caar prg) 'int) 
      (run (cdr prg) ent (expandir-memoria (cdar prg) mem)) ; agregar variable a memoria
      (if (eq (caar prg) 'main)
        (ejecutar (nth 1 (car prg)) ent mem) ; ejecutar main
        (format t "ERROR: no hay main~%")))))
  
 
