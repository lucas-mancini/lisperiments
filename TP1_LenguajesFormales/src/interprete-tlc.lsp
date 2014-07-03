;;;; --------------------------
;;;; Lenguajes Formales (75.14)
;;;; --------------------------
;;;; TP1 - Intérprete de TLC-Lisp en X-LISP
;;;;
;;;; Autor: Lucas Mancini (86893)

;;;; Diferencias de TLC-Lisp con X-Lisp:
;;;;   - los ambientes funcionan de forma distinta
;;;;   - nth es diferente (recibe parámetros en otro orden)

;;; Permite buscar el valor del símbolo pasado en el ambiente.
;;; Si el símbolo no se encuentra retorna nil.
;;; El ambiente se representa con una lista de simbolos y valores.
;;; ej de ambiente: (a 4 b 8 var 12) implica a=4, b=8, var=12
(defun buscar-en-ambiente (simbolo amb)
  (if (null amb)
    NIL
    (if (equal simbolo (car amb))
      (cadr amb)
      (buscar-en-ambiente simbolo (cddr amb)))))

;;; Extiende el ambiente pasado, mediante los simbolos y valores indicados.
;;; ej: (extender-ambiente '(x y) '(2 4) '(a 1 b 3)) -> (x 2 y 4 a 1 b 3)
(defun extender-ambiente (simbolos valores amb)
  (if (null simbolos)
    amb
	(extender-ambiente (cdr simbolos) (cdr valores) (cons (car simbolos) (cons (car valores) amb)))))

;;; "aplicar" aplica la función pasada por parámetro, denominada func
;;; sobre la lista de argumentos ya evaluados, args.
;;; ej: (aplicar 'cons '(a (b c)) 'amb) -> (a b c)
(defun aplicar (func args amb)
  (if (atom func)
	(cond
	  ((equal func 'car) (caar args))
	  ((equal func 'cdr) (cdar args))
	  ;; el indice del nth en TLC LISP va al final, y se comienza a contar desde 1
	  ((equal func 'nth) (nth (- (cadr args) 1) (car args)))
	  ((equal func 'cons) (cons (car args) (cadr args)))
	  ((equal func 'list) args) 
	  ((equal func 'append) (append (car args) (cadr args)))

	  ((equal func 'atom) (atom (car args)))
	  ((equal func 'numberp) (numberp (car args)))
	  ((equal func 'listp) (listp (car args)))
	  ((equal func 'symbolp) (symbolp (car args)))
	  ((equal func 'null) (null (car args)))
	  ((equal func 'length) (length (car args)))

	  ((equal func 'eq) (eq (car args) (cadr args)))
	  ((equal func '+) (+ (car args) (cadr args)))
	  ((equal func '-) (- (car args) (cadr args)))
	  ((equal func '*) (* (car args) (cadr args)))
	  ((equal func '/) (/ (car args) (cadr args)))
	  ((equal func '>) (> (car args) (cadr args)))
	  ((equal func '<) (< (car args) (cadr args)))

	  ((equal func 'apply) (apply (car args) (cadr args)))
	  ((equal func 'mapcar) (mapcar (car args) (cadr args)))

	  ;; la función no es una primitiva, hay que buscar la definición en el ambiente
	  ((buscar-en-ambiente func amb) (aplicar (buscar-en-ambiente func amb) args amb) )
	  (T "función no definida"))
	; func es algo del estilo (lambda (..) (..)) , no es un átomo
	(evaluar (nth 2 func) (extender-ambiente (nth 1 func) args amb))))

;;; "evaluar" evalúa una expresión, es la base del intérprete.
;;; Recibe como parámetros la expresión a evaluar y un ambiente 
;;; representado por una lista donde se buscan a qué valores
;;; están asociados los símbolos.
;;; ej: (evaluar '(cons atomo '(b c)) '(atomo a otroatomo d)) -> (a b c)
(defun evaluar (exp amb)
  (if (atom exp)
    (if (or (numberp exp) (equal exp T)) 
      exp ;exp es número o T, devolver eso
      (buscar-en-ambiente exp amb)) ;exp es símbolo
    (cond ;exp es una lista
	  ;; algunas funciones no requieren evaluar todos los parámetros 
      ((equal (car exp) 'quote) (cadr exp))
      ((equal (car exp) 'and) (if (evaluar (cadr exp) amb)
                                (evaluar (caddr exp) amb)
                                NIL))
	  ((equal (car exp) 'or) (if (evaluar (cadr exp) amb)
								T
								(evaluar (caddr exp) amb)))
	  ((equal (car exp) 'not) (if (evaluar (cadr exp) amb)
								NIL
								T))
	  ((equal (car exp) 'if) (if (evaluar (cadr exp) amb)
								(evaluar (nth 2 exp) amb)
								(evaluar (nth 3 exp) amb)))
	  ((equal (car exp) 'cond) (if (evaluar (car (nth 1 exp)) amb)
								(evaluar (cadr (nth 1 exp)) amb)
								(evaluar (cons 'cond (cddr exp)) amb)))
	  ;((equal (car exp) 'setq) (extender-ambiente (list (cadr exp)) (list (evaluar (caddr exp) amb)) amb))
	  ;; el primer elemento de la lista es una función, hay que aplicarla a los argumentos evaluados	
	  (T (aplicar (car exp) (mapcar (lambda (arg) (evaluar arg amb)) (cdr exp)) amb) ))))

;;; para usar el programa como un intérprete interactivo
;;; el ambiente se considera NIL en este caso
(defun repl ()
	(loop (format t ">> ~a ~%" (evaluar (read) NIL))))
