
(load "../src/interprete-tlc")

;;; Función usada para testear, llama a evaluar
(defun test (exp amb)
  (format t "Exp: ~a ~%" exp)
  (format t "Amb: ~a ~%" amb)
  (format t "Resultado: ~a ~%" (evaluar exp amb))
  (format t "--------~%"))

; átomo que es un numero
(test 5 '(w 4 func 85 b 8 z 22))

; átomo que es un símbolo
(test 'b '(w 4 func 85 b 8 z 22))

; uso de quote
(test '(quote (x y z)) '(a 5 b 4))

; uso de and
(test '(and T T) '(a 5 b 4))
(test '(and NIL T) '(a 5 b 4))
(test '(and T NIL) '(a 5 b 4))
(test '(and NIL NIL) '(a 5 b 4))

; uso de or
(test '(or T T) '(a 5 b 4))
(test '(or NIL T) '(a 5 b 4))
(test '(or T NIL) '(a 5 b 4))
(test '(or NIL NIL) '(a 5 b 4))

; uso de if
(test '(if (and T NIL)
		 5
		 b)
'(b 12 a 9))

; uso de cond
(test '(cond
		 ((or T NIL) (quote (a b c)))
		 ((and T T) 88)
	 	 ((T simboloB))) 
'(simboloA 59 simboloB 42))

(test '(cond
		 ((or NIL NIL) (quote (a b c)))
		 ((and T T) 88)
	 	 ((T simboloB))) 
'(simboloA 59 simboloB 42))

(test '(cond
		 ((or NIL NIL) (quote (a b c)))
		 ((and NIL T) 88)
	 	 (T simboloB)) 
'(simboloA 59 simboloB 42))

; uso de nth 
(test '(nth '(5 8 9 3 2) 3) '(w 4 func 85 b 8))
(test '(nth '(a e i o u) 2) NIL)
(test '(nth lista 5) '(lista (24 52 33 66 22 88 77)))

; car y cdr
(test '(cdr '(5 8 7 5)) '(a 1 b 2 c 3))
(test '(car '(1 2 3)) '(a 1 b 2 c 3))
(test '(car notas) '(notas (8 9 10) letras (a b c)))

; cons
(test '(cons 'a '(b c d e)) '(w 1 z 5))
(test '(cons '(-8 -10) numeros-negativos) '(numeros-negativos ((-4 -5 -1))))
(test '(cons 22 (cons 33 (cons 44 '(55 66 77)))) NIL)

; list
(test '(list 'a 'b 'c d) '(e 22 d 4))
(test '(list 'a (car '(b w z)) 'c d) '(e 22 d 4))

; append
(test '(append '(2 3 4) otralista) '(unalista (a b c) otralista (5 6 7)))

; atom
(test '(atom x) '(y 25 x 24 z 84))
(test '(atom (cdr '(w a v))) NIL)

; numberp
(test '(numberp 10) NIL)
(test '(numberp (nth '(10 20 30 40) n)) '(n 3) )

; symbolp, listp
(test '(listp '(a b c)) NIL)
(test '(symbolp un-simbolo) '(un-simbolo X))

; length
(test '(length '(1 2 3 4)) NIL)

; eq
(test '(eq (car '(w x z)) (car '(w a b))) NIL)

; operadores matemáticos
(test '(+ 1 (- 20 (* 4 4))) NIL) ; = 5
(test '(+ (* a b) (/ c d)) '(a 2 b 4 c 10 d 2)) ; = 13 

; mapcar
(test '(mapcar 'atom '(2 4 '(a b) '(c d))) NIL)

; lambda
(test '((lambda (x) (+ x 1)) 5) '(a 2))
(test '((lambda (x) (* x 2)) b) '(a 2 b 4))

; si existen funciones ya definidas en el ambiente
(test '(unafuncion 5) '(unafuncion (lambda (x) (+ x 1)) otrafuncion (lambda (y) (* y 3))))
