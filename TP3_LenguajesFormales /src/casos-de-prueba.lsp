
(load "../src/interprete-c")

;;; Función usada para testear, llama a run
(defun test (prg ent)
  (format t "Entrada: ~a ~%" ent)
  (format t "Ejecutando programa..~%") 
  (format t "Salida:")
  (run prg ent)
  (format t "~%--------~%"))

; variables inicializadas
(setq programa1 '(
					(int a = 2 b = 5)
					(int c = 4)
					(main (
						  (printf a)
						  (printf b) 
						  )
					)))

; probando printf y evaluar 
; resultado: (15 89 90)
(setq programa2 '(
                    (int a = 88)
				    (main (
                          (printf 15)
                          (++ a)
                          (printf a)
                          (a ++)
                          (printf a)
                          )
                    )))

; no hay main
; resultado: NIL
(setq programa3 '(
					(int c d)
					(int e f g = 77)
					(int w = 5 x y = 3)
					(main NIL)
				 ))

; uso de scanf
; resultado: (77 5)
(setq programa4 '(
					(int c d)
					(int e f g = 77)
					(int w = 5 x y = 3)
					(main (
						  (printf g)
						  (scanf x)
						  (printf x)
						  )
					)))

; if
; resultado: (20)
(setq programa5 '(
					(int c d)
					(int a = 5 b = 10)
					(main (
						  (scanf c) ; c vale 15 ahora
						  (if (a < c)
                            ( (b = a + c)
                              (printf b) 
                            ) )
						  )
					)))

; if-else-while
; resultado: valores de "a" desde 150 a 141
(setq programa6 '(
					(int c d)
					(int a = 150 b = 10)
					(main (
						  (scanf c) ; c vale 15 ahora
						  (if (a < c) ( 
                              (b = a + c)
                              (printf b) 
                          ) else (
                            ( while (a > 140) 
                              ( (printf a)
                                (a --)) )
                          )) 
						  )
					)))

; scanf-while-printf
; resultado: (1 2 3 3 4 5 6 7 8 9 10) 
(setq programa7 '(
					(int i j)
					(int k)
					(main (
						  (scanf k) ; k vale 10 ahora
                          (i = 1)
                          (while (i < k) (
						    (printf i)
                            (i ++)
                            (if (i == 3) (
                              (printf 3)
                            ))
                              
                          ))
                          (printf k)
                          )
					)))

; asignando valores una variable
; resultado: (7 8 9 14)
(setq programa8 '(
					(int variable)
                    (int otravar)
					(main (
						  (variable = 1 + 2 * 3)
                          (printf variable)
                          (variable = 1 + 2 * 3 + 1)
                          (printf variable)
                          (variable = 1 + 2 * (3 + 1))
                          (printf variable)
                          (otravar = 5)
                          (variable += otravar)
                          (printf variable)  
						  )
					)))

; varios if y else
; resultado: (3 6 3 6 18)
(setq programa9 '(
					(int a b c)
					(main (
						  (a = 1 + 2)
                          (printf a)
                          (b = a + a)
                          (printf b)
                          (if (a < b)
                            ((printf a))
                          else
                            ((printf b))
                          )
                          (if (a > b)
                            ((printf a))
                          else
                           ((printf b))
                          )
                           (c = a * b)
                           (printf c) 
						  )
					)))

; probando operadores >, <, >=, <=
; resultado: (0 1 1 1)
(setq programa10 '(
					(int a b)
					(main (
                          (a = 1 > 2)
			              (printf a)
			              (b = a >= a)
			              (printf b)
			              (b = 2 <= 2) 
			              (printf b)
			              (b = 3 == 3) 
			              (printf b)
                          )
					)))

; probando operadores ++ y --
; resultado: (12 18 108)
(setq programa11 '(
					(int a b c h)
					(main (
                          (scanf a)
			              (scanf b)
                          (scanf c)
			              (a ++)
                          (++ a)
                          (b --)
                          (-- b)
			              (printf a)
			              (printf b)
                          (h = a * b / c) ;12 * 18 / 2
                          (printf h)
                          )
					)))

; probando operadores scanf y printf
; resultado: (12)
(setq programa12 '(
					(int a b tot)
					(main (
                          (scanf a)
			              (scanf b)
                          (printf (a + b))                         
                          )
					)))

(test programa1 NIL)
(test programa2 NIL)
(test programa3 NIL)
(test programa4 '(5))
(test programa5 '(15))
(test programa6 '(15))
(test programa7 '(10))
(test programa8 NIL)
(test programa9 NIL)
(test programa10 NIL)
(test programa11 '(10 20 2))
(test programa12 '(8 4))

