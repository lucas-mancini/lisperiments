
(load "gps")

;;; Algunas corridas de prueba con su resultado esperado
(indicar-ruta 'a 'f)
; resultado esperado
; a - b - d - g
; Hipólito Yrigoyen - Av. Paseo Colón - destino

(indicar-ruta 'c 'h)
; resultado esperado
; c - d - f -h
; Av. Belgrano - Av. Paseo Colón - destino

(indicar-ruta 'g 'b)
; resultado esperado
; g - e - c - a - b 
; Piedras - Av. Julio A. Roca - Hipólito Yrigoyen - destino


(indicar-ruta 'd 'g)
; resultado esperado
; d - c - e -g 
; Av. Belgrano - Piedras - destino

