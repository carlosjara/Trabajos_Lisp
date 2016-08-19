; Solución de CSPs usando la técnica domain splitting
; G. Alvarez
; Septiembre 6 de 2015
; Restricciones del programa: supone que todas las restricciones son comparaciones entre números y que son binarias


; Ejemplo de prueba
(defvar cspVar '(a b c d))
(defvar cspDom '((1 2 3 4)(1 2 3 4)(1 2 3 4)(1 2 3 4)))
(defvar cspCons '((< a b)(< b c)(= c d)))

;-------------------------------

; Obtiene el valor asignado a una variable en la respuesta parcial ans
; si la variable no tiene valor asignado retorna nil
(defun getValue (var ans)
  (cond ((null ans) nil)
        ((equal (caar ans) var) (cadar ans))
        (t (getValue var (cdr ans)))))
;(defvar ans '((A 1)(B 2)(C 3)))
;(getValue 'c ans)

;-------------------------------

; Cambia las variables por sus valores actuales para poder evaluar una restricción
; si la variable no tiene valor asigna nil en esa posicion
(defun makeConst (const ans)
  (list (car const)
        (getValue (cadr const) ans)
        (getValue (caddr const) ans)))
;(makeConst (car cspCons) ans)
;(makeConst '(> B X) ans)

;-------------------------------

; Determina si una variable está asignada en la solución parcial o no
(defun varAssigned (var ans)
  (cond ((null ans) nil)
        ((equal (caar ans) var) t)
        (t (varAssigned var (cdr ans)))))
;(varAssigned 'B ans)
;(varAssigned 'X ans)

;-------------------------------

; dada una restricción determina si sus dos variables estan asignadas en la
; solucion parcial
(defun varsAssigned (const ans)
  (and (varAssigned (cadr const) ans)
       (varAssigned (caddr const) ans)))
;(varsAssigned (car cspCons) ans)
;(varsAssigned '(> B X) ans)

;-------------------------------

; determina si al asignar el valor value a la variable var se viola alguna restriccion
(defun validValue (var value const ans)
  (cond ((null const) t)
        ((varsAssigned (car const) (append ans (list (list var value)))) 
         (and (eval (makeConst (car const) 
                               (append ans (list (list var value)))))
              (validValue var value (cdr const) ans)))
        (t(validValue var value (cdr const) ans))))
;(validValue 'd 3 cspCons ans)

;-------------------------------

; soluciona un csp haciendo backtracking, es decir, haciendo una particion de dominios
; que divide el dominio en porciones de tamaño 1.
(defun domSplit (cspV cspDom cspC ans)
  (cond ((null cspV) (print ans))
        ((null (car cspDom)) nil)
        ((validValue (car cspV) (caar cspDom) cspC ans)
         (domSplit (cdr cspV)
                   (cdr cspDom) 
                   cspC 
                   (append (list (list (car cspV)(caar cspDom))) ans))
         (domSplit cspV
                   (append (list (cdar cspDom))(cdr cspDom)) 
                   cspC 
                   ans))
         (t (domSplit cspV
                     (append (list (cdar cspDom))(cdr cspDom)) 
                     cspC 
                     ans))))
;(domSplit cspVar cspDom cspCons nil)
