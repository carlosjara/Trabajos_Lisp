;; Taller Lips
;; Carlos Jaramillo Aros
;; 21/09/2016

;; Definicion Taller
;; Pasar una expresion del cálculo de proposiciones a Forma Normal Conjuntiva

; ->  ~ implicacion  		~~ miImpl
; <-> ~ doblle implicacion 	~~ miDblImpl
; ¬   ~ Negacion 			~~ miNeg
; \/  ~ and					~~miAnd
; /\  ~ or					~~miOr
; =   ~ equivalencia 		~~miEquival


;funcion sin tener en cuenta el orden de los operadores (NO SIRVE) por que no es eficiente
; (defun translate (exp)
; 	(let ((Operator (car exp))
	
; 		(cond ((equal (Operator 'miDbImpl')) (Fun_miDbImpl (exp))
; 		      ((equal (Operator 'miImpl')) (Fun_miImpl (exp))
; 			  ((equal (Operator 'miNeg')) (Fun_miNeg (exp))
; 			  ((equal (Operator 'miNeg')) (Fun_miNeg (exp))
; 			  ((equal (Operator 'miOr')) (Fun_miOr (exp))
; 			) 
; 	)
; )
(defun translate (exp)
	(Fun_Distributivas 
		(Fun_Negaciones
			(Fun_Implicaciones
				(Fun_DobleImplicaciones exp))
			)
		)

	)



;Esta Función procesa la expresion que encuentra con doble implicacion, la convierte en conjuncion
;en donde sus dos operadores son implicaciones de los operadores de la implicacion 
; (miDbImpl (A)(B)) ~~ (miAnd (miImpl (A)(B)) (miImpl (B)(A)))
(defun Fun_DobleImplicaciones (exp)

	)
;Esta Función procesa la expresion que encuentra con implicacion, la convierte en disjuncion
;con el antecendente con una negacion y el consecuente tal como está.
; (miImpl (A)(B)) ~~ (miOr (miNeg (A))(B))
(defun Fun_Implicaciones (exp)
	
	(cond ((null ext) nil)
		(list miOr
			(list miNeg (car (cdr exp)) 
				(cdr (cdr exp)) 
				)
			)
	)
)
;Esta función procesa la expresion con la negacion dependiendo de el operador que se vaya a negar
; (conjuncion o disyuncion) se aplica la operacion de morgan
; (miNeg (miAnd (A)(B)))  ~~ (miOr (miNeg (A))(miNeg (B)))
; (miNeg (miOr (A)(B)))   ~~ (miAnd (miNeg (A))(miNeg (B)))
(defun Fun_Negaciones (exp) 

	)

;Esta función procesa la expresion que contenga Disyunciones, en el cual se distribuye el operador
;Disyunciones sobre los Conjunciones
; (miOr (A)(miAnd (B)(C)))  ~~ (miAnd (miOr (A)(B)) (miOr (A)(C) ) )
(defun Fun_Distributivas (exp)

	)