;TallerClase.lisp
;Autor: C. Jaramillo
;Agosto 19 2016
;Programa que realiza el cruce genetico de dos individuos
;Parametros:
;Lista 1 -- Individuo 1
;Lista 2 -- Individuo 2
;Resp 1 -- Creacion del nuevo (Cruzado) individuo 1
;Resp 2 -- Creacion del nuevo (Cruzado) individuo 2
;
;Muestra una lista con dos listas ((Resp1)(Resp2))

(defun CruceCj (lista1 lista2 resp1 resp2)
	(let (( lenR (random (list-length lista1)) ))
		;(print lenR)
		(append 
			(cons (append (subseq lista1 0 lenR ) (nthcdr lenR lista2)) resp1)
			(cons (append (subseq lista2 0 lenR ) (nthcdr lenR lista1)) resp2)
		)
	)
)

;(crucecj '(2 3 5 6 4 2) '(4 6 34 7 3 7) () ())