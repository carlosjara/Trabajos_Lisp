; Taller1.lisp
; Autor: J. Carlos
; Agosto 18 de 2016
; Este archivo contiene funciones del taller planteado en la clase 1 de Lisp 
; Inteligencia Artificial PUJ Cali

(defun conjuntoCj (lista)
	(cond ((null lista) nil)
		((member (car lista) (cdr lista)) (conjuntoCj (cdr lista)))
		(t(cons (car lista) (conjuntoCj (cdr lista))))
	)
)
;union de conjunto
;se asumen conjuntos
(defun unionCj (lista1 lista2) 
	(conjuntoCj (append (conjuntoCj lista1) (conjuntoCj lista2)))
)

;interseccion de conjuntos
;se asumen conjuntos los parametros
;Pasos
;al finalizar la recursion lo que devuelve lo hacemos conjunto
;ve si es miembro y llama recursivo con el resto de la lista y la lista respeusta con el repetido
(defun interseccionCj (lista1 lista2 intersec)
	(cond ((null lista1) (conjuntoCj intersec)) 
		((member (car lista1) lista2 ) (interseccionCj (cdr lista1) lista2 (cons (car lista1) intersec))) 
		(t (interseccionCj (cdr lista1) lista2 intersec))	
	)	
)

;resta de conjuntos
;Asumimos conjuntos
;(resta A B) dejo los elementos de A que no estan en B
;Pasos
;si es vacia pues vacio
;si un elemento de A es parte del conjunto B llamo recursividad con el resto del A (olvidando el elemento que esta) y B
;si no es elemento lo "recuerdo" agredandolo a la recursividad del resto de elementos en cuestion
(defun restaCj (lista1 lista2)
	(cond ((null lista1) nil)
		((member (car lista1) lista2) (restaCj (cdr lista1) lista2))
		(t (cons (car lista1) (restaCj (cdr lista1) lista2)))
	)
)

;Grafos. Encontrar Adyacencias
;param Grafo tuplas, vertice del que se quieren encontrar adyacencias
;Se asumen la tuplas (V1 V2 Cost) como arcos del vertice V1 al vertice V2 de costo Cost
;Pasos
;Se hace la recursion de tuplas y se revisa si el vertice es igual al primer Vertice de la tupla en analisis para 
;agregar el segundo elemento de la tupla al resto de la recursion
;
(defun AdyaGr (Grafo Vertice)
	(cond ((null Grafo) nil)
		((eq Vertice (car(car Grafo))) (cons (car(cdr(car Grafo))) (AdyaGr (cdr Grafo) Vertice)))
		((eq Vertice (car(cdr(car Grafo)))) (cons (car(car Grafo)) (AdyaGr (cdr Grafo) Vertice)))
		(t (AdyaGr (cdr Grafo) Vertice))
	)
)