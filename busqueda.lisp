; busqueda.lisp
; Autor: G. Alvarez
; Febrero 8 de 2016
; Programa de busqueda generica en un grafo representado con una lista de arcos con costo

; Ejemplo de representación del grafo de entrada.
; (x y 10) es un arco dirigido que va del vertice x al vertice y y tiene costo de 10.
(setq g1 '((x y 10)
           (y z 20)
           (x z 12)
           (z w 15)
           (w y 6)
           (w x 18)
           (y w 40)
           (x p 9)
           (p q 12)
           (q w 17)
           (p r 29)
           (q r 70)
           (q s 6)
           (r s 21)))

(setq g2 '((x y 10)
           (y z 20)
           ;(x z 12)
           (z w 15)))


; Funciones que debe definir el usuario para establecer una estrategia de búsqueda
;------------------------------------------------
(defun esObjetivo (vert)
 (equal vert 'w) 
)

(defun selecFront (front)
  (car front)
)

(defun eliminarFront (front)
  (cdr front)
)

(defun adicionarAFrontera (vecinos front)
  ;(append vecinos front)
  (append front vecinos)

)
;------------------------------------------------

; Funciones auxiliares

; elimina de la frontera los vertices que ya han sido visitados previamente
(defun elimVisitados (front visit resp)
  (cond ((null front) resp)
        ((member (car front) visit)(elimVisitados (cdr front) visit resp))
        (t (elimVisitados (cdr front) visit (append resp (list (car front)))))))

; elimina de la frontera el elemento ya procesado y adiciona sus vecinos
(defun nuevaFrontera (vecinos front visit)
  (elimVisitados (adicionarAFrontera vecinos (eliminarFront front)) visit nil)
)
; (nuevaFrontera '(a b c) '(x y z w))


; Retorna una lista con los nodos adyascentes a un vertice del grafo
(defun vecinos (vert graf)
  (cond ((null graf) nil)
        ((equal (first (car graf)) vert) 
         (cons (second (car graf)) (vecinos vert (cdr graf))))
;        ((equal (second (car graf)) vert) 
;         (cons (first (car graf)) (vecinos vert (cdr graf))))
        (t (vecinos vert (cdr graf)))))

; (vecinos 'x g1)
;------------------------------------------------

; Funcion de busqueda generica, recibe como entradas:
; graf: grafo donde se va a buscar
; front: frontera de decision. Al inicio contiene el vertice donde inicia la busqueda
; resp: parametro de salida donde se construye el camino desde el inicio hasta la respuesta
; La funcion retorna nil si no fue posible encontrar el objetivo en el grafo y 
; en caso de encontrarlo retorna el camino desde el inicio hasta el
(defun busqueda (graf front resp visit) 
  ;(print front)
  (cond ((null front) resp)
        ((esObjetivo (selecFront front))(cons (selecFront front) resp))
        (t (let ((x (selecFront front)))
             (if (member x visit)
                 (busqueda graf (nuevaFrontera (vecinos x graf) front visit) resp (cons x visit))
                 (busqueda graf (nuevaFrontera (vecinos x graf) front visit) (cons x resp) (cons x visit)))))))

;(busqueda g1 '(x) nil nil)

;_______________________________________________
; Ejercicios
; Usar el programa para encontrar diferentes nodos
; Arreglar el programa para que la respuesta no salga en orden inverso sino al derecho
; Cambiar el programa para que la búsqueda no se haga en profundidad sino en anchura
; Qué funciones se deberian cambiar para implementar el algoritmo A* a partir de este código?
; Qué funciones nuevas habría que construir para implementar el algoritmo A* a partir de este código?

;Ejercicio
(defun busqueda (graf front resp visit) 
  ;(print front)
  (cond ((null front) nil)
        ((esObjetivo (selecFront front))(cons resp (list(selecFront front))))
        (t (let ((x (selecFront front)))
             (if (member x visit)
                 (busqueda graf (nuevaFrontera (vecinos x graf) front visit) resp (cons x visit))
                 (busqueda graf (nuevaFrontera (vecinos x graf) front visit) (append resp (list x)) (cons x visit))))))
)

;
;(let ((x 19)
;	  (y 21)
;	  (z 2))
;	(+ (* x y) z)
;)

;________________________________________________
(defun CruceCj (lista1 lista2 resp1 resp2)
	(let (( lenR (random (list-length lista1)) ))
		(print lenR)
		(append 
			(cons (append (subseq lista1 0 lenR ) (nthcdr lenR lista2)) resp1)
			(cons (append ( subseq lista2 0 lenR ) (nthcdr lenR lista1)) resp2)
		)
	)
)

