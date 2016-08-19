;clase 2 lisp
;funciones de alto nivel

;
(mapcar #'+ '(1 2 3) '(4 5 6))
;
(mapcar (lambda (num) (* num num) '(1 2 3 4 5)))

;
(reduce '+ (mapcar (lambda (num) (* num num) '(1 2 3 4 5))))

;Ejemplo uso de mapcar
(defun maximo (lista)
  (cond ((null lista) 0)
        ((> (car lista)(maximo (cdr lista))) (car lista))
        (t (maximo (cdr lista))))
)
;(mapcar 'maximo '((1 2 3)(4 5 6)(7 8 9)))

;otro ejemplo
(mapcar #'cons '(a b c d) '( () (p p) (n n) ()))

;Manejo de defincion de variables y funciones globales
(flet ((add-something (num)
			(+ num 17))))
(let ((add-something (lambda (number)
						(+ (number 30))))))

(mapcar #'add-something '(1 2 3))
(mapcar add-somethind '(1 2 3))