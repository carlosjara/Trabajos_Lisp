; ejemplo1.lisp
; Autor: G. Alvarez 
; Febrero 7 de 2015
; Este archivo contiene funciones simples de manejo de listas

; Recibe un lista y retorna un entero que corresponde al número de elementos en ella
(defun size (lista)
    (cond ((null lista) 0)
          (t (+ 1 (size (cdr lista))))))

(size '(a b c d e))
; Recibe una lista una lista vacía y retorna su reverso
(defun inverse (lista resp)
    (cond ((null lista) resp)
          (t (inverse (cdr lista)(cons (car lista) resp)))))

(inverse '(a b c d e)())

(defun inverse1 (lista)
    (cond ((null lista) nil)
          (t (append (inverse1 (cdr lista))(list (car lista))))))

(inverse1 '(a b c d e))

(defun merge1 (lista1 lista2)
  (cond ((null lista1) lista2)
        ((null lista2) lista1)
        (t (append (list (car lista1)(car lista2))
                   (merge1 (cdr lista1)(cdr lista2))))))

(merge1 '(a b c d e) '(1 2 3 4 5 6 7 8 9))

(defun maximo (lista)
  (cond ((null lista) 0)
        ((> (car lista)(maximo (cdr lista))) (car lista))
        (t (maximo (cdr lista)))))

; función de Sebastián Lozano
(defun encontrar (lista elemento)
  (cond((null lista) nil)
       ((= (car lista) elemento) elemento)
       (t (encontrar  (cdr lista) elemento) ) ) ) 


; cambiar todas la ocurrencias de un elemento por otro en una lista
(defun cambiar (elem1 elem2 lista)
  (cond ((null lista) nil)
        ((equal (car lista) elem1)
          (cons elem2 (cambiar elem1 elem2 (cdr lista))))
        (t (cons (car lista)(cambiar elem1 elem2 (cdr lista))))))

(defun cambiarCj (elem1 elem2 lista)
  (if (equal (car lista) elem1)
       (cons elem2 (cambiar elem1 elem2 (cdr lista)))
       (cons (car lista)(cambiar elem1 elem2 (cdr lista)))
  )
  )

;Contar cuantas veces aparece un elemento en una lista

(defun contar (elem lista)
  (if (equal (car lista) elem)
      (+1 (contar elem (cdr lista)))
      (+0 (contar elem (cdr lista)))
    ) 

  )

; Mezclar una lista con el reverso de otra [EN UNA SOLA LINEA]

(defun )
