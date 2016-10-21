;[IA_2016]Proyecto.lisp
;Autor: C. Jaramillo
;Agosto 28 2016
;Proyecto final Inteligencia Artificial
;Tecnica 1 . . . Algoritmos geneticos
;Modificado 19/10/2015
;se añade implementacion algoritmos geneticos

;El individuo esta definido como una tupla de atributos
;Estos atributos son tomados de la base de conocimientos que es entregado en 
;(Precio_Apertura, Precio_cierre, Porcentaje_Cambio_Precio, Volumen, Porcentaje_Cambio_Volumen)
;Para poder dar mayor agilidad al calculo utilizaremos pares de registros basado en el teorema de bayes,
;que en otras palabras indica que para calcular el precio de mañana solo necesito los valores de hoy y para 
;calcula el precio de hoy solo necesito el de ayer, con eso entocnes tomaremos solo los registros de dos semanas
;concecutivas para asi calcular el de la siguiente semana.
;-----------------------------------------------------------------
;NOTA
;-----------------------------------------------------------------
;El algorimo presentado en este da respuesta en un 31.5% de las veces 
;y un 47.3% de las veces falla
;entre las respuestas un 50% son acertadas 
;NOTA - una prueba de un trimestre en el final del algoritmo
;ver pasos en el final del algoritmo
;-----------------------------------------------------------------
;-----------------------------------------------------------------

;//Funciones
;-----------------------------------------------------------------
;Funcion GetVol toma el porcentaje del cambio del volumen del individuo para asignarlo como Fitness
(defun GETVOL (individuo)
	;(write "GETVOL")
	(last individuo)
)
;Funcion insert-at agregar un elemento al una lista, sin borrar el elemento anterior
(defun insert-Fitness-At (item indList pos_ind)
	;(write "Insert-Fitness-At")
  (append (subseq indList 0 (- pos_ind 1))
          item
          (nthcdr (- pos_ind 1) indList))
)
;Funcion Fitness Tomará el valor del porcentaje y lo asigno como fitness de cada valor
(defun Fitness (indList fitnessList cont)
	;(write "Fitness")
	(cond ( (= cont 3) fitnessList)
		  ( (< cont 4) (Fitness (cdr indList) (INSERT-FITNESS-AT (GETVOL (car indList)) fitnessList (+ (list-length fitnessList ) 1)) (+ 1 cont)))
	)
)
;(FITNESS '((12 14 0.15 2000 0.30)(11 9 -0.25 5000 0.20)(11 9 -0.25 5000 0.30)) '() 0)

;Funcion GET_FITNESS entrega la lista con los fitness de los individuos
;@Param indListM Individuos con la mutacion aplicada
(defun GET_FITNESS (indListM)
	;(write "GET_FITNESS")
	(FITNESS indListM '() 0)
)

;Funcion que quita el elemento seleccionado y entrega la nueva pareja
(defun SELECCION_GEN (indList fitnessList)
	(selectAux (UbiSelect indList fitnessList) indList)
)

;Funcion Aplicacion De Seleccion
(defun selectAux (indMin indList)
	;(write "Select")
	(remove indMin indList :test #'equal)
)
;(select '(4 5 6) '((1 2 3)(2 3 4)(4 5 6)))

;Funcion UbiSeleccion
;Ubica el elemento con menor fitness para poder eliminarlo
(defun Ubiselect (indList fitnessList)
	;(write "UbiSelect")
	(nth (position (reduce #'min fitnessList) fitnessList :test #'equal) indList)
)
;(ubiselect '((1 2 3)(2 3 4)(4 5 6)) '(10 34 5))

;Funcion crucep genera una random en el largo de dos individuos
;y toma la primera parte del segundo individuo y se la agrega
;la segunda parte del primer individuo
;Faltan correciones
(defun crucep (ind1 ind2)
		;(write "Cruce1")
     (let ((numero (random-from-range 1 (- (list-length ind1) 1))))
		 (append (subseq ind2 0 numero) (nthcdr numero ind1))
    )
) 

;Funcion CruzandoPadres Separa los padres para realizar el cruce
(defun cruz_p (indList)
	(crucep (car indList) (cadr indList)) 
)

;Funcion random-From-range genera un numero aleatorio entre dos numeros
(defun random-from-range (start end)
	;(write "random-from-range")
  (+ start (random (+ 1 (- end start))))
)
;Funcion change-at agregar un elemento al una lista, borrando el elmento anterior en esa posicion
(defun change-at (item indList pos_ind)
  ;(write "Change-at")
  (append (subseq indList 0 (- pos_ind 1))
          (list item)
          (nthcdr pos_ind indList))
)

;Funcion insert-at agregar un elemento al una lista, sin borrar el elemento anterior
(defun insert-at (item indList pos_ind)
	;(write "Insert-At")
  (append (subseq indList 0 (- pos_ind 1))
          (list item)
          (nthcdr (- pos_ind 1)indList))
)

;Funcion Mutacion selecciona cualquier individuo para aplicarle mutacionAux
(defun Mutacion_Gen (indList)
	;(write "Mutacion")
	(let ((pos_indi (random-from-range 1 (list-length indList))))
		(let ((ind (subseq indList 0 pos_indi)));saca los individuos desde 1 hasta pos_indi
			;(print ind)
            (change-at (mutacionAux (car (last ind))) indList pos_indi)
        )
	)
)

;Funcion mutaciónAux de individuo, solecciona aleatoriamente un atributo del individuo y a este le aplica
;un valor generado aleatoriamente entre -30.0% y 30.0% 
;si el valor generado es par lo suma al atributo de lo contrario lo resta.
(defun mutacionAux (ind1)
	;(write "MutacionAux")
    (let ((pos_atrib (random-from-range 1 (list-length ind1))))
        (let ((list1 (subseq ind1 0 pos_atrib)))
            (let ((PorMutacion (/ (random-from-range -30.0 30.0) 100.0)))
				(cond 
					((eq (mod (floor (* PorMutacion 100)) 2) 0)
						(substitute (+ (* PorMutacion (car (last list1))) (car (last list1))) (car (last list1 )) ind1)
					)
					((not (eq (mod (floor (* PorMutacion 100)) 2) 0))
						(substitute (- (car (last list1)) (* PorMutacion (car (last list1))) ) (car (last list1 )) ind1)
					)
				)
            )
        )
    )
)

;funcion Algorimo genetico (Principal) para busqueda de la mejor pareja
(defun AG (indPar Generaciones)
	(let ((contador (cons '0 '())))
		(if (= Generaciones 1)
				(indPar)
				(GAAux (* 10 Generaciones) indPar contador)
		)
	)
)


;Funcion GAAux Recorre la cantidad de generaciones para realizar el procedimiento
(defun GAAux (Generaciones indPar contador)
	;(write "AGAux")
	(if (< (car contador) Generaciones)
		(GAAux Generaciones (Solucion indPar)  (change-at (+ 1 (car contador)) contador 1))
		(list indPar)
	)
	
)

;Funcion Solucion
(defun SOLUCION (Pareja)
		(SELECCION_GEN (MUTACION_GEN (INSERT-AT (CRUZ_P Pareja) Pareja 1)) (GET_FITNESS (MUTACION_GEN (INSERT-AT (CRUZ_P Pareja) Pareja 1))))
)


;Funcion SeleccionMejor
(defun SeleccionMejor (Padres)
	(SELECCION_GEN Padres (GET_FITNESS Padres))
)

;Dice la tendencia de la prediccion
;1 sube
(defun TENDENCIA (Padre Generado)
	(if (= (car (TendenciaSemanalPrecio Padre Generado)) 1)
		(	if (= (car (TendenciaSemanalVolumen Padre Generado)) 1)
				(write "Subirá")
				(write "Bajará")
			)
		(	if (= (car (TendenciaSemanalVolumen Padre Generado)) 0)
				(write "Bajará")
				(write "Subirá")
			)
	)
)
;Funcion TendenciaSemanalPrecio - Resta los precios de ambas semanas
;si es >0 hay tendencia alcista por tanto devuelve 1
;de lo contrario Bajista por tanto devuelve 0
;ESTRUCTURA DE REGISTRO
;(Apertura,Cierre,Porcentaje_cambio_precio,volumen,porcentaje_cambio_volumen)
;0 -> Apertura
;1 -> Cierre
;2 -> Porcentaje_cambio_precio
;3 -> volumen
;4 -> Porcentaje_cambio_volumen
(defun TendenciaSemanalPrecio (Padre Generado)
	(if (> (- (nth 0 Padre) (nth 0 Generado)) 0)
		(list '1)
		(list '0)
	)
)

;Funcion TendenciaSemanalVolumen - Resta los Volumenes de ambas semanas
;si es >0 hay El volumen sube por tanto devuelve 1
;de lo contrario El volumen baja por tanto devuelve 0
;ESTRUCTURA DE REGISTRO
;(Apertura,Cierre,Porcentaje_cambio_precio,volumen,porcentaje_cambio_volumen)
;0 -> Apertura
;1 -> Cierre
;2 -> Porcentaje_cambio_precio
;3 -> volumen
;4 -> Porcentaje_cambio_volumen
(defun TendenciaSemanalVolumen (Padre Generado)
	(if (> (- (nth 3 Padre) (nth 3 Generado)) 0)
		(list '1)
		(list '0)
	)
)


;//Pasos
;-----------------------------------------------------------------
;Definicion de pruebas de codigo
;se definen la pareja inicial para realizar la busqueda
;----------------------------------------------------------------------
;(setq padre '(16.19 16.38 -2.47066 138428495 -43.02495926))           |
;(setq abuelo '(16.71 15.97 -4.42849 242963398 1.380223028))           |
;----------------------------------------------------------------------
;Nota:
;--- Ver mas abajo definiciones de parejas ---
;de pasan y se realiza el calculo de la tendencia del valor individuo
;generado.
;----------------------------------------------------------------------
;(setq pareja (append (list padre) (list abuelo)))                     |
;(TENDENCIA padre (car (SELECCIONMEJOR (car (AG pareja 2)))))          |

;// PRUEBA 
;-----------------------------------------------------------------------------------------------------
;_____________________________________________________________________________________________________
;STOCK AA
;______________________________________________________________________|PREDICCION |BIEN/MALO| FALLAS|
;(setq padre '(16.19 15.79 -2.47066 138428495 -43.02495926))           |   bajara  |  BIEN   |   0   |
;(setq abuelo '(16.71 15.97 -4.42849 242963398 1.380223028))           

;(setq padre '(15.87 16.13 1.63831 151379173 9.355500109))             |   bajara  |  MALO   |   1   |
;(setq abuelo '(16.19 15.79 -2.47066 138428495 -43.02495926))          

;(setq padre '(16.18 17.14 5.93325 154387761 1.987451735))             |   subira  |  BIEN   |   0   |
;(setq abuelo '(15.87 16.13 1.63831 151379173 9.355500109))            

;(setq padre '(17.33 17.37 0.230814 114691279 -25.71219489))           |   bajara  |  MALO   |   0   |
;(setq abuelo '(16.18 17.14 5.93325 154387761 1.987451735))            

;(setq padre '(17.39 17.28 -0.632547 80023895 -30.22669579))           |   bajara  |  BIEN   |   0   |
;(setq abuelo '(17.33 17.37 0.230814 114691279 -25.71219489))          

;(setq padre '(16.98 16.68 -1.76678 132981863 66.17769355))            |   bajara  |  BIEN   |   1   |
;(setq abuelo '(17.39 17.28 -0.632547 80023895 -30.22669579))          

;(setq padre '(16.81 16.58 -1.36823 109493077 -17.66315005))           |   bajara  |  BIEN   |   0   |
;(setq abuelo '(16.98 16.68 -1.76678 132981863 66.17769355))           

;(setq padre '(16.58 16.03 -3.31725 114332562 4.419900447))            |   subira  |  MALO   |   1   |
;(setq abuelo '(16.81 16.58 -1.36823 109493077 -17.66315005))          

;(setq padre '(15.95 16.11 1.00313 130374108 14.03060136))             |   bajara  |  MALO   |   6   |
;(setq abuelo '(16.58 16.03 -3.31725 114332562 4.419900447))           

;(setq padre '(16.38 17.09 4.33455 95550392 -26.71060729))             |   bajara  |  MALO   |   0   |
;(setq abuelo '(15.95 16.11 1.00313 130374108 14.03060136))            

;------------------------------------------------------------------------------------5 de 10--9 de 19-
;(setq pareja (append (list padre) (list abuelo)))                     |
;(TENDENCIA padre (car (SELECCIONMEJOR (car (AG pareja 2)))))          |