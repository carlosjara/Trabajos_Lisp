;[IA_2016]Proyecto.lisp
;Autor: C. Jaramillo
;Agosto 28 2016
;Proyecto final Inteligencia Artificial
;Tecnica 2 . . . Redes Neuronales - Perceptron Multicapa (MLP)
;Modificado 19/10/2015
;Comentarios En el Archivo [IA_2016]ComentariosP2.txt

;Descripcion

;Parametros

;Funcion EscogerPrediccion
(defun EscogerPrediccion (lista_Salidas)
	(if (> (car Lista_Salidas) (car (cdr Lista_Salidas)))
		(print "Sube");(car Lista_Salidas)
		(print "Baja");(car(cdr Lista_Salidas))
	)
)


(defun CalculoIncremento (Error SalidaJ SalidaI)
	(* 0.5 Error (sigmoide SalidaJ) (- 1 (sigmoide SalidaJ)) SalidaI)
) 

;Se tomara la pareja de Unidades I J, con las salidas necesarias (SalidaJ, SalidaI, SalidaEsperada) y el pesos W_[SalidaI,J] entre ellos
(defun parejas (perceptron)
	(reemplazar-en (reemplazar-en (ElementosFuncion (cadadr perceptron) (caadr perceptron) (cadar (last perceptron)) (caadar perceptron)) 2 (car perceptron)) 1 perceptron)
)

;Aplicacion de la funcion para Propagacion en Salidas
;SalidasJ 2
;SalidasI 6
;SalidaEs 2
;PesosIJ  6
(defun ElementosFuncion (SalidasJ SalidasI SalidaEsperada PesosIJ)
	;Errores hace el ciclo de 2
	(Errores SalidasJ SalidasI SalidaEsperada PesosIJ (list-length SalidasJ) '());NuevosPesosIJ) 
)
;
;Parejas de nuevos pesos por cada unidadJ
(defun Errores (SalidasJ SalidasI SalidaEsperada PesosIJ CantSalidasJ NuevosPesosIJ)
	(cond ((= CantSalidasJ 0) NuevosPesosIJ)
			(t (Errores (cdr SalidasJ) SalidasI (cdr SalidaEsperada) PesosIJ (- CantSalidasJ 1) 
						(append (list (Incremento (CalculoError (car SalidaEsperada) (car SalidasJ)) 
										  SalidasI 
										  (car SalidasJ) 
										  PesosIJ
										  (list-length PesosIJ)
										  '()
								 )) NuevosPesosIJ
						)
				)
		   ) 
	)
)

;Tuplas de nuevos pesos pode cada unidadI
(defun Incremento (Error SalidaI SalidaJ PesosIJ CantPesosIJ NuevosPesosT)
	(cond ((= CantPesosIJ 0) NuevosPesosT)
			(t (Incremento Error (cdr SalidaI) SalidaJ (cdr PesosIJ) (- CantPesosIJ 1) 
						(append 
								(list (+ (CalculoIncremento Error (car SalidaI) SalidaJ) (car PesosIJ)))
								NuevosPesosT
						)
			   )
			)
	)
)

;Calculo de los errores con respecto a los valores esperados
;ValoreEsperados (1 0) ~ (cadar (last (Propagacion '(20.94 1.2894 49.3298 21.22 1.548) 6 '(1 0))))
;ValoreGenerados (0.7109495 0.58419055) ~ (cadddr (Propagacion '(20.94 1.2894 49.3298 21.22 1.548) 6 '(1 0)))
(defun CalculoError (ValorEsperado ValorGenerado)
	(abs (- ValorEsperado ValorGenerado))
)
;(calculoError '(1 0) '(0.7109495 0.58419055))
(defun reemplazar-en (Elemento Pos Lista)
	(append 
			 (subseq Lista 0 (- pos 1))
             (cons Elemento (nthcdr pos Lista))
	)
)


;(EscogerPrediccion (cadddr (Propagacion '(20.94 1.2894 49.3298 21.22 1.548) 6 '(1 0))))

;Iniciar Propapagacion Hacia Adelante
(defun Propagacion (Entradas CapasOcultas Salidas)
	(AgregarCapasActivadas (crearPerceptroninicial Entradas CapasOcultas Salidas) 2)
)
;(Propagacion '(20.94 1.2894 49.3298 21.22 1.548) 6 '(1 0))

;AgregarCapasActivdas
(defun AgregarCapasActivadas (perceptron contador) 
	
	(cond ((= contador 0) perceptron)
		((= contador 2) (AgregarCapasActivadas (insertar-en (sigmoide_Suma_Neuronas (caar perceptron) (caar (last perceptron)) (caadr perceptron) (list-length (caar perceptron)) '()) perceptron (list-length perceptron))(- contador 1)))
		((= contador 1) (insertar-en (sigmoide_Suma_Neuronas (cadar perceptron) (caddr perceptron) (cadadr perceptron) (list-length (cadar perceptron)) '()) perceptron (list-length perceptron)))
		
	)
)

;Funcion sigmoide suma neuronas
(defun Sigmoide_Suma_Neuronas (PesosNeuronas Entradas BiasNeuronas contNeuronas NeuronasActivadas)
	(cond ( (= contNeuronas 0) NeuronasActivadas)
		(t (Sigmoide_Suma_Neuronas (cdr PesosNeuronas) Entradas (cdr BiasNeuronas) (- contNeuronas 1)
				(ActivarNeurona (car PesosNeuronas) Entradas (car BiasNeuronas) NeuronasActivadas))
		)
	)
)
;(sigmoide_suma_neuronas '((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0)) '(1 2 3 4 5) '(0.54 0.02 0.42 0.91 0.45 0.26) 6 '())


;Aplica la funcion sigmoidal al resultado de la suma de las entradas por los pesos de la neurona a acticar
(defun activarNeurona (PesosNeurona Entradas Bias NeuronasActivadas)
	(insertar-en (sigmoide (+(sumas (productos PesosNeurona Entradas (list-length Entradas) '()))Bias)) NeuronasActivadas (+(list-length NeuronasActivadas)1) )
)
;(activarneurona '(0 0 0 0 0) '(1 2 3 4 5) 0.3 '())

(defun sumas (lista)
  (reduce '+ lista)
)

;Funcion Sigmoide; aplica la funcion a un valor
(defun sigmoide (Valor)
	(/ 1 (+ 1 (exp (* Valor -1))))
)

;Funcion sumatoria, suma los productos para una unidad ya sea oculta o de salida
(defun productos (Pesos entradas cantEntradas salida)
	(cond ( (= cantEntradas 0) salida)
		(t (productos (cdr Pesos) (cdr entradas) (- cantEntradas 1) (nconc salida (list (* (car Pesos) (car entradas)) ))))

	)
)
;(sumatoria '(0.5 0.5 0.5 0.5 0.5) '(2 4 6 8 10) 5 '())

;Crea el perceptron agreando a la lista las entradas y las salidas
(defun crearPerceptronInicial (Entradas cantNeuronasOcultas Salidas)
	(append
		(creacionPerceptronPesos_Bias Entradas cantNeuronasOcultas (list-length Salidas))
		(list (append 
			(list Entradas)
			(list Salidas)
		))
	)
)
;(crearPerceptroninicial '(2 4 6 8 10) 6 '(1 1))


;Crea el perceptron inicial, con los pesos y los bias iniciales
(defun creacionPerceptronPesos_Bias (Entradas cantOcultas cantSalidas)
	(append 
		(list (append 
			(list (C_PesosIniciales (list-length Entradas) cantOcultas '()))
			(list (C_PesosIniciales cantOcultas cantSalidas '()))
		))
		(list (C_Bias cantOcultas cantSalidas))
	) 
)
;Crea la lista de los valores de Bias tanto para la lista de unidades de la capa oculta como las de 
;las unidades de salida
(defun C_Bias (NeuronasOcultas NeuronasSalida)
	(append 
		(list (Asignar_Bias NeuronasOcultas '()))
		(list (Asignar_Bias NeuronasSalida '()))
	)
) 

;Asigna valores aleatorios entre cero y uno para darselos como Bias a cada unidad
(defun Asignar_Bias (Neuronas B)
	(cond ((= Neuronas 0) B)
		(t (Asignar_Bias (- Neuronas 1) (nconc B (list (random-for-bias 0 1)))))
	)
)
;Se crean pesos de cero para las conexiones entre las unidades, tando de la capa oculta con la de entredas como
;las de la capa oculta con las de salida
(defun C_PesosIniciales (Entradas Ocultas W) 
	(cond ((= Ocultas 0) W)
		(t (C_PesosIniciales Entradas (- Ocultas 1) (nconc W (list (CrearSublista_PesosIniciales Entradas '() )))))
	)
)
;Crea lista con los elementos previos que estan conectados a una unidad
(defun CrearSublista_PesosIniciales (Entradas Lista)
	(cond ((= Entradas 0) Lista)
		(t (CrearSublista_PesosIniciales (- Entradas 1) (nconc Lista (list 0))))
	)
)

;Funcion insert-at agregar un elemento al una lista, sin borrar el elemento anterior
(defun insertar-en (item Lista pos)
	;(write "Insert-At")
  (append (subseq Lista 0 (- pos 1))
          (list item)
          (nthcdr (- pos 1) Lista))
)
;Funcion random-From-range genera un numero aleatorio entre dos numeros
(defun random-for-bias (start end)
	(/ (+ start (random (+ 1 (- (* end 100) start)))) 100.0)
)