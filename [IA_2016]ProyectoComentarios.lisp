
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
;


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