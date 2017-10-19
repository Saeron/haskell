-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º ETSI Informática. UMA
-- Práctica 1 - Ejercicios extra
--
-- Alumno: APELLIDOS, NOMBRE
-------------------------------------------------------------------------------


module Practica1Extra where

import Test.QuickCheck

----------------------------------------------------------------------
-- Ejercicio - esPrimo
----------------------------------------------------------------------

-- esPrimo :: completa la definición de tipo
esPrimo n = undefined

----------------------------------------------------------------------
-- Ejercicio - libre de cuadrados
----------------------------------------------------------------------

libreDeCuadrados :: Integer -> Bool
libreDeCuadrados n = undefined

----------------------------------------------------------------------
-- Ejercicio - números de Harshad
----------------------------------------------------------------------

sumaDigitos :: Integer -> Integer
sumaDigitos n = undefined

harshad :: Integer -> Bool
harshad x = undefined

harshadMultiple :: Integer -> Bool
harshadMultiple n = undefined

vecesHarshad :: Integer -> Integer
vecesHarshad n = undefined

prop_Boem_Harshad_OK :: Integer -> Property
prop_Boem_Harshad_OK n = undefined

----------------------------------------------------------------------
-- Ejercicio - ceros del factorial
----------------------------------------------------------------------

factorial :: Integer -> Integer
factorial n  = undefined

cerosDe :: Integer -> Integer
cerosDe n = undefined

prop_cerosDe_OK :: Integer -> Integer -> Property
prop_cerosDe_OK n m = undefined

{-

Responde las siguientes preguntas:

¿En cuańtos ceros acaba el factorial de 10?

¿En cuańtos ceros acaba el factorial de 100?

¿En cuańtos ceros acaba el factorial de 1000?

¿En cuańtos ceros acaba el factorial de 10000?


-}

----------------------------------------------------------------------
-- Ejercicio - números de Fibonacci y fórmula de Binet
----------------------------------------------------------------------

fib :: Integer -> Integer
fib n  = undefined

llamadasFib :: Integer -> Integer
llamadasFib n = undefined

{-

Responde a las siguientes preguntas:

¿Cuántas llamadas a fib son necesarias para calcular fib 30?


¿Cuántas llamadas a fib son necesarias para calcular fib 36?


-}

fib' :: Integer -> Integer
fib' n = undefined

prop_fib_OK :: Integer -> Property
prop_fib_OK n = undefined

{-

Responde a las siguientes preguntas:

¿Cuántas llamadas a fib son necesarias para calcular fib' 30?


¿Cuántas llamadas a fib son necesarias para calcular fib' 36?


-}


phi :: Double
phi = undefined

binet :: Integer -> Integer
binet n = undefined

prop_fib'_binet_OK :: Integer -> Property
prop_fib'_binet_OK n = undefined

{-

Responde a la siguiente pregunta:

¿A partir de qué valor devuelve binet resultados incorrectos?

-}
