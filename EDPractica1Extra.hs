-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º ETSI Informática. UMA
-- Práctica 1 - Ejercicios extra
--
-- Alumno: Gutiérrez Ojeda, Antonio
-------------------------------------------------------------------------------


module Practica1Extra where

import Test.QuickCheck

----------------------------------------------------------------------
-- Ejercicio - esPrimo
----------------------------------------------------------------------

esPrimo :: (Integral a) => a -> Bool
esPrimo n | n <= 0 = error " esPrimo: argumento negativo o cero"
          | n == 1 = False
          | otherwise = esPrimo' (n,n-1)

esPrimo':: (Integral a) => (a,a) -> Bool
esPrimo' (x,y) | y > 1     = mod x y /= 0 && esPrimo' (x,y-1)
               | otherwise = True 

----------------------------------------------------------------------
-- Ejercicio - libre de cuadrados
----------------------------------------------------------------------

libreDeCuadrados :: Integer -> Bool
libreDeCuadrados n | n <= 0 = error "libreDeCuadrados: argumento cero o negativo"
                   | n == 1 = True
                   | otherwise = libreDeCuadrados' (n,2)

libreDeCuadrados' :: (Integer, Integer) -> Bool
libreDeCuadrados' (x,y) | y^2 <= x   = mod x (y^2) /= 0 && libreDeCuadrados' (x,y+1)
                        | otherwise = True  
--Código para ayudar a depurar
libreDeCuadrados'' :: (Integer, Integer) -> Integer
libreDeCuadrados'' (x,y) | y^2 <= x   = if mod x (y^2) == 0 then y else libreDeCuadrados'' (x,y+1)
                         | otherwise = 0


----------------------------------------------------------------------
-- Ejercicio - números de Harshad
----------------------------------------------------------------------

sumaDigitos :: Integer -> Integer
sumaDigitos n | n < 0     = error "sumaDigitos: argumento negativo"
              | n < 10    = n
              | otherwise = (mod n 10) + sumaDigitos (div n 10)

harshad :: Integer -> Bool
harshad x | x <= 0     = error "harshad: argumento no positivo"
          | otherwise = mod x (sumaDigitos x) == 0

harshadMultiple :: Integer -> Bool
harshadMultiple n | n <= 0    = error "harshadMultiple: argumento no positivo" 
                  | otherwise = harshad n && harshad (div n (sumaDigitos n))

vecesHarshad :: Integer -> Integer
vecesHarshad n | n <= 0             = error "vecesHarshad: argumento no positivo"
               | sumaDigitos n == 1 = 1
               | otherwise = if harshad n then 1 + vecesHarshad (div n (sumaDigitos n)) else 0 

prop_Boem_Harshad_OK :: Integer -> Property
prop_Boem_Harshad_OK n = n > 0  ==> vecesHarshad (1008 * 10^n) == n +2 
--OK, passed 100 tests.

----------------------------------------------------------------------
-- Ejercicio - ceros del factorial
----------------------------------------------------------------------

factorial :: Integer -> Integer
factorial n | n > 1 = n * factorial (n-1)
            | otherwise = 1 

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
