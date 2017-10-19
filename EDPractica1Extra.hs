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
cerosDe n | n == 0    = 1
          | otherwise =if mod n 10 == 0 then 1 + cerosDe (div n 10) else 0

prop_cerosDe_OK :: Integer -> Integer -> Property
prop_cerosDe_OK n m = m > 0 && m < 1000 && n > 0 ==> cerosDe (n*10^m) == cerosDe n + m

{-

Responde las siguientes preguntas:

¿En cuańtos ceros acaba el factorial de 10?
2
¿En cuańtos ceros acaba el factorial de 100?
24
¿En cuańtos ceros acaba el factorial de 1000?
249
¿En cuańtos ceros acaba el factorial de 10000?
2499

-}

----------------------------------------------------------------------
-- Ejercicio - números de Fibonacci y fórmula de Binet
----------------------------------------------------------------------

fib :: Integer -> Integer
fib n | n < 0 = error "fib: argumento negativo"
      | n == 0 = 0
      | n == 1 = 1
      | otherwise = fib (n-1) + fib (n-2)

llamadasFib :: Integer -> Integer
llamadasFib n | n < 0 = error "fib: argumento negativo"
              | n == 0 = 1
              | n == 1 = 1
              | otherwise = 1 + llamadasFib (n-1) + llamadasFib (n-2)


{-

Responde a las siguientes preguntas:

¿Cuántas llamadas a fib son necesarias para calcular fib 30?
2692537

¿Cuántas llamadas a fib son necesarias para calcular fib 36?
48315633

-}
fibAc :: Integer -> Integer -> Integer -> Integer
fibAc n x y | n > 0 = fibAc (n-1) y (x+y)
            | otherwise = x

fib' :: Integer -> Integer
fib' n | n < 0 = error "fib': argumento negativo"
       | otherwise = fibAc n 0 1

prop_fib_OK :: Integer -> Property
prop_fib_OK n = n > 0 && n <= 30 ==> fib n == fib' n
--OK, passed 100 tests.
{-

Responde a las siguientes preguntas:

¿Cuántas llamadas a fib son necesarias para calcular fib' 30?
30

¿Cuántas llamadas a fib son necesarias para calcular fib' 36?
36

-}


phi :: Double
phi = (1 + sqrt 5) / 2

binet :: Integer -> Integer
binet n = round ((phi^n - (1 - phi)^n) / sqrt 5)

prop_fib'_binet_OK :: Integer -> Property
prop_fib'_binet_OK n = n > 0 ==> fib' n == binet n

{-

Responde a la siguiente pregunta:

¿A partir de qué valor devuelve binet resultados incorrectos?
Failed! Falsifiable (after 82 tests):
76
-}
