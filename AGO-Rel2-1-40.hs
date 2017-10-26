-------------------------------------------------------------------------------
---- Estructuras de Datos. 2º Curso. ETSI Informática. UMA
----
---- (completa y sustituye los siguientes datos)
---- Titulación: Grado en Ingeniería …………………………………… [Informática | del Software | de Computadores].
---- Alumno: APELLIDOS, NOMBRE
---- Fecha de entrega: DIA | MES | AÑO
----
---- Relación de Ejercicios 2. Ejercicios resueltos: ..........
----
---------------------------------------------------------------------------------
import Test.QuickCheck
--Ejercicio 1--
data Direction = North | South | East | West
     deriving (Eq,Ord,Enum,Show)
--a--
--(<<) :: Direction -> Direction -> Bool
{-
p_menor x y = (x < y) == (x << y)
instance Arbitrary Direction where
   arbitrary = do
           n <- choose (0,3)
           return $ toEnum n
-}

--Ejercicio 2 --
--a y b--
maximoYResto :: Ord a => [a] -> (a,[a])
maximoYResto []   = error "maximoYResto: lista vacia"
maximoYResto (x:xs) = ((maximo (x:xs)), resto (maximo (x:xs)) (x:xs) [])

maximo :: Ord a => [a] -> a
maximo []       = error "maximo: lista vacia"
maximo [x]      = x
maximo (x:y:xs) = if x >= y then maximo (x:xs) else maximo (y:xs)

resto :: (Eq a,Ord a) => a -> [a] ->[a] -> [a]
resto x [] ys = reverse ys
resto x (y:xs) ys = if x /= y then resto x xs (y:ys) else resto x xs ys

--Ejercicio 3--
reparte :: [a] -> ([a],[a])
reparte []  = error "reparte: lista vacia"
reparte [x] = ([x],[])
reparte (x:y:xs) = reparte' (x:y:xs) ([],[])

reparte' :: [a] -> ([a],[a]) -> ([a],[a])
reparte' [] ys = ys
reparte' [x] (zs,ys) = (zs ++ [x], ys)
reparte' (x:y:xs) (zs,ys) = reparte' xs (zs ++ [x], ys ++ [y])

--Ejercicio 4--
distintos :: Ord a => [a] -> Bool
distintos [] = error "distintos: lista vacia"
distintos [x] = True
distintos (x:y:xs) = x /= y && distintos (x:xs) && distintos (y:xs)

--Ejercicio 5-
--a--
replicate' :: Int -> a -> [a]
replicate' n x | n == 0 = []
               | otherwise = [y | y <- [1..n], let y = x]
--b--
p_replicate' n x = n >= 0 && n <= 1000 ==> length (filter (==x) xs) == n
 && length (filter (/=x) xs) == 0
 where xs = replicate' n x
 --c--
 --OK, passed 100 tests.

--Ejercicio 6--
divisores ::  Int -> [Int]
divisores n = [x | x <- [1..n], divideA x n]

divideA :: Int -> Int -> Bool
divideA x y | mod y x == 0 = True
            | otherwise = False
--segunda parte--
divisores' ::  Int -> [Int]
divisores' n = (reverse [x*(-1) | x <- [1..n], divideA x n])++[x | x <- [1..n], divideA x n]
