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
