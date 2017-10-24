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

p_menor x y = (x < y) == (x << y)
instance Arbitrary Direction where
   arbitrary = do
           n <- choose (0,3)
           return $ toEnum n 


--Ejercicio 2 --
--a--
maximoYResto :: Ord a => [a] -> (a,[a])
maximoYResto []   = error "maximoYResto: lista vacia"
maximoYResto (x:xs) = ((maximo x:xs), resto x xs)

maximo :: Ord a => [a] -> a
maximo (x:y:xs) | [] = error "maximo: lista vacia"
	        | x:[]  = x
                | x > y = maximo x:xs
                | y >=x = maximo y:xs  

resto :: a -> [a] ->[a] 
resto x (y:xs) = xs
