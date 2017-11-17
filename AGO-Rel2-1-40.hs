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
import Data.List
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

--Ejercicio 7--
--a--
mcd :: Int -> Int -> Int
mcd a b = maximum [x | x <-(divisores a), y <-(divisores b), x==y]
--b--
p_mcd x y z = x>0 && y>0 && z>0 ==> mcd (z*x) (z*y) == z * mcd x y
 --OK, passed 100 tests.
 --c--
mcm :: Int -> Int -> Int
mcm x y = div (x*y) (mcd x y)

--Ejercicio 8--
--a--
esPrimo :: Int -> Bool
esPrimo x = length (divisores x) == 2
--b--
primosHasta :: Int -> [Int]
primosHasta x = [y | y <- [0..x],esPrimo y]
--c--
primosHasta' :: Int -> [Int]
primosHasta' x = filter esPrimo [0..x]
--d--
p1_primos x = primosHasta x == primosHasta' x
--OK, passed 100 tests.

--Ejercicio 9--
--a--
pares :: Int -> [(Int,Int)]
pares n | n < 2 = []
        | even n = [(x,y) | x <- (primosHasta n), y <- (primosHasta n), x + y == n && x <= y]
        | otherwise = []
--b--
golbach :: Int -> Bool
golbach x = length (pares x) > 0
--c--
golbachHasta :: Int -> Bool
golbachHasta n = and [x | y <- [4..n], let x = (golbach y), even y]
--d--
golbachDebilHasta :: Int -> Bool
golbachDebilHasta n = and [x | y <- [7..n], let x = (golbach (y-3)), not (even y)]

--Ejercicio 10--
--a--
esPerfecto :: Int -> Bool
esPerfecto n = foldr (+) 0 [x | x <- (divisores n) , x/=n] == n && n > 0
--b--
perfectosMenoresQue :: Int -> [Int]
perfectosMenoresQue n = [x | x <- [0..n], esPerfecto x]

--Ejercicio 11--
--a--
take' :: Int -> [a] -> [a]
take' n xs = [x | (p,x) <- zip[0..n-1] xs]
--b--
drop' :: Int -> [a] -> [a]
drop' n xs = [x | (p,x) <-zip [0..(length xs)-1] xs, p >=n]
--c--
p_dropTake n xs = n>=0 ==> (take' n xs) ++ (drop' n xs) == xs
--OK, passed 100 tests.

--Ejercicio 12--
--a--
concat' :: [[a]] -> [a]
concat' xs = foldr (++) [] xs
--b
concat'' :: [[a]] -> [a]
concat'' xs = [y | x <- xs, y <- x]

--Ejercicio 13--
desconocida :: (Ord a) => [a] -> Bool
desconocida xs = and [ x<=y | (x,y) <- zip xs (tail xs) ]
--Coje una lista de elementos que tengan orden y comprueba si estan ordenados

--Ejercicio 14--
--a--
inserta :: Int -> [Int] -> [Int]
inserta n xs = (takeWhile (<n) xs)++(n:[])++(dropWhile (<n) xs)
--b--
inserta' :: Ord a => a -> [a] -> [a]
inserta' n [] = n:[]
inserta' n (x:xs) = if n > x then x:(inserta' n xs) else n:x:xs
--c--
p1_inserta x xs = desconocida xs ==> desconocida (inserta x xs)
--Gave up! Passed only 72 tests.
--comprueba si inserta lo hace de forma ordenada
--d--
--Es como un fold que crea una lista ordenada a partir de una vacia y una desordenada
--e--
ordena :: Ord a => [a] -> [a]
ordena xs = foldr (inserta') [] xs
--f--
p_ordena xs = True ==> desconocida (ordena xs)
--OK, passed 100 tests.
--g--
-- No se aun bien--

--Ejercicio 15--
--a--
geometrica :: Num a => a -> a -> [a]
geometrica i k = iterate (*k) i
--b--
p1_geometrica x r = x>0 && r>0 ==>
 and [ div z y == r | (y,z) <- zip xs (tail xs) ]
 where xs = take 100 (geometrica x r)
 --OK, passed 100 tests.
 --Comprueba que al dividir un numero de la geometrica por su anterior
 --obtines como resultado la constante de la geometrica, hay que tener
 --cuidado con definir geometrica como Int, en este caso no tiene sufi-
 --ciente capacidad para 100 elementos y se quedan en 0(no se cumple).

 --c--
multiplosDe :: Num a => a -> [a]
multiplosDe k = iterate (+k) 0
--d--
potenciasDe :: Num a => a -> [a]
potenciasDe k = iterate (*k) 1

--Ejercicio 16--
--a--
multiplosDe' :: (Ord a, Num a) => a -> [a]
multiplosDe' k | k < 0 = error "Debe ser mayor que 0"
               | otherwise = iterate (+k) k
--b--
primeroComun ::  (Ord a) => [a] -> [a] -> a
primeroComun (x:xs) (y:ys)
 | x > y = primeroComun (x:xs) ys
 | x < y = primeroComun xs (y:ys)
 | otherwise = x
--primeroComun xs ys = minimum [x | x <- xs, y <- ys , x==y]
--c--
mcm' :: Int -> Int -> Int
mcm' x y = primeroComun (multiplosDe' x) (multiplosDe' y)
--d--
p_mcm x y = x>=0 && y>=0 ==> mcm' x y == lcm x y
--p_mcm x y = x>0 && y>0 ==> mcm x y == lcm x y
-- con numeros igual a 0 no debe de funcionar, lampoco funciona lcm
--OK, passed 100 tests.

 --Ejercicio 17--
primeroComunDeTres :: Ord a => [a] -> [a] -> [a] -> a
primeroComunDeTres (x:xs) (y:ys) (z:zs)
 | x > y = primeroComunDeTres (x:xs) ys (z:zs)
 | x > z = primeroComunDeTres (x:xs) (y:ys) zs
 | y > x = primeroComunDeTres xs (y:ys) (z:zs)
 | z > x = primeroComunDeTres xs (y:ys) (z:zs)
 | otherwise = x

 --Ejercicio 18--
factPrimos :: Integer -> [Integer]
factPrimos x = fp x 2
 where
    fp x d
      | x' < d = [x]
      | r == 0 = d : fp x' d
      | otherwise = fp x (d+1)
       where (x',r) = divMod x d -- cociente y resto

--a--

--b--
--Son todos primos por que al empezar con 2 por ejemplo y dividir entre el
--hasta que el resto no de 0 elminias la posibilidad de que sea dividido entre
--4 y asi susesivamente, ya que los demás numeros acaban siendo composisión
-- de los números primos previos.
 --Se para en x' < d por que eso significa que el divisor es el dividendo,
 -- ya no hay mas que dividir
 --c--
factPrimos' :: Int -> [Int]
factPrimos' x = fp x 2
  where
     fp x d
       | x' < d = [x]
       | r == 0 = d : fp x' d
       | even d && d > 2 = fp x (d+1)
       | otherwise = fp x (d+1)
        where (x',r) = divMod x d -- cociente y resto
 --d--
p_factores x = x > 0 ==> (product (factPrimos x)) == x
--OK, passed 100 tests.

--Ejercicio 19--
--a--
mezcla :: [Int] -> [Int] -> [Int]
mezcla [] ys = ys
mezcla xs [] = xs
mezcla (x:xs) (y:ys) | x == y = x:(mezcla xs ys)
                     | y < x  = y:(mezcla (x:xs) ys)
                     | y > x  = x:(mezcla xs (y:ys))
--b--
mcm'' :: Int -> Int -> Int
mcm'' x y = foldr (*) 1 (mezcla (factPrimos' x) (factPrimos' y))
--c--
p_mcm'' x y = x>=0 && y>=0 ==> mcm'' x y == lcm x y
--OK, passed 100 tests.
--Ejercicio 20--
--a--
mezc' :: [Int] -> [Int] -> [Int]
mezc' [] ys = []
mezc' xs [] = []
mezc' (x:xs) (y:ys) | x == y = x:(mezc' xs ys)
                    | x > y  = mezc' (x:xs) ys
                    | y > x  = mezc' xs (y:ys)
--b--
mcd' :: Int -> Int -> Int
mcd' x y = foldr (*) 1 (mezc' (factPrimos' x) (factPrimos' y))
--c--
p_mcd' x y = x>0 && y>0 ==> mcd' x y == gcd x y
--OK, passed 100 tests.

--Ejercicio 21--
p_neutroDer xs = xs++[] == xs
--OK, passed 100 tests.
--Ejercicio 22--
p_asociativa xs ys = xs++ys == ys++xs
--OK, passed 100 tests.

--Ejercicio 23--
--a--
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (x:xs) = x:(nub' (delete' x xs ))

delete' :: Eq a => a -> [a] -> [a]
delete' n [] = []
delete' n (x:xs) = if n==x then delete' n xs else (x:(delete' n xs))

--b--
p_nub' xs = nub xs == nub' xs
--passed 100 tests.
--c--
p_sinRepes xs = distintos (nub' xs)
--No es suficiente por que pueden aparecer en otro orden, o aprecer menos elementos
--passed 100 tests.
--d--
todosEn :: (Eq a) => [a] -> [a] -> Bool
ys `todosEn` xs = all (`elem` xs) ys

p_sinRepes' xs = (nub' xs) `todosEn` xs && distintos (nub' xs)
--passed 100 tests.

--Ejercicios 24--
--a--
binarios :: Int -> [[Char]]
binarios 0 = [[]]
binarios n = (map ('0':) sub) ++ (map ('1':) sub)
    where sub = binarios (n-1)
--b--
p_binarios n = n>=0 && n<=2 ==> long xss == 2^n && distintos xss && all (`todosEn` "01") xss
  where xss = binarios n

long :: [a] -> Integer
long xs = fromIntegral (length xs)
--Gave up! Passed only 64 tests.
--Hay que reducir el valor de n en otro caso tarda mucho, yo lo reduci a 2

--Ejercicio 25--
--a--
{-
varRep :: Int -> a -> [[a]]
varRep 0 x  = [[]]
varRep n xs = (map (head l:) sub ++ (map (head l:) sub))
   where sub = xs
         l
-}

--Ejercicio 31--
facts :: [Integer]
facts = factsAux 1 1

factsAux :: Integer -> Integer -> [Integer]
factsAux n p = [p]++(factsAux (n+1) (p*n))
