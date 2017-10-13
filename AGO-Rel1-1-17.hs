-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º Curso. ETSI Informática. UMA
--
-- (completa y sustituye los siguientes datos)
-- Titulación: Grado en Ingeniería Informática
-- Alumno: GUTIÉRREZ OJEDA, ANTONIO
-- Fecha de entrega: DIA | MES | AÑO
--
-- Relación de Ejercicios 1. Ejercicios resueltos: ..........
--
-------------------------------------------------------------------------------
import Test.QuickCheck


--Funciones de apuntes para probar

twice :: (Num a) => a -> a
twice x = x + x

square :: Integer -> Integer
square x  = x * x

pythagoras :: Integer -> Integer -> Integer
pythagoras x y  = square x + square y

maxInteger :: Integer -> Integer -> Integer
maxInteger x y = if x >= y then x else y

second :: Integer -> Integer -> Integer
second x y  = y

succPred :: Int -> (Int,Int)
succPred x  = (x+1,x-1)

sing :: (Ord a, Num a) => a -> a
sing x | x > 0        =  1
       | x < 0        = -1
       | otherwise    =  0

--Ejercicio 1--
--A--
esTerna :: Integer -> Integer -> Integer -> Bool
esTerna x y z = if (square x + square y) == square z then True else False
--B--
terna :: Integer -> Integer -> (Integer,Integer,Integer)
terna x y | x > y       = (square x - square y, 2*x*y,square x + square y)
          | otherwise   = error "No cumple las condiciones"
--C y D--
p_ternas x y = x>0 && y>0 && x>y ==> esTerna l1 l2 h
  where
    (l1,l2,h) = terna x y

--Ejercicio 2--
intercambia :: (a,b) -> (b,a)
intercambia (x,y) = (y,x)

--Ejercicio 3--
--A--
ordena2 :: Ord a => (a,a) -> (a,a)
ordena2 (x,y) = if x > y then (y,x) else (x,y)

p1_ordena2 x y = enOrden (ordena2 (x,y))
  where enOrden (x,y) = x<=y
--Main> quickCheck p1_ordena2
--OK, passed 100 tests.

p2_ordena2 x y = mismosElementos (x,y) (ordena2 (x,y))
  where
    mismosElementos (x,y) (z,v) = (x==z && y==v) || (x==v && y==z)
--Main> quickCheck p2_ordena2
--OK, passed 100 tests.
--B--
ordena3 :: Ord a => (a,a,a) -> (a,a,a)
ordena3 (x,y,z) | x > y             = ordena3 (y,x,z)
                | y > z             = ordena3 (x,z,y)
                | otherwise         = (x,y,z)

--C--
p1_ordena3 x y z = enOrden (ordena3 (x,y,z))
  where enOrden (x,y,z) = x<=y && y<=z
--Main> quickCheck p1_ordena3
--OK, passed 100 tests.

p2_ordena3 x y z = mismosElementos (x,y,z) (ordena3 (x,y,z))
  where
    mismosElementos (x,y,z) (a,b,c) = (x==a && y==b && z==c) || (x==b && y==a && z==c) || (x==a && y==c && z==b) || (x==c && y==b && z==a)
--Main> quickCheck p1_ordena3
--OK, passed 100 tests.

--Ejercicio 4--
--A--
max2 :: Ord a => a -> a -> a
max2 x y | x >= y     = x
         | otherwise   = y

--B--
p1_max2 x y = True ==> max2 x y == x || max2 x y == y
p2_max2 x y = True ==> max2 x y >= x || max2 x y >= y
p3_max2 x y = x >= y ==> max2 x y == x
p4_max2 x y = y >= x ==> max2 x y == y
---OK, passed 100 tests.

--Ejercicio 5--
entre :: Ord a => a -> (a,a) -> Bool
entre x (a,b) | x >= a && x <= b    = True
              | otherwise           = False

--Ejercicio 6--
iguales3 :: Eq a => (a,a,a) -> Bool
iguales3 (x,y,z) | x==y && x==z   = True
                 | otherwise      = False
--Ejercicio 7--
--A--
type TotalSegundos = Integer
type Horas = Integer
type Minutos = Integer
type Segundos = Integer
descomponer :: TotalSegundos -> (Horas,Minutos,Segundos)
descomponer x = (horas, minutos, segundos)
  where
    (horas,resto) = divMod x 3600
    (minutos,segundos) = divMod resto 60
--B--
p_descomponer x = x>=0 ==> h*3600 + m*60 + s == x && entre m (0,59) && entre s (0,59)
  where (h,m,s) = descomponer x
--OK, passed 100 tests.

--Ejercicio 8--
unEuro :: Double
unEuro = 166.386
--A--
pesetasAEuros :: Double -> Double
pesetasAEuros x = x / unEuro
--B--
eurosAPesetas :: Double -> Double
eurosAPesetas x = x * unEuro
--C--
p_inversas x = eurosAPesetas (pesetasAEuros x) == x
--Failed! Falsifiable (after 21 tests and 1075 shrinks):5.0e-324
--No se cumple por el redondeo

--Ejercicio 9--
infix 4 ~=
(~=) :: Double -> Double -> Bool
x ~= y = abs (x-y) < epsilon
  where epsilon = 1/1000
p1_inversas x = eurosAPesetas (pesetasAEuros x) ~= x
--OK, passed 100 tests.

--Ejercicio 20--
--A--
raices :: (Ord a, Floating a) => a -> a -> a -> (a,a) 
raices a b c | discriminante >= 0 = ((-b + raizDis)/(2*a),(-b -raizDis)/(2*a) )
             | otherwise          = error "No existen raices reales"
           where
            discriminante = b^2 - 4*a*c
            raizDis       = sqrt discriminante
--B--
p2_raíces a b c = (b^2 - 4*a*c >= 0) && (a/=0)  ==>  esRaíz r1 && esRaíz r2
 where
  (r1,r2) = raices a b c
  esRaíz r = a*r^2 + b*r + c ~= 0
--OK, passed 100 tests.

--Ejercicio 11--
esMultiplo :: (Integral a) => a -> a -> Bool
esMultiplo x y | mod x y == 0 = True
               | otherwise    = False 

--Ejercicio 12--
infixl 1 ==>>
(==>>) :: Bool -> Bool -> Bool
False ==>> y = True
True  ==>> y = False 

--Ejercicio 13--
esBisiesto :: Integer -> Bool
esBisiesto x | ((mod x 4 == 0) && (mod x 100 /=0)) || ((mod x 100 ==0) && (mod x 400 == 0)) = True
             | otherwise = False

--Ejercicio 14--
--A--
potencia :: Integer -> Integer -> Integer
potencia b n | n == 0    = 1
             | n > 0     = b * potencia b (n-1)
             | otherwise = b
--B--
potencia' :: Integer -> Integer -> Integer
potencia' b n | n == 0       = 1
              | mod n 2 == 0 = potencia' b (div n 2) * potencia' b (div n 2)
              | otherwise    = b * ( potencia b (div (n-1) 2) * potencia b (div (n-1) 2) )
--C--
p_pot b n = n>=0 ==> potencia b n == sol
 && potencia' b n == sol
 where sol = b^n
--OK, passed 100 tests.
--D--


--Ejercicio 15--
factorial ::  Integer -> Integer
factorial x | x == 0 = 1
            | x > 0  = x * factorial (x-1)

--Ejercicio 16--
--A--
divideA :: Integer -> Integer -> Bool
divideA x y | mod y x  == 0 = True
            | otherwise    = False
--B--
p1_divideA x y = y/=0 && y `divideA` x ==> div x y * y == x
--Gave up! Passed only 91 tests.
--C--
p2_divideA x y z = z/=0 && z `divideA` x && z `divideA` y  ==> z `divideA` (x+y) == True
--Gave up! Passed only 61 tests.

--Ejercicio 17--
mediana :: Ord a => (a,a,a,a,a) -> a
mediana (x,y,z,t,u) | x > y = mediana (y,x,z,t,u)
                    | y > z = mediana (x,z,y,t,u)
                    | z > t = mediana (x,y,t,z,u)
                    | t > u = mediana (x,y,z,u,t)
                    | otherwise = z
