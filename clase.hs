data Vector = Vector  Double Double Double deriving Ord
v1 :: Vector
v1 = Vector 0 0 
v1 = Vector 0 0 0

instance Eq Vector where
  Vector x y z == Vector x1 y1 z1 = x == x1 && y == y1 && z == z1

instance Show Vector where
  show (Vector x y z ) = "V(" ++ (show x) ++ " " ++ (show y) ++ " " 
  ++ (show z ) ++ ")"

modulo :: Vector -> Double
modulo (Vector x y z) = sqrt (x*x + y*y + z*z)

--Para importar modulos :set -i/ruta
--:cd /ruta
--:r

--Ejercicio 1 de pilas--
import DataStructures.Stack.LinearStack

wellBalanced
