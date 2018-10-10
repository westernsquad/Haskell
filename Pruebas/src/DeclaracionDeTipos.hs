
module DeclaracionDeTipos where
{--Se pide una funci�n que dada una lista de racionales, donde cada racional se define como
dos n�meros enteros (numerador y denominador), y un n�mero racional, devuelva otra
lista con todos los racionales equivalentes al dado. Realiza dos versiones del ejercicio:
1. Empleando type.
2. Empleando data. 
--}
type Nominador = Integer
type Denominador = Integer
data Fraccion = F Nominador Denominador deriving Show

equivalentes :: Fraccion->Fraccion->Bool
equivalentes (F n1 d1)(F n2 d2)= n1*d2 == n2*d1 