
module Ejercicios where
import Data.Char;
--secuencia [1,2,3,5] [[],[1],[1,2],[1,2,3][1,2,3,5]]
--secuencia lista por compresion y fold

listar :: [a]->[[a]]
listar lista = foldl(\acum l ->acum++[last acum++[l]])[[]]lista

--secuencia una cib map y recursi
secuencia :: [a]->[[a]]
secuencia []=[[]]
secuencia(x:xs) = []:(map (x:)(secuencia xs))

--Hacer una funcion que saque los 3 numeros mayores de una lista

menor :: [Int]->Int
menor [x]=x
menor (x:xs)=menorAux xs x

menorAux::[Int]->Int->Int
menorAux[]x=x
menorAu(y:ys)x=if y<x then menorAux ys y else menorAux ys x ---busca el menor de una lista

intercambiar :: [Int]->Int->Int->[Int]
intercambiar (x:xs)y z = if x == y then (z:xs) else x:(intercambiar xs y z) --intecambia el numero menor por el q añadimos

nmayores :: [Int]->Int->[Int]
nmayores l n = nmayoresAux l n 0 []

nmayoresAux::[Int]->Int->Int->[Int]->[Int]--n es 4 el numero de elementos a sacar
nmayoresAux []_ _ acum = acum
nmayoresAux(x:xs)n cont acum= if cont < n then nmayores xs n (cont+1)(acum++[x])--esto se raliza n veces 
							  else let a = menor(acum) in --crea una variable
							  	if x < a then nmayoresAux xs n cont acum
							  	else nmayoresAux xs n cont (intercambiar acum a x)