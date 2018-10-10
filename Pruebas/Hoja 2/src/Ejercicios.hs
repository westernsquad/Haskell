module Ejercicios where
import Data.Char

{--d) Implementa una funci�n que sume los cuadrados de los n�meros pares contenidos en
una lista de n�meros enteros. Se piden dos versiones:
a. Una versi�n que haga uso de las funciones de orden superior de listas map y
filter para definir la nueva funci�n.
b. Una versi�n que utilice la definici�n de listas por comprensi�n--}

--A
sumaCuadrado :: [Int]->[Int]
sumaCuadrado lista=  eliminar lista ++ (map(^2) (filter even (lista)))
--sumaCuadrado lista=filter even (map (^2) lista)

eliminar::[Int]->[Int]
eliminar lista=foldl(\ acum x ->if even x then acum else acum ++[x])[]lista

--B
sumaCuadrados :: [Int]->[Int]
sumaCuadrados lista = [x^2 | x<-lista , even x ]

{--Se pide una funci�n polim�rfica en Haskell que dado un elemento y una lista a�ada
dicho elemento al final de la lista.--}

alFinal::[a]->a->[a]  ---Porque B no funciona
alFinal []elemento=[elemento]
alFinal lista elemento = lista ++ [elemento]