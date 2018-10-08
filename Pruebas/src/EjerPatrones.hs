module EjerPatrones where
--Ejercicios segunda parte I)

{--patron::((Int,String),(Int,String),(Int,String))->(Int,Int,Int)
patron ((x,y),(c,d),(r,t)) = (x,c,r)
patron ((1,"h"),(2,"f"),(3,"d"))--}

--Implementar una funcion que devuelve True si la suma de los cuatro primeros elementos de una lista de numeros enteros
-- es un valor menor a 10 y devolvera False en caso contrario

sumaDiez::[Int]->Bool
sumaDiez (x:y:w:z:zs) =(x+y+w+z) < 10 
sumaDiez_=False
--sumaDiez (1,2,3,4)

--Implementar una funcion que dada una frase retorne un mensaje donde se indique cual es la primera y ultima letra de la frase original
encontrarLetra::String ->String
encontrarLetra s="La primera leta de la frase "++s++" es "++[head s]++ " y la ultima es "++[last s] --al poner los corchetes se convierten en listas de ese string

--Implentar una funcion dada una cadena de caracteres y un caracter, indique el numero de apariciones del caracter en la cadena.
aparicion::String->Char->Int
aparicion []_=0
aparicion s c= length[x|x<-s,x==c]--x es lo q devuelve ,x sale del string s si x es = el char c