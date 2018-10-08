module Recusividad where

import Data.Char;

--Factorial
factorial:: Int -> Int
factorial n = if n == 0 then 1
 else n * factorial(n-1)
 
 --Factorial con patrones hay q respetar el orden 
factoriall:: Int -> Int
factoriall 0 = 1
factoriall n = n * factorial(n-1)

------------------parametros de acumulacion
fact:: Int -> Int
fact n = fact2(n,1)

fact2:: (Int,Int) -> Int
fact2 (n,r) = if n == 1 then r else fact2(n-1,r*n)

---ejercicio funcion q suma los elemntos de una lista	

suma::[Int]->Int
suma[]=0
--suma lista =head lista + suma(tail lista)
suma(x:xs)=x+suma xs--es lo mismo que lo de arriba


--------------------funcion que suma en un acumulador los elementos de una lista
sumas::([Int],Int)->Int
sumas([],acum)=acum
sumas(x:xs,acum)=sumas(xs,acum+x)

---Invertir una lista
invertir::[Char]->[Char]
invertir[]=[]
invertir(c:cs)=invertir cs ++ [c]


--funcion qu busca en una lista
pertenece::Int->[Int]->Bool
pertenece _[]= False
pertenece x(y:ys)=(x==y)||(pertenece x ys)

--una lista y dos elementos hay que cambiar todos los segundos elementos que aparezcan en la lista por el primer elemento
cambio::Int->Int->[Int]->[Int]
cambio _ _ []=[]
cambio x y (c:cs)= if (c==x) then y:(cambio x y cs) else c:(cambio x y cs)

--Ternas : recibe dos listas y crea ternas el primer elemento sera de la primera lista y los otros dos elentos 
--[1,4,8,9,0][15,6,-1,3,2,5,8]

ternas::[Int]->[Int]->[(Int,Int,Int)]
ternas []_=[]
ternas _[]=[]
ternas _[c]=[]
ternas (c:cs)(z:zs)= (c,z,head zs):ternas cs zs--los : significa que itera en la lista de ternas [(Int,Int,Int)]

--Recibe una ista de numeros enteros y devuelve una lista de tuplas que contiene el numero de la lista y la posicion en la que esta

posicion:: [Int]->[(Int,Int)]
posicion l = posicionAux l 0 [][]


posicionAux::[Int]->Int->[Int]->[(Int,Int)]->[(Int,Int)]
posicionAux []_ _ res=res
posicionAux(l:ls) pos control res =if pertenece l control then posicionAux ls (pos+1) control res else posicionAux ls (pos+1) (l:control) (res++[(l,pos)])

--recibe una lista y hay q sacar la cantidad de veces que sale el 0 si sale dos veces seguizas el 00 cuenta como 1
secuencia::[Int]->Int
secuencia []=0
secuencia [0]=1
secuencia(0:0:xs)=secuencia(0:xs)
secuencia(0:x:xs)=1+ secuencia xs
secuencia(y:x:xs)=secuencia(x:xs)
--mergesort
mitadIz :: [l]->[l]
mitadIz xs= take(length xs `div` 2 )xs

mitadDe :: [l]->[l]
mitadDe xs= drop(length xs `div` 2 )xs

ordenar :: Ord l => [l]->[l]->[l] ---Utilizamos ord para poder compararlos  con < o > ord tevuelve e vaolor de la tabla ascii del elemnto
ordenar xs []=xs
ordenar [] ys = ys
ordenar (x:xs)(y:ys)
				|(x<=y) = x:(ordenar xs (y:ys))--si x<=y entonces usamos x como primer elemnto de la neva lista y vamos comprobando el segundo elemnto xs con los elemntos de la segunda mitad (y:ys)
				|otherwise = y:(ordenar (x:xs)ys)--si no usamos el primer elemento de la segunda mitad y vamos comprobando con todos los elemntos de la mitad iz y los restantes de la mitadDE

ordenado :: Ord l => [l]->[l]
ordenado []=[]
ordenado [a]=[a]
ordenado xs = ordenar(ordenado(mitadIz xs )) (ordenado(mitadDe xs))--aqui concatena y ordena las dos mitades


--mergesort2
mergesortt ::[Int]->[Int]
mergesortt []=[]
mergesortt [a]=[a]
mergesortt l = fusionar(mergesortt(a )) (mergesortt(b))
				where
					(a,b)=partir l [](length l `div`2)
					
partir:: [Int]->[Int]->Int->([Int],[Int])
partir resto acum 0 =(acum,resto)
partir (x:xs) acum long = partir xs (acum++[x])(long-1)

fusionar::[Int]->[Int]->[Int]
fusionar l[]=l
fusionar []l=l
fusionar(x:xs)(y:ys)=if(x<y)then(x:(fusionar xs(y:ys))) else (y:(fusionar (x:xs)ys))


---funcion que recibe una lista y un valor y se debe eliminar de la lista los valores q sea igul al valor dado

eliminar::[Int]->Int->[Int]
eliminar lista elemento=foldl(\ acum x ->if x==elemento then acum else acum ++[x])[]lista

--funcion q tiene una lista y un elemnto y lo insetamos al final de la lista

insertar :: [Int]->Int->[Int]
insertar lista elemento=foldr(\x acum->x:acum)[elemento]lista

--numeros de apariciones de un elemnto dentro de una lista

apariion::[Int]->Int->Int
apariion lista elemento =foldr(\x acum->if x==elemento then acum+1 else acum)0 lista

--funcion q elimine los mutiplos de un elemnto
eliminarMul ::[Int]->Int->[Int]
eliminarMul lista elemento=foldr(\x acum->if x`mod`elemento==0 then acum else x:acum)[]lista--x son los elemntos de a lista

--funcion q tiene una lista y sale una tupla con los impare a la iz y los pares a la derecha
separar::[Int]->([Int],[Int])
separar lista=foldr(\x (acum,acum1)->if even x then (acum,x:acum1) else (x:acum,acum1))([],[])lista

--funcion q recibe una lista y hay q separa en una tupla en un lado los elementos que aparecen una vez y en otra los q aparecen mas de una vez
separarRe::[Int]->([Int],[Int])
separarRe lista = foldl(\(unicos,repes)x->if pertenece x repes then (unicos,repes) 
											else if pertenece x unicos then (borrar x unicos ,repes++[x]) 
											else (unicos++[x],repes))([],[])lista


borrar :: Int->[Int]->[Int]
borrar _[] =[]
borrar x(y:ys)=if x==y then (borrar x ys) else x:(borrar x ys)

{--long ::[a]->Int --a representa tanto un elemnto int y un elemnto char  -b->[a]->(a,a)b es un elemnto distinto o igual que a
long[]=0
long(x:xs)=1+long xs--}





