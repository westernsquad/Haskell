
module Genericos where
import Data.Char
--funcion q elimine los numeros q se encuentre en las posiciones pares

pares ::[a]->[a]
pares lista =resultado
				where(resultado,desecho)=paresAux lista

paresAux::[a]->([a],Int)
paresAux lista =foldl(\(acum,pos)x->if even pos then(acum,pos+1)else(acum++[x],pos+1))([],0)lista 

--Cambiar mayusculas por minusculas
cambiarM::[String]->[String]
cambiarM lista = map (foldl(\acum c ->if isUpper c then acum++[toLower c]else acum++[toUpper c])[]) lista


--funcion recibe una palabra y devuelve una dupla con un lado de consonantes y otras con vocales

--separaLetras ::String->(String,String)
--separaLetras palabra =foldr(\letra (vocales,consonante) -> if(elem letra vocal)then (letra:vocales,consonante)else (vocales,letra:consonante) )(,)palabra

vocal =['a','e','i','o','u','A','E','I','O','U'] 








