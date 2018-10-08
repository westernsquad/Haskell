
module Pruebas where
abono::Int->Int
abono edad 
			|edad<10 = 0
			|edad<25=20
			|edad<65=100
			|otherwise=0	
--[(1,2),(1)]