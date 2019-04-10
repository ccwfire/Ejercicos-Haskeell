--Ejercicios marcados en el pdf
--Alumno Sergio Eduardo Ramirez Fernandez

--- 1.-Encontrar la potencia de un numero
potencia ::  Int -> Int -> Int
potencia x y = x ^ y

--- 2.-Encontar un numero n en un rago determinado
encuentraNumero x = if (x <= 100)
    then "El numero esta en el rango de 100 "
    else "El numero no esta en rango"

--- 3.-Dado un numero entero en segundos determinar la canitdad de horas, minutos y segundos que contiene
deterHora :: Integer -> (Integer,Integer,Integer)
deterHora s = (h, m, se) where 
 h = div s 3600
 ss = mod s 3600
 m = div ss 60
 se = mod ss 60


--- 4.-Determine el numero mayor de 4 numeros
mayorCuatro:: Integer -> Integer -> Integer -> Integer -> String 
mayorCuatro a b c d = if ( a > b &&  a> c && a >d)
                        then "Primero"
                        else if ( b > c &&  b > d )
                            then "Segundo"
                            else if (c >d)
                                then "Tercero"
                                else "Cuarto"

--forma dos
mayor :: Integer -> Integer -> Integer
mayor x y = div ((x + y) + abs(x - y)) 2
                                
mayor4 :: Integer -> Integer -> Integer -> Integer -> Integer
mayor4 w x y z = mayor (mayor w x) (mayor y z)
                                                               
--- 5.-Suma de listas

sumaNumeros = sum [5,2,1,6,3,2,5,7]

--6.-Determina si un elemento dado está contenido en una lista. Devuelve verdadero o falso

elemnto::Int->Bool 
elemnto a = a `elem` [5,4,8]

--7.Determina si dada una lista, ésta se encuentra ordenada. Se debe devolver verdadero o falso. 
listaOrden :: Ord a =>[a] -> Bool
listaOrden [] = True
listaOrden [_] = True
lsitaOrden (x:y:xs)=(x<y) && listaOrden (y:xs) 

--- 8-Determinar si dos listas son iguales

listaPar:: Eq a => [a]->[a]->Bool
listaPar l1 l2 = l1 == l2

-- 9.Realizar una función recursiva que retorne como salida el resultado de la suma 1 + 3 + 5 + 7 + 9 + N
sumaRecur :: Int -> Int
sumaRecur 0 = 0
sumaRecur nume = nume*2-1 + sumaRecur (nume-1)

--10 devolver una lista con los valores pares
valoresPares :: [Int] -> [Int]
valoresPares zs = filter even zs