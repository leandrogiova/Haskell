module PracticaComplementaria where

import Data.List
import GHC.Num

--PRACTICA COMPLEMENTARIA

--Definir la función media3 tal que (media3 x y z) es la media aritmética de los números x , y y z .
--Por ejemplo,1.2. Suma de euros de una colección de monedas:: media3 1 3 8  = 4.0
--media:: Integer -> Integer -> Integer -> Integer
media x y z = (x + y + z) / 3



--Ejercicio 1.5.1. Definir la función ultimaCifra tal que (ultimaCifra x) es la última cifra del número x .
--Por ejemplo, ultimaCifra 325 == 5
ultimaCifra:: Int -> Int
ultimaCifra x = rem x 10



--Ejercicio 1.6.1. Definir la función maxTres tal que (maxTres x y z) es el máximo de x , y y z .
--Por ejemplo, maxTres 6 2 4 == 6       -- maxTres 6 7 4 == 7         --maxTres 6 7 9 == 9
maxTres:: Int -> Int -> Int -> Int
maxTres x y z = max x (max y z)


--1.9.1 Definir la función rango tal que (rango xs) es la lista formada por el menor y mayor elemento de xs
-- Por ejemplo, rango [3,2,7,5] == [2,7] Indicación: Se pueden usar minimum y maximum
rango:: [Int] -> [Int]
rango xs = [minimum xs, maximum xs]



--Ejercicio 1.10.1. Definir la función palindromo tal que (palindromo xs) se verifica si xs es un palíndromo; es decir, es lo mismo leer xs de izquierda a derecha que de derecha a izquierda.
--Por ejemplo, palindromo [3,2,5,2,3] == True       -- palindromo [3,2,5,6,2,3] == False
polindromo:: [Int] -> Bool
polindromo xs = if xs == reverse xs then True
                else False



--1.11.1Ejercicio 1.11.1. Definir la función interior tal que (interior xs) es la lista obtenida eliminando los extremos de la lista xs.
--Por ejemplo, interior [2,5,3,7,3] == [5,3,7]          -- interior [2..7] == [3,4,5,6]
interior:: [Int] -> [Int]
interior xs = tail (init xs)



--1.12 Definir la función finales tal que (finales n xs) es la lista formada por los n finales elementos de xs.
--Por ejemplo, finales 3 [2,5,4,7,9,6] == [7,9,6]
finales:: Int -> [Int] -> [Int]
finales n xs = drop n xs



--Ejercicio 1.13.1. Definir la función segmento tal que (segmento m n xs) es la lista de los elementos de xs comprendidos entre las posiciones m y n .
--Por ejemplo, segmento 3 4 [3,4,1,2,7,9,0] == [1,2]            --segmento 3 5 [3,4,1,2,7,9,0] == [1,2,7]       --segmento 5 3 [3,4,1,2,7,9,0] == []
--segmento:: Int -> Int -> [Int] -> [Int]
--segmento n m xs = drop (n-1) (take m xs)



--1.11.1Ejercicio 1.11.1. Definir la función interior tal que (interior xs) es la lista obtenida eliminando los extremos de la lista xs.
--Por ejemplo, interior [2,5,3,7,3] == [5,3,7]          -- interior [2..7] == [3,4,5,6]
interior2:: [Int] -> [Int]
interior2 [] = []
interior2 xs = init (tail xs)



--Ejercicio 1.10.1. Definir la función palindromo tal que (palindromo xs) se verifica si xs es un palíndromo; es decir, es lo mismo leer xs de izquierda a derecha que de derecha a izquierda.
polindromo2:: [Int] -> Bool
polindromo2 xs = if xs == reverse xs then True
                 else False



--1.12 Definir la función finales tal que (finales n xs) es la lista formada por los n finales elementos de xs.
--Por ejemplo, finales 3 [2,5,4,7,9,6] == [7,9,6]
--finales_:: Int -> [Int] -> [Int]
--finales_ x xs = drop (length xs - x) xs



--Ejercicio 1.22.1. Las dimensiones de los rectángulos puede representarse por pares; por ejemplo,
--(5,3) representa a un rectángulo de base 5 y altura 3. Definir la función mayorRectangulo tal que (mayorRectangulo r1 r2) es el rectángulo de mayor área entre r1 y r2 .
--Por ejemplo, mayorRectangulo (4,6) (3,7) == (4,6) -- mayorRectangulo (4,6) (3,8) == (4,6) --mayorRectangulo (4,6) (3,9)==(3,9)
triangulo:: (Float, Float) -> (Float, Float) -> (Float, Float)
triangulo (a,b) (c,d) = if (a*b) / 2 > (c*d) / 2 then (a,b)
                        else (c,d)



--Ejercicio 1.23.1. Definir la función cuadrante tal que (cuadrante p) es es cuadrante del punto p (se supone que p no está sobre los ejes).
--Por ejemplo, cuadrante (3,5) == 1   --cuadrante (-3,5) == 2   --cuadrante (-3, -5)==3      --cuadrante (3,-5) == 4
cuadrante:: (Int, Int) -> Int
cuadrante (x, y) | (x >= 0 && y >= 0) = 1
                 | (x < 0 && y >= 0)  = 2
                 | (x < 0 && y < 0)   = 3
                 | (x >= 0 && y < 0)  = 4



--Ejercicio 1.23.2. Definir la función intercambia tal que (intercambia p) es el punto obtenido intercambiando las coordenadas del punto p.
--Por ejemplo, intercambia (2,5) == (5,2)
intercambia:: (Int, Int) -> (Int, Int)
intercambia (a,b) = (b,a)



--Ejercicio 1.24.1. Definir la función sumaComplejos tal que (sumaComplejos x y) es la suma de los números complejos x e y .
--Por ejemplo, sumaComplejos (2,3) (5,6) == (7,9)
sumaComplejos:: (Int, Int) -> (Int, Int) -> (Int, Int)
sumaComplejos (a,b) (c,d) = (a+c, b+d)



--Ejercicio 1.25.1. Definir la función intercala que reciba dos listas xs e ys de dos elementos
--cada una, y devuelva una lista de cuatro elementos, construida intercalando los elementos de xs e ys.
--Por ejemplo, intercala [1,4] [3,2] == [1,3,4,2]
intercala:: [Int] -> [Int] -> [Int]
intercala [x1,x2] [y1,y2] = [x1,y1,x2,y2]




--Ejercicio 1.26.1. Definir una función ciclo que permute cíclicamente los elementos de una lista, pasando el último elemento al principio de la lista.
--Por ejemplo, ciclo [2,5,7,9] = [2,9,5,7]
ciclo:: [Int] -> [Int]
ciclo [] = []
--ciclo xs = [head xs : last xs]


factorial:: Int -> Int
factorial 1 = 1
--factorial n = n * factorial (n-1)

maximo_:: [Int] -> Int
maximo_ [] = error "La lista esta vacia"
maximo_ [x] = x
maximo_ (x:xs) = x `max` (maximo_ xs)


--replicate_:: Int -> Int -> [Int]
--replicate_ cant x | cant <= 0 = []
--                  | otherwise = x : replicate_ (cant-1) x

replicate2_:: Int -> Int -> [Int]
replicate2_ 0 num = []
replicate2_ 1 num = [num]
--replicate2_ cant num = num : replicate (cant-1) num



--Ejercicio2.2.1. Tal que (replica n x) es la lista formada por n copias del elemento x.
--Por ejemplo, replica 3 True == [True, True, True]
replica:: Int -> a -> [a]
replica n a = [a | _ <- [1..n] ]



--Ejercicio 2.3.1. Definir la función suma tal (suma n) es la suma de los n primeros números.
--Por ejemplo, suma 3 == 6
suma_::Int -> Int
suma_ x = sum [x | x <- [1..x] ]

suma2_ :: Int -> Int
suma2_ x = sum [1..x]


--divisoresDeX:: Int -> [Int]
--divisoresDeX x = [m | m <- [1..(x-1)], x `mod` m == 0 ]


--Multiplos hasta 100
--multiplosHastaCien:: Int -> [Int]






-- Ejercicio 1. Definir la función
--    elimina :: Int -> [Int] -> [Int]
-- tal que (elimina n xs) es la lista obtenida eliminando en la lista xs
-- los múltiplos de n. Por ejemplo,
--    elimina 3 [2,3,8,9,5,6,7]  ==  [2,8,5,7]
elimina3:: Int -> [Int] -> [Int]
elimina3 z [] = []
elimina3 z [x] = if z == x then []
                 else [x]

elimina3 z (x:xs) | (x `mod` z == 0) = elimina3 z xs
                  | otherwise        = x : elimina3 z xs


elimina3_:: Int -> [Int] -> [Int]
elimina3_ x xs = [ z | z <- xs, z `mod` x  /= 0 ]





{-
Ejercicio 1.4. Definir la función cuadrado tal que cuadrado x es el cuadrado del número x .
Por ejemplo,
cuadrado 3 == 9

Ejercicio 1.5. Definir la función suma_de_cuadrados tal que suma_de_cuadrados l es la
suma de los cuadrados de los elementos de la lista l . Por ejemplo,
suma_de_cuadrados [1,2,3] == 14
-}
cuadrado:: Int -> Int
cuadrado x = x*x

sumaDeCuadrados:: [Int] -> Int
sumaDeCuadrados [x] = cuadrado x
sumaDeCuadrados (x:xs) = cuadrado x + sumaDeCuadrados xs

--Con la funcion del preludio map
sumaDeCuadrados- [x] = cuadrado x
sumaDeCuadrados- xs = sum (map cuadrado xs)



--Valor absoluto -> (-3) == 3   (3) = 3
valorAbsoluto:: Int -> Int
valorAbsoluto x = if x >= 0 then x
                  else -x


valorAbsoluto2 x | x >= 0 = x
                 | otherwise = -x


{-
valorAbsoluto3 x = case x of
                        x >= 0 → x
                        _ → (-x)
-}


--Ejercicio 2.5. Definir la función inverso tal que inverso x es el inverso de x . Por ejemplo,
--inverso 2 ; 0.5
inverso:: Float -> Float
inverso 0 = 0
inverso x = (1 / x)


--Ejercicio 2.8. Redefinir la función map tal que map f l es la lista obtenida aplicando f a cada elemento de l .
--Por ejemplo, map (2*) [1,2,3] ; [2,4,6]
map2 f [] = []
map2 f (x:xs) = f x : map2 f xs


--Por listas por comprension
map3 f xs = [ f x | x <- xs ]

{-Ejercicio 2.9. Redefinir la función filter tal que filter p l es la lista de los elementos de l
que cumplen la propiedad p. Por ejemplo, filter even [1,3,5,4,2,6,1] ; [4,2,6] filter (>3) [1,3,5,4,2,6,1] ; [5,4,6]
filter_:: a -> [a]-> [a]
filter_ fun [] = fun []
filter_ fun (x:xs) = x : filter_ fun xs
-}
filter2_:: (a -> Bool) -> [a] -> [a]
filter2_ fun xs = [x | x <- xs, fun x]

{-
Ejercicio 2.10. Redefinir la función sum tal que sum l es la suma de los elementos de l. Por
ejemplo, n_sum [1,3,6] ; 10
-}
n_sum:: [Int] -> Int
n_sum [] = 0
n_sum [x] = x
n_sum (x:xs) = x + (n_sum xs)
{-
ESTO LO RESUELVE ASI:
[1,2,3]
1 + [2,3]
1 + 2 + [3]
1 + 2 + 3
-}

{-
Ejercicio 2.17. Definir la función factoriales tal que factoriales n es la lista de los facto-
riales desde el factorial de 0 hasta el factorial de n . Por ejemplo,
factoriales 5 ; [1,1,2,6,24,120]

factorial 0 = 1
factorial 1 = 1
factorial 2 (2.1) = 2
factorial 3 (3.2.1) = 6
factorial 4 (4.3.2.1) = 24

factorial_:: Int -> Int
factorial_ x = if x == 0 then 1
               else x * (factorial_ (x-1))

factorial_:: Int -> Int
factorial_ x = product [1..x]

factoriales:: Int -> [Int]
factoriales 0 = []
factoriales 1 = [1]
factoriales x = reverse (aux_factoriales x)


aux_factoriales:: Int -> [Int]
aux_factoriales 0 = [1]
aux_factoriales 1 = [1]
aux_factoriales x = (factorial_ x : aux_factoriales (x-1) )
-}


{-Ejercicio 2.18. Redefinir la función until tal que until p f x aplica la f a x el menor número
posible de veces, hasta alcanzar un valor que satisface el predicado p . Por ejemplo, until (>1000) (2*) 1 ; 1024

n_until:: (a -> Bool) -> (a -> a) -> a -> a
n_until p fun 0 = 0
n_until p fun x = if p x then x 
                  else n_until p fun (fun x)
-}

{-
2.19.
Composición de funciones
Ejercicio 2.19. Redefinir la función (.) tal que f . g es la composición de las funciones f y g ;
es decir, la función que aplica x en f(g(x)) . Por ejemplo,    (cuadrado . siguiente) 2 ; 9         (siguiente . cuadrado) 2 ; 5

siguiente_:: Int -> Int
siguiente_ x = x +1 

cuadrado_:: Int -> Int
cuadrado_ x = x * x

-- cuadrado == f  /////   siguiente == g   
-- f (g (x))    ==  cuadrado ((siguiente x))
 f 'composicion' g x = f (g x )

 -}

{-
Ejercicio 2.21. Definir la función divisible tal que divisible x y se verifica si x es divisible
por y . Por ejemplo,   divisible 9 3 ; True             divisible 9 2 ; False

divisible:: Int -> Int -> Bool
divisible x y = if x `rem` y == 0 then True
                else False

-}

{-
Ejercicio 2.22. Definir la función divisores tal que divisores x es la lista de los divisores de x . Por ejemplo,
divisores 12 ; [1,2,3,4,6,12]


-- Por lista de comprension
divisores:: Int -> [Int]
divisores x = [z | z <- [1..x], rem x z == 0]


--Con filter
divisores2 x = filter (divisible x) [1..x]

-}

{-
2.23. Comprobación de número primo
Ejercicio 2.23. Definir la función primo tal que primo x se verifica si x es primo. Por ejemplo,
primo 5 ; True
primo 6 ; False


primo:: Int -> Bool
primo 1 = True
primo x = if (length (divisores x) <= 2) then True
          else False


primo2 :: Int -> Bool
primo2 x = divisores x == [1,x]

-}

{-
Ejercicio 2.24. Definir la función primos tal que primos x es la lista de los números primos
menores o iguales que x . Por ejemplo,
primos 40 ; [2,3,5,7,11,13,17,19,23,29,31,37]

--Por lista de comprension
primos:: Int -> [Int]
primos x = [z | z <- [1..x], (primo z) == True ]


--Por funcion filter
primos2:: Int -> [Int]
primos2 x = filter (primo) [1..x]
-}

{- 
3.1.
47. Relación de igualdad entre listas 
Ejercicio 3.1. Definir la función igualLista tal que igualLista xs ys se verifica si las dos listas xs e ys son iguales. Por ejemplo,
igualLista [1,2,3,4,5] [1..5] ; True
igualLista [1,3,2,4,5] [1..5] ; False
-}

igualLista :: Eq a => [a] -> [a] -> Bool
igualLista [] [] = True
--igualLista [x] [y] = if x == y then True
--                    else False
igualLista (x:xs) (y:ys) = if x == y then igualLista xs ys
                           else False


igualLista2 [] [] = True
igualLista2 (x:xs) (y:ys) = (x == y) && (igualLista2 xs ys)
igualLista2 _ _ = False


{- 
3.2. Concatenación de listas
Ejercicio 3.2. Definir la función conc tal que conc l1 l2 es la concatenación de l1 y l2 . Por
ejemplo,   conc [2,3] [3,2,4,1] ; [2,3,3,2,4,1]
-}
-----conc:: [Int] -> [Int] -> [Int]
conc [] [] = []
conc [x] [y] = (x:y)
conc (x:xs) ys = x : conc xs ys

{- 
Ejercicio 3.3. Redefinir la función concat tal que concat l es la concatenación de las lista de l .
Por ejemplo, concat [[1,2,3],[4,5],[],[1,2]] ; [1,2,3,4,5,1,2]

concat_1:: [[a]] -> [a]
concat_1 [] = []
concat_1 [xs] = xs
concat_1 (xs:xss) = xs ++ concat_1 xss  

-}


{- 
3.4. Cabeza de una lista
Ejercicio 3.4. Redefinir la función head tal que head l es la cabeza de la lista l . Por ejemplo,
head [3,5,2] ; 3    


head_:: [a] -> a
head_ [] = error "Las listas vacias no tienen ninguna cabezera"
head_ [x] = x
head_ (x:xs) = x

head_2:: [a] -> a
head_2 (x:_) = x

-}

{-
3.5. Resto de una lista
Ejercicio 3.5. Redefinir la función tail tal que tail l es el resto de la lista l . Por ejemplo,
tail [3,5,2] ==  [5,2]      

tail_ [] = error "Una lista vacia no tiene ningun elemento"
tail_ [x] = []
tail_ (x:xs) = xs

-}


{-
3.6. Último elemento
Ejercicio 3.6. Redefinir la función last tal que last l es el último elemento de la lista l . Por
ejemplo,
last [1,2,3] == 3   ;        last [] Program error: pattern match failure: last []

last_:: [a] -> a
last_ [] = error "Las listas vacias no tiene elementos"
last_ [x] = x
last_ xs = head (reverse xs)


last_2:: [a] -> a
last_2 [x] = x
last_2 (_:xs) = last_2 xs
-}


{- 
3.7. 51 Lista sin el último elemento
Ejercicio 3.7. Redefinir la función init tal que init l es la lista l sin el último elemento. Por
ejemplo, init [1,2,3] ==  [1,2]           ;        init [4] == []


init_:: [a] -> [a]
init_ [] = error "Las listas vacias no tienen elementos"
init_ [x] = []
init_ xs = reverse (drop 1 (reverse xs) ) 

-}

{-
3.8. Segmento inicial. Ejercicio 3.8. Definir la función take tal que take n l es la lista de los n primeros elementos
de l . Por ejemplo, take 2 [3,5,4,7] ==  [3,5]    ;       take 12 [3,5,4,7] == [3,5,4,7]

aux_factoriales:: Int -> [Int]
aux_factoriales 0 = [1]
aux_factoriales 1 = [1]
aux_factoriales x = (factorial_ x : aux_factoriales (x-1) )
-}
