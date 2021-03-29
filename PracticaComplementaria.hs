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
segmento:: Int -> Int -> [Int] -> [Int]
segmento n m xs = drop (n-1) (take m xs)



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
finales_:: Int -> [Int] -> [Int]
finales_ x xs = drop (length xs - x) xs



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
factorial n = n * factorial (n-1)

maximo_:: [Int] -> Int
maximo_ [] = error "La lista esta vacia"
maximo_ [x] = x
maximo_ (x:xs) = x `max` (maximo_ xs) 


replicate_:: Int -> Int -> [Int]
replicate_ cant x | cant <= 0 = [] 
                  | otherwise = x : replicate_ (cant-1) x

replicate2_:: Int -> Int -> [Int]
replicate2_ 0 num = []
replicate2_ 1 num = [num]
replicate2_ cant num = num : replicate (cant-1) num



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






















