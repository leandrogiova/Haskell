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










