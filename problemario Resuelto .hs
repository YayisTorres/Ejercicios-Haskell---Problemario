
-----------------------------------------
--Funicones Basicas 
---------------------------------------------

-- Ejercicio 1
-- Descripción: Calcula el promedio de tres números
-- Parámetros: Tres números de tipo Float
-- Retorno: El promedio de los tres números como Float
promedio3 :: Float -> Float -> Float -> Float
promedio3 x y z = (x + y + z) / 3

-- Ejercicio 2
-- Descripción: Calcula la suma del valor de monedas
-- Parámetros: Cinco enteros representando la cantidad de monedas de 1, 2, 5, 10 y 20
-- Retorno: La suma total del valor de las monedas como Int
sumaMonedas :: Int -> Int -> Int -> Int -> Int -> Int
sumaMonedas a b c d e = a * 1 + b * 2 + c * 5 + d * 10 + e * 20

-- Ejercicio 3
-- Descripción: Calcula el volumen de una esfera
-- Parámetros: El radio de la esfera como Float
-- Retorno: El volumen de la esfera como Float
volumenEsfera :: Float -> Float
volumenEsfera r = (4 / 3) * pi * r ^ 3

-- Ejercicio 4
-- Descripción: Calcula el área de una corona circular
-- Parámetros: Dos radios (r1 y r2) como Float
-- Retorno: El área de la corona circular como Float
areaDeCoronaCircular :: Float -> Float -> Float
areaDeCoronaCircular r1 r2 = pi * (r2 ^ 2 - r1 ^ 2)

-- Ejercicio 5
-- Descripción: Obtiene la última cifra de un número entero
-- Parámetros: Un número entero
-- Retorno: La última cifra del número como Int
ultimaCifra :: Int -> Int
ultimaCifra x = x `rem` 10

-- Ejercicio 6
-- Descripción: Encuentra el máximo de tres números
-- Parámetros: Tres números enteros
-- Retorno: El máximo de los tres números como Int
maxTres :: Int -> Int -> Int -> Int
maxTres x y z = max x (max y z)

-- Ejercicio 7
-- Descripción: Rota una lista una posición a la izquierda
-- Parámetros: Una lista de elementos de cualquier tipo
-- Retorno: La lista rotada una posición a la izquierda
rota1 :: [a] -> [a]
rota1 xs = tail xs ++ [head xs]

-- Ejercicio 8
-- Descripción: Rota una lista n posiciones a la izquierda
-- Parámetros: Un entero n y una lista de elementos de cualquier tipo
-- Retorno: La lista rotada n posiciones a la izquierda
rota :: Int -> [a] -> [a]
rota n xs = drop n xs ++ take n xs

-- Ejercicio 9
-- Descripción: Obtiene el rango (mínimo y máximo) de una lista
-- Parámetros: Una lista de elementos comparables
-- Retorno: Una lista con el mínimo y máximo de la lista original
rango :: (Ord a) => [a] -> [a]
rango xs = [minimum xs, maximum xs]

-- Ejercicio 10
-- Descripción: Verifica si una lista es un palíndromo
-- Parámetros: Una lista de elementos comparables
-- Retorno: True si es palíndromo, False en caso contrario
palindromo :: (Eq a) => [a] -> Bool
palindromo xs = xs == reverse xs

-- Ejercicio 11
-- Descripción: Obtiene el interior de una lista (todos los elementos excepto el primero y el último)
-- Parámetros: Una lista de elementos de cualquier tipo
-- Retorno: La lista sin el primer y último elemento
interior :: [a] -> [a]
interior xs = tail (init xs)

-- Ejercicio 13
-- Descripción: Obtiene un segmento de una lista entre dos índices
-- Parámetros: Dos enteros (m y n) y una lista de elementos de cualquier tipo
-- Retorno: El segmento de la lista entre los índices m y n (inclusive)
segmento :: Int -> Int -> [a] -> [a]
segmento m n xs = take (n - m + 1) (drop (m - 1) xs)

-- Ejercicio 14
-- Descripción: Obtiene los n primeros y n últimos elementos de una lista
-- Parámetros: Un entero n y una lista de elementos de cualquier tipo
-- Retorno: Una lista con los n primeros y n últimos elementos de la lista original
extremos :: Int -> [a] -> [a]
extremos n xs = take n xs ++ drop (length xs - n) xs

-- Ejercicio 15
-- Descripción: Encuentra el valor mediano de tres números
-- Parámetros: Tres valores comparables
-- Retorno: El valor mediano de los tres
mediano :: (Ord a) => a -> a -> a -> a
mediano x y z = head (drop 1 (sort [x, y, z]))
  where
    sort = foldr insert []
    insert x [] = [x]
    insert x (y : ys)
      | x <= y = x : y : ys
      | otherwise = y : insert x ys

-- Ejercicio 16
-- Descripción: Verifica si tres valores son iguales
-- Parámetros: Tres valores comparables
-- Retorno: True si los tres valores son iguales, False en caso contrario
tresIguales :: (Eq a) => a -> a -> a -> Bool
tresIguales x y z = x == y && y == z

-- Ejercicio 17
-- Descripción: Verifica si tres valores son diferentes entre sí
-- Parámetros: Tres valores comparables
-- Retorno: True si los tres valores son diferentes entre sí, False en caso contrario
tresDiferentes :: (Eq a) => a -> a -> a -> Bool
tresDiferentes x y z = x /= y && y /= z && x /= z

-- Ejercicio 18
-- Descripción: Verifica si cuatro valores son iguales
-- Parámetros: Cuatro valores comparables
-- Retorno: True si los cuatro valores son iguales, False en caso contrario
cuatroIguales :: (Eq a) => a -> a -> a -> a -> Bool
cuatroIguales x y z u = tresIguales x y z && z == u


------------------------------------------------------
--  Guardas y Patrones
--------------------------------------------------------

-- Ejercicio 1
-- Descripción: Realiza una división segura (evita división por cero)
-- Parámetros: Dos números de tipo Double (dividendo y divisor)
-- Retorno: El resultado de la división o 9999.0 si el divisor es cero
divisionSegura :: Double -> Double -> Double
divisionSegura x y
  | y /= 0 = x / y
  | otherwise = 9999.0

-- Ejercicio 2
-- Descripción: Implementa la operación XOR (O exclusivo)
-- Parámetros: Dos valores booleanos
-- Retorno: True si exactamente uno de los valores es True, False en caso contrario
xor1 :: Bool -> Bool -> Bool
xor1 True False = True
xor1 False True = True
xor1 _ _ = False

-- Ejercicio 3
-- Descripción: Compara dos rectángulos y devuelve el de mayor área
-- Parámetros: Dos tuplas, cada una representando un rectángulo (base, altura)
-- Retorno: La tupla del rectángulo con mayor área
mayorRectangulo :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
mayorRectangulo r1@(b1, h1) r2@(b2, h2)
  | b1 * h1 >= b2 * h2 = r1
  | otherwise = r2

-- Ejercicio 4
-- Descripción: Intercambia los elementos de una tupla
-- Parámetros: Una tupla de dos elementos
-- Retorno: Una nueva tupla con los elementos intercambiados
intercambia :: (a, b) -> (b, a)
intercambia (x, y) = (y, x)

-- Ejercicio 5
-- Descripción: Calcula la distancia entre dos puntos en un plano
-- Parámetros: Dos tuplas, cada una representando un punto (x, y)
-- Retorno: La distancia entre los dos puntos como Double
distancia :: (Double, Double) -> (Double, Double) -> Double
distancia (x1, y1) (x2, y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

-- Ejercicio 6
-- Descripción: Realiza un ciclo en una lista (mueve el último elemento al principio)
-- Parámetros: Una lista de elementos de cualquier tipo
-- Retorno: La lista con el último elemento movido al principio
ciclo :: [a] -> [a]
ciclo [] = []
ciclo xs = last xs : init xs

-- Ejercicio 7
-- Descripción: Encuentra el número mayor entre dos dígitos combinados
-- Parámetros: Dos números de un solo dígito
-- Retorno: El número mayor formado por la combinación de los dos dígitos
numeroMayor :: (Num a, Ord a) => a -> a -> a
numeroMayor x y = max (10 * x + y) (10 * y + x)

-- Ejercicio 8
-- Descripción: Determina el número de raíces de una ecuación cuadrática
-- Parámetros: Tres coeficientes (a, b, c) de la ecuación ax^2 + bx + c = 0
-- Retorno: El número de raíces reales (0, 1 o 2)
numeroDeRaices :: (Floating t, Ord t) => t -> t -> t -> Int
numeroDeRaices a b c
  | discriminante > 0 = 2
  | discriminante == 0 = 1
  | otherwise = 0
  where
    discriminante = b ^ 2 - 4 * a * c

-- Ejercicio 9
-- Descripción: Calcula las raíces de una ecuación cuadrática
-- Parámetros: Tres coeficientes (a, b, c) de la ecuación ax^2 + bx + c = 0
-- Retorno: Una lista con las raíces reales (puede estar vacía, tener una o dos raíces)
raices :: Double -> Double -> Double -> [Double]
raices a b c
  | disc > 0 = [(-b + sqrt disc) / (2 * a), (-b - sqrt disc) / (2 * a)]
  | disc == 0 = [-b / (2 * a)]
  | otherwise = []
  where
    disc = b ^ 2 - 4 * a * c

-- Ejercicio 10
-- Descripción: Calcula el área de un triángulo usando la fórmula de Herón
-- Parámetros: Tres lados del triángulo (a, b, c)
-- Retorno: El área del triángulo como Double
area :: Double -> Double -> Double -> Double
area a b c = sqrt (s * (s - a) * (s - b) * (s - c))
  where
    s = (a + b + c) / 2

-- Ejercicio 11
-- Descripción: Calcula la intersección de dos intervalos
-- Parámetros: Dos listas, cada una representando un intervalo [inicio, fin]
-- Retorno: Una lista representando el intervalo de intersección (vacía si no hay intersección)
interseccion :: (Ord a) => [a] -> [a] -> [a]
interseccion [a, b] [c, d]
  | b < c || d < a = []
  | otherwise = [max a c, min b d]
interseccion _ _ = []

-- Ejercicio 12
-- Descripción: Genera una línea de números en secuencia
-- Parámetros: Un entero n
-- Retorno: Una lista de enteros representando la n-ésima línea de una secuencia triangular
linea :: Integer -> [Integer]
linea n = [s + 1 .. s + n]
  where
    s = sum [1 .. n - 1]

-------------------------------------------- --
--Recursividad
------------------------------------------------
-- Ejercicio 1
-- Descripción: Calcula la potencia de un número
-- Parámetros: Base (x) y exponente (n)
-- Retorno: El resultado de x elevado a la n
potencia :: Integer -> Integer -> Integer
potencia _ 0 = 1
potencia x n = x * potencia x (n - 1)

-- Ejercicio 2
-- Descripción: Calcula el máximo común divisor de dos números
-- Parámetros: Dos números enteros
-- Retorno: El máximo común divisor de los dos números
mcd :: Integer -> Integer -> Integer
mcd a 0 = a
mcd a b = mcd b (a `mod` b)

-- Ejercicio 3
-- Descripción: Verifica si un elemento pertenece a una lista
-- Parámetros: Un elemento y una lista
-- Retorno: True si el elemento está en la lista, False en caso contrario
pertenece :: (Eq a) => a -> [a] -> Bool
pertenece _ [] = False
pertenece x (y : ys)
  | x == y = True
  | otherwise = pertenece x ys

-- Ejercicio 4
-- Descripción: Toma los primeros n elementos de una lista
-- Parámetros: Un entero n y una lista
-- Retorno: Una lista con los primeros n elementos de la lista original
tomar :: Int -> [a] -> [a]
tomar _ [] = []
tomar 0 _ = []
tomar n (x : xs) = x : tomar (n - 1) xs

-- Ejercicio 5
-- Descripción: Convierte un número entero en una lista de sus dígitos
-- Parámetros: Un número entero
-- Retorno: Una lista de enteros, cada uno representando un dígito del número
digitosC :: Integer -> [Integer]
digitosC n = [read [x] | x <- show n]

-- Ejercicio 6
-- Descripción: Suma los dígitos de un número entero
-- Parámetros: Un número entero
-- Retorno: La suma de los dígitos del número
sumaDigitosR :: Integer -> Integer
sumaDigitosR n
  | n < 10 = n
  | otherwise = (n `mod` 10) + sumaDigitosR (n `div` 10)

-- Ejercicio 2.1
-- Descripción: Implementa el algoritmo de ordenamiento rápido (QuickSort)
-- Parámetros: Una lista de elementos comparables
-- Retorno: La lista ordenada
ordenaRapida :: (Ord a) => [a] -> [a]
ordenaRapida [] = []
ordenaRapida (x : xs) =
  ordenaRapida menores ++ [x] ++ ordenaRapida mayores
  where
    menores = [y | y <- xs, y <= x]
    mayores = [y | y <- xs, y > x]

-- -------------------------
--  Tipos de Datos
---------------------------


-- Definición del tipo Estudiante
data Estudiante = Estudiante {nombre :: String, apellido :: String, edad :: Int, numeroControl :: String} deriving (Show, Eq)

-- Lista de estudiantes
estudiantes :: [Estudiante]
estudiantes =
  [ Estudiante "Diego" "Hernandez" 18 "22170815",
    Estudiante "Lucia" "Mendoza" 23 "22170816",
    Estudiante "Raul" "Jimenez" 20 "22170055",
    Estudiante "Isabel" "Ortega" 22 "22170056",
    Estudiante "Fernando" "Vargas" 24 "22170057",
    Estudiante "Gabriela" "Nunez" 19 "22170058",
    Estudiante "Roberto" "Silva" 21 "22170059",
    Estudiante "Patricia" "Morales" 22 "22170060",
    Estudiante "Jorge" "Castillo" 25 "22170061",
    Estudiante "Valeria" "Cruz" 20 "22170062"
  ]


-- Descripción: Ordena una lista de estudiantes por edad usando QuickSort
-- Parámetros: Una lista de estudiantes
-- Retorno: La lista de estudiantes ordenada por edad
quickSort :: [Estudiante] -> [Estudiante]
quickSort [] = []
quickSort (x : xs) =
  quickSort menores ++ [x] ++ quickSort mayores
  where
    menores = [y | y <- xs, edad y <= edad x]
    mayores = [y | y <- xs, edad y > edad x]

-- Descripción: Ordena una lista de estudiantes por edad
-- Parámetros: Una lista de estudiantes
-- Retorno: La lista de estudiantes ordenada por edad
ordenarPorEdad :: [Estudiante] -> [Estudiante]
ordenarPorEdad = quickSort

-- Descripción: Encuentra el estudiante de menor edad en una lista
-- Parámetros: Una lista de estudiantes
-- Retorno: El estudiante de menor edad
estudianteMenor :: [Estudiante] -> Estudiante
estudianteMenor [x] = x
estudianteMenor (x : xs)
  | edad x <= edad menorResto = x
  | otherwise = menorResto
  where
    menorResto = estudianteMenor xs

-- Descripción: Encuentra el estudiante de mayor edad en una lista
-- Parámetros: Una lista de estudiantes
-- Retorno: El estudiante de mayor edad
estudianteMayor :: [Estudiante] -> Estudiante
estudianteMayor [x] = x
estudianteMayor (x : xs)
  | edad x >= edad mayorResto = x
  | otherwise = mayorResto
  where
    mayorResto = estudianteMayor xs

-- Descripción: Calcula la suma de las edades de los estudiantes
-- Parámetros: Una lista de estudiantes
-- Retorno: La suma de las edades como Int
sumaEdades :: [Estudiante] -> Int
sumaEdades [] = 0
sumaEdades (x : xs) = edad x + sumaEdades xs

-- Descripción: Cuenta el número de estudiantes en una lista
-- Parámetros: Una lista de estudiantes
-- Retorno: El número de estudiantes como Int
contarEstudiantes :: [Estudiante] -> Int
contarEstudiantes [] = 0
contarEstudiantes (_ : xs) = 1 + contarEstudiantes xs

-- Descripción: Calcula el promedio de edades de los estudiantes
-- Parámetros: Una lista de estudiantes
-- Retorno: El promedio de edades como Double
promedioEdades :: [Estudiante] -> Double
promedioEdades es = fromIntegral (sumaEdades es) / fromIntegral (contarEstudiantes es)

---------------------------
--  Árboles
---------------------------

-- Definición del tipo de dato Árbol
data Arbol a = Hoja | Nodo a (Arbol a) (Arbol a) deriving (Show, Eq)

-- Descripción: Genera un nodo hoja con un valor
-- Parámetros: Un valor de cualquier tipo
-- Retorno: Un árbol con un solo nodo (hoja)
generarNodo :: a -> Arbol a
generarNodo x = Nodo x Hoja Hoja

-- Descripción: Inserta un elemento en el árbol binario de búsqueda
-- Parámetros: Un elemento y un árbol
-- Retorno: El árbol actualizado con el nuevo elemento
insertar :: (Ord a) => a -> Arbol a -> Arbol a
insertar x Hoja = generarNodo x
insertar x (Nodo a izq der)
  | x < a = Nodo a (insertar x izq) der
  | x > a = Nodo a izq (insertar x der)
  | otherwise = Nodo a izq der

-- Descripción: Inserta múltiples elementos en el árbol desde un arreglo
-- Parámetros: Una lista de elementos y un árbol
-- Retorno: El árbol actualizado con todos los nuevos elementos
insertarDesdeArreglo :: (Ord a) => [a] -> Arbol a -> Arbol a
insertarDesdeArreglo [] arbol = arbol
insertarDesdeArreglo (x : xs) arbol = insertarDesdeArreglo xs (insertar x arbol)

-- Descripción: Busca un elemento en el árbol
-- Parámetros: Un elemento y un árbol
-- Retorno: True si el elemento está en el árbol, False en caso contrario
buscar :: (Ord a) => a -> Arbol a -> Bool
buscar _ Hoja = False
buscar x (Nodo a izq der)
  | x == a = True
  | x < a = buscar x izq
  | otherwise = buscar x der

-- Descripción: Realiza un recorrido inorden del árbol
-- Parámetros: Un árbol
-- Retorno: Una lista con los elementos del árbol en orden inorden
inorden :: Arbol a -> [a]
inorden Hoja = []
inorden (Nodo a izq der) = inorden izq ++ [a] ++ inorden der

-- Descripción: Realiza un recorrido preorden del árbol
-- Parámetros: Un árbol
-- Retorno: Una lista con los elementos del árbol en orden preorden
preorden :: Arbol a -> [a]
preorden Hoja = []
preorden (Nodo a izq der) = [a] ++ preorden izq ++ preorden der

-- Descripción: Realiza un recorrido postorden del árbol
-- Parámetros: Un árbol
-- Retorno: Una lista con los elementos del árbol en orden postorden
postorden :: Arbol a -> [a]
postorden Hoja = []
postorden (Nodo a izq der) = postorden izq ++ postorden der ++ [a]
