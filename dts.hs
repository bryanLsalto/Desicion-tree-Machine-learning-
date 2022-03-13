data Tree a = Node a [Tree a] deriving (Show)

-- Matris filas columnas: Construye un arbol con la matris de entrada y una lista con sus filas y columnas
construir :: [[String]] -> [Int] -> [Int] -> Tree String
construir m f c = Node (show a) (hijos m  f c l a)
    where
          (x:xs) = calheugen $ itter m (takecol m [] f 0) f c
          aux = minimum (x:xs)
          a = minim (x:xs) aux c
          l = takecol m [] f a

-- Matris Filas Columas Strings_columna_a quitar Columna a quitar: Analicis de los hijos de un arbol. Si son predecibles se añaden las ojas, sino se añade sus subarboles
hijos :: [[String]] -> [Int] -> [Int] -> [String] -> Int -> [Tree String]
hijos _ _ _ [] _ = []
hijos m f c (l:s) q
    | predecible m (x:xs) = [Node l [Node (selectcas m x 0) []]] ++ (hijos m f c s q)
    |otherwise = [Node l [construir m (x:xs) aux]] ++ (hijos m f c s q)
        where
            (x:xs)  = quitarcol m f q l
            aux     = delete q c

--Matris filas: retorna si la submatris por esas filas es predecible.
predecible :: [[String]] -> [Int] -> Bool
predecible m f
    |1 == (length $ takecol m [] f 0) = True
    |otherwise = False

--              matriz       f      quitar  letras : retorna filas  al tener una columna y una string como condicion
quitarcol :: [[String]] -> [Int] -> Int -> String -> [Int]
quitarcol m f  q l =  a
      where
          a = selectfilas m l f q []

--                m          quitar    filas    col    resul
selectfilas :: [[String]] -> String -> [Int] -> Int -> [Int] -> [Int]
selectfilas _ _ [] _ l = l
selectfilas m q (f:fs) c l
    |selectcas m f c == q =  selectfilas m q fs c (l ++ [f])
    |otherwise = selectfilas m q fs c l


--bucle que repite la funcion recorrer
otra :: Tree String -> IO ()
otra a = do
    putStrLn "Quieres classificar otra zeta [s/n]"
    aux <- getLine
    if aux == "s" then do
        recorrer a
        otra a
    else
        return()

--Recorre los nodos que corresponde a las columnas de un arbol segun los valores de entrada
recorrer :: Tree String -> IO ()
recorrer (Node a []) = do
    putStrLn ("la zeta es " ++ a)
    putStrLn  "---------------------------------------------------------------------------------"
    putStrLn  "---------------------------------------------------------------------------------"
    return()
recorrer (Node a xs) = do
    putStrLn  "---------------------------------------------------------------------------------"
    putStrLn ("Estas en la columna " ++ a ++ " selecciona una de las posibilidades:")
    llistpos xs
    aux <- getLine
    let b = selectTree xs aux
    if value b == "error" then
      recorrer (Node a xs)
    else
      entrar b

entrar :: Tree String -> IO ()
entrar (Node a (x:xs)) = do
    recorrer x

value :: Tree String -> String
value (Node a _) = a

--Retorna un arbol de una lista de arboles con el valor del nodo igual al valor pasado por la función
selectTree :: [Tree String] -> String -> Tree String
selectTree [] a = (Node ("error") [])
selectTree (x:xs) n
    |value x == n = x
    | otherwise = selectTree xs n

--Imprime los valores de todos los nodos de un vector de arboles
llistpos :: [Tree String] -> IO ()
llistpos [] = return()
llistpos (x:xs) = do
    putStrLn $ ("-> " ++ value x)
    llistpos xs

lenghtf :: String -> Int
lenghtf x = length $ lines x


lenghtc :: String -> Int
lenghtc x = length $ words $ head $ lines x

--Calcula la hueristica de cada columna
calheugen :: [[[Int]]] -> [Float]
calheugen [] = []
calheugen (x:xs) = [a] ++ calheugen xs
    where
      a = calheucol x (total x)

--Retorna la suma de todos los elementos
total :: [[Int]] -> Int
total [] = 0
total (x:xs) = total xs + sum x

--con el conteo de las repeticiones de una columna:  retorna su hueristica
calheucol :: [[Int]] -> Int -> Float
calheucol [] _ = 0
calheucol (x:xs) t = e*(u/g) + calheucol xs t
    where
        g = fromIntegral t :: Float
        s = sum x
        u = fromIntegral s :: Float
        e = heuristicaP x s

--Calculo más basico de una heristica
heuristicaP :: [Int] -> Int -> Float
heuristicaP [] _ = 0
heuristicaP  (x:xs) s
    | x /= 0 = a + (heuristicaP xs s)
    |otherwise = (heuristicaP xs s)
        where
            x1 = fromIntegral x :: Float
            t = fromIntegral s :: Float
            a = -x1/t * logBase 2 (x1/t)
----------------------------------------------------------------------------------------------------------

--calcula las repeticiones de cada tipo en cada elemento y los divide entre los elementos de la primera columna
--para calcular la entropia de cada una
itter :: [[String]] -> [String] -> [Int] -> [Int] -> [[[Int]]]
itter _ _ _ [] = []
itter m m0 f (c:cs) = [a] ++ (itter m m0 f cs)
    where
        letras = takecol m [] f c
        a = contador m m0 letras f c (iniciador (length letras) (length m0))

--inicializa el contador con el tamaño adecuado para ir contenado los elementos
iniciador :: Int -> Int -> [[Int]]
iniciador 0 _  = []
iniciador a b = iniciadorIn b : iniciador (a-1) b

iniciadorIn :: Int -> [Int]
iniciadorIn 0 = []
iniciadorIn a = 0 : (iniciadorIn (a - 1))


--cuaenta los elementos de cada tipo en una columa, y los distingue entre los elementos de la columna 0 
--              m            m0         letras     filas  columnas retorno
contador :: [[String]] -> [String] -> [String] -> [Int] -> Int -> [[Int]] -> [[Int]]
contador _ _ _ [] _ aux = aux
contador m m0 l (f:fs) c r = contador m m0 l fs c aux
    where
        aux = actualizar l m0 (selectcas m f c) (selectcas m f 0) r

--Actualiza un valor de la lista de lleva el conteo de las repeticiones
actualizar :: [String] -> [String] -> String -> String -> [[Int]] -> [[Int]]
actualizar (l:ls) m0 le m (x:xs)
    |l == le = (actualizarIn m0 m x) : xs
    |otherwise = x :  (actualizar ls m0 le m xs)

actualizarIn :: [String] -> String -> [Int] -> [Int]
actualizarIn [] _ a = a
actualizarIn (l:ls) le (x:xs)
    |l == le = [x+1] ++ xs
    |otherwise = [x] ++ actualizarIn ls le xs

-----------------------------------------------------------------------------------------------
--- matris []  filas columna: Retorna los diferentes elementos de una columna
takecol :: [[String]] -> [String]-> [Int] -> Int -> [String]
takecol _ l [] _ = l
takecol m r (f:fs) c
    | elem (selectcas m f c) r == False = takecol m (r ++ [selectcas m f c]) fs c
    |otherwise = takecol m r fs c

-- retorna un elemento de la matris
selectcas ::  [[String]] -> Int -> Int -> String
selectcas m f c = a
    where
        (x:xs) = drop f m
        a  = head $ drop c x

--Crea una matris, vector de vectores
matriz :: [String] -> [[String]]
matriz [] = []
matriz (x:xs) = [words x ] ++ matriz xs


--Elimina elemento de un vector
delete :: Int -> [Int] -> [Int]
delete n (x:xs)
    | n == x = xs
    | otherwise = x : (delete n xs)

--Imprimen los arboles de una manera ordenada. Casa nivel de un arbol tiene un espació más
imprimir :: Tree String -> String -> String
imprimir (Node a []) esp = "\n"++ esp ++ a
imprimir (Node a (x:xs)) esp =  "\n"++ esp ++ a ++ (imprimirh (x:xs) (esp ++ "  "))

imprimirh :: [Tree String] -> String -> String
imprimirh [] _ = ""
imprimirh (x:xs) esp = (imprimir x esp) ++ imprimirh xs esp


minim :: [Float] -> Float -> [Int] -> Int
minim (x:xs) n (y:ys)
    | x == n = y
    |otherwise = minim xs n ys

main :: IO ()
main = do

  text <- readFile "agaricus-lepiota.data"
  putStrLn $ "Hola, se esta creando el arbol, espera un momento por favor:"

  let arbol = construir (matriz $ lines text) [0 .. ((lenghtf text)-1)] [1 ..((lenghtc text)-1)]
  putStrLn  "---------------------------------------------------------------------------------"
  putStrLn $ imprimir arbol []
  putStrLn  "---------------------------------------------------------------------------------"
  putStrLn "Ahora vamos a classificar una zeta, cada columna es un tipo de classificación enviado por la matriz, escoje el tipo al que pertenesca tu dato para llegar a classificarlo."
  recorrer arbol
  otra arbol
