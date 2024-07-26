-- No se permite agregar nuevos imports
-- Sólo está permitido usar estas funciones:
-- https://campus.exactas.uba.ar/pluginfile.php/557895/mod_resource/content/1/validas_tp.pdf

-- Nombre de grupo: Sipinetta jede
-- Integrante1: 38786703 | Nyari, Lisandro  | lisandronyari@gmail.com
-- Integrante2: 44284061 | Pacheco Parrondo, Geronimo Gabriel  | pachecogero16@gmail.com
-- Integrante3: 45041759 | Cuestas, Martin Ivan  | martincuestas51@gmail.com
-- Integrante4: 44082641 | Suter, Matias Agustin | suter.uni9@gmail.com

-- Integrantes que abandonaron la materia: 

module Solucion where
import Data.Char

-- caracter = 'a' = Char
-- texto = "dak{}[]324" = String
-- textos = [texto]

-- EJ 1
esMinuscula :: Char -> Bool
esMinuscula caracter = ord 'a' <= ord caracter && ord caracter <= ord 'z'

-- EJ 2
letraANatural :: Char -> Int
letraANatural letra = ord letra - ord 'a'

-- EJ 3
desplazar :: Char -> Int -> Char
desplazar caracter desplazamiento
    | not (esMinuscula caracter) = caracter
    | posicionFinal > 25 = desplazar 'a' (posicionFinal - 26) 
    | posicionFinal < 0 = desplazar 'a' (posicionFinal + 26) 
    | otherwise = naturalALetra posicionFinal
        where posicionFinal = letraANatural caracter + desplazamiento

naturalALetra :: Int -> Char -- 0 <= num <= 25
naturalALetra num = chr (num + ord 'a')

-- EJ 4 
cifrar :: String -> Int -> String
cifrar [] _ = []
cifrar (caracter:cs) desplazamiento = desplazar caracter desplazamiento : cifrar cs desplazamiento

-- EJ 5 
descifrar :: String -> Int -> String
descifrar [] _ = []
descifrar (caracter:cs) desplazamiento = desplazar caracter (-desplazamiento) : descifrar cs desplazamiento

-- EJ 6
cifrarLista :: [String] -> [String]
cifrarLista textos = cifrarListaAux textos 0

cifrarListaAux :: [String] -> Int -> [String]
cifrarListaAux [] _ = []
cifrarListaAux (texto:ts) posicion  = cifrar texto posicion : cifrarListaAux ts (posicion+1)

-- EJ 7
frecuencia :: String -> [Float]
frecuencia texto = frecuenciaAux 'a' texto 

frecuenciaAux :: Char -> String -> [Float] 
frecuenciaAux letra texto  | letra == 'z' = [porcentajeDe 'z' texto]
                           | otherwise = porcentajeDe letra texto : frecuenciaAux (desplazar letra 1) texto 

porcentajeDe :: Char -> String -> Float
porcentajeDe letra texto | cantidadMinusculas texto == 0 = 0 -- para evitar dividir por 0
                         | otherwise = cantidadApariciones letra texto `division` cantidadMinusculas texto * 100

division :: Int -> Int -> Float 
division a b = fromIntegral a / fromIntegral b

cantidadApariciones :: (Eq t) => t -> [t] -> Int
cantidadApariciones _ [] = 0
cantidadApariciones e (x:xs) | e == x = 1 + cantidadApariciones e xs
                             | otherwise = cantidadApariciones e xs

cantidadMinusculas :: String -> Int
cantidadMinusculas [] = 0
cantidadMinusculas (caracter:cs) | esMinuscula caracter = 1 + cantidadMinusculas cs
                                 | otherwise = cantidadMinusculas cs

-- EJ 8
cifradoMasFrecuente :: String -> Int -> (Char, Float)
cifradoMasFrecuente texto desplazamiento = cifradoMasFrecuenteAux (cifrar texto desplazamiento)

cifradoMasFrecuenteAux :: String -> (Char, Float)
cifradoMasFrecuenteAux cifrado = (naturalALetra (obtenerPosicion maxFrecuencia (frecuencia cifrado)), maxFrecuencia)
    where maxFrecuencia = maximo (frecuencia cifrado)

maximo :: (Num t, Ord t) => [t] -> t
maximo [x] = x
maximo (x:y:xs) | x >= y = maximo (x:xs)
                | otherwise = maximo (y:xs)

obtenerPosicion :: (Eq t) => t -> [t] -> Int -- Empieza desde 0
obtenerPosicion e (x:xs) | e == x = 0
                         | otherwise = 1 + obtenerPosicion e xs

-- EJ 9
esDescifrado :: String -> String -> Bool
esDescifrado texto1 texto2 = esDescifradoAux texto1 texto2 0

esDescifradoAux :: String -> String -> Int -> Bool
esDescifradoAux _ _ 26 = False
esDescifradoAux texto1 texto2 desplazamiento = texto1 == cifrar texto2 desplazamiento || esDescifradoAux texto1 texto2 (desplazamiento+1)

-- EJ 10 -- preguntar por el asegura
todosLosDescifrados :: [String] -> [(String, String)]
todosLosDescifrados ls =  todosLosDescifradosAux ls ls

todosLosDescifradosAux :: [String] -> [String] -> [(String, String)]
todosLosDescifradosAux [] _ = []
todosLosDescifradosAux (palabra:ps) listaOriginal = obtenerDescifradosDe palabra (quitar palabra listaOriginal) ++ todosLosDescifradosAux ps listaOriginal
 
obtenerDescifradosDe :: String -> [String] -> [(String, String)]
obtenerDescifradosDe _ [] = []
obtenerDescifradosDe palabra1 (palabra2:ps) | esDescifrado palabra1 palabra2 = (palabra1, palabra2) : obtenerDescifradosDe palabra1 ps
                                            | otherwise = obtenerDescifradosDe palabra1 ps

quitar :: (Eq t) => t -> [t] -> [t]
quitar _ [] = []
quitar e (x:xs) | e == x = xs
                | otherwise = x : quitar e xs

-- EJ 11
expandirClave :: String -> Int -> String
expandirClave clave longitud = expandirClaveAux clave clave longitud

expandirClaveAux :: String -> String -> Int -> String
expandirClaveAux [] claveOriginal longitud = expandirClaveAux claveOriginal claveOriginal longitud -- si longitud no es 0 y me quede sin letras en clave, le reasigno su valor original
expandirClaveAux (letra:ls) claveOriginal longitud | longitud == 0 = [] -- Caso base
                                                   | otherwise = letra : expandirClaveAux ls claveOriginal (longitud-1)
-- EJ 12
cifrarVigenere :: String -> String -> String
cifrarVigenere texto clave = cifrarVigenereAux texto (expandirClave clave (length texto))

cifrarVigenereAux :: String -> String -> String
cifrarVigenereAux [] _ = [] 
cifrarVigenereAux (caracter:cs) (minuscula:ms) = desplazar caracter (letraANatural minuscula) : cifrarVigenereAux cs ms 

-- EJ 13
descifrarVigenere :: String -> String -> String -- duda: que pasa si clave = []?
-- descifrarVigenere s [] = s
descifrarVigenere texto clave = descifrarVigenereAux texto (expandirClave clave (length texto))

descifrarVigenereAux :: String -> String -> String
descifrarVigenereAux [] _ = [] 
descifrarVigenereAux (caracter:cs) (minuscula:ms) = desplazar caracter (-(letraANatural minuscula)) : descifrarVigenereAux cs ms

-- EJ 14
peorCifrado :: String -> [String] -> String
peorCifrado _ [clave] = clave 
peorCifrado texto (clave1:clave2:cs) | distancia texto (cifrarVigenere texto clave1) <= distancia texto (cifrarVigenere texto clave2) = peorCifrado texto (clave1:cs)
                                     | otherwise = peorCifrado texto (clave2:cs)

distancia :: String -> String -> Int
distancia [] [] = 0
distancia (letra1:xs) (letra2:ys) = absoluto (letraANatural letra1 - letraANatural letra2) + distancia xs ys

absoluto :: Int -> Int
absoluto n | n < 0 = -n
           | otherwise = n

-- EJ 15
combinacionesVigenere :: [String] -> [String] -> String -> [(String, String)]
combinacionesVigenere [] _ _ = []
combinacionesVigenere (msj:msjs) claves cifrado = buscarCombinaciones msj claves cifrado ++ combinacionesVigenere msjs claves cifrado

buscarCombinaciones :: String -> [String] -> String -> [(String, String)]
buscarCombinaciones _ [] _ = []
buscarCombinaciones msj (clave:cs) cifrado | cifrarVigenere msj clave == cifrado = (msj, clave) : buscarCombinaciones msj cs cifrado
                                           | otherwise = buscarCombinaciones msj cs cifrado
