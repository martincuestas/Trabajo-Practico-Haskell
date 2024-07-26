-- duda: El archivo de nuestro test tiene que tener algun nombre en especifico?

import Test.HUnit
import Solucion
import Data.List

--import Data.List estaba dos veces por algo?
-- No está permitido agregar nuevos imports.

runTestsPropios = runTestTT allTests

allTests = test [
    "esMinuscula" ~: testSuiteEsMinuscula,
    "letraANatural" ~: testsSuiteletraANatural,
    "desplazar" ~: testSuiteDesplazar,
    "cifrar" ~:  testSuitEjcifrar,
    "descifrar" ~: testsSuitEjdescifrar,
    "cifrarLista" ~: testsEjcifrarLista,
    "frecuencia" ~: testSuiteFrecuencia,
    -- "cifradoMasFrecuente" ~: testsEjcifradoMasFrecuente,
    -- "esDescifrado" ~: testsEjesDescifrado,
    -- "todosLosDescifrados" ~: testsEjtodosLosDescifrados,
    "expandirClave" ~: testSuiteExpandirClave,
    "cifrarVigenere" ~: testSuiteCifrarVigenere,
    "descifrarVigenere" ~: testSuiteDescifrarVigenere,
    "peorCifrado" ~: testSuitePeorCifrado,
    "combinacionesVigenere" ~: testSuiteCombinacionesVigenere
    ]


 testSuiteEsMinuscula = test [
    "a" ~: esMinuscula 'a' ~?= True,
    "z" ~: esMinuscula 'z' ~?= True,
    "d" ~: esMinuscula 'd' ~?= True,
    "ñ" ~: esMinuscula 'ñ' ~?= False,
    "à" ~: esMinuscula 'à' ~?= False,
    "A" ~: esMinuscula 'A' ~?= False,
    "@" ~: esMinuscula '@' ~?= False,
    ]

testsSuiteletraANatural = test [
   "a" ~: letraANatural 'a' ~?= 0,
   "z" ~: letraANatural 'z' ~?= 25,
   "ñ" ~: letraANatural 'ñ' ~?= 14,
   "b" ~: letraANatural 'b' ~?= 1,
   "/" ~: letraANatural '/' ~?= "Error, fuera del dominio",
   "A" ~: letraANatural 'A' ~?= "Error, fuera del dominio", 
    ]
-- EJ 3
testSuiteDesplazar = test [
    "c no es minuscula" ~: desplazar 'B' 4 ~?= 'B',
    "n = 0" ~: desplazar 'g' 0 ~?= 'g',
    "n > 0" ~: desplazar 'g' 4 ~?= 'k',
    "n < 0" ~: desplazar 'g' (-4) ~?= 'c',
    "n > 0 y rota" ~: desplazar 'g' 55 ~?= 'j',
    "n < 0 y rota" ~: desplazar 'g' (-55) ~?= 'd',
    ]
-- EJ 4
 testSuitEjcifrar = test [
     "ejemploPositivo" ~: cifrar "computacion" 3 ~?= "frpsxwdflrq",
     "ejemploNegativo" ~: cifrar "computacion" (-3) ~?= "frpsxwdflrq",
     "exceder26Positivo" ~: cifrar "computacion" 29 ~?= "frpsxwdflrq",
     "exceder26Negativo" ~: cifrar "computacion" (-23) ~?= "frpsxwdflrq",
     "cero" ~: cifrar "computacion" 0 ~?= "computacion",
     "espacio" ~: cifrar " " 3 ~?= " ",
     "mayusculas" ~: cifrar "cOmPut4C1ón" 3 ~?= "fOpPxw4C1óq",
     ]
-- EJ 5
testsSuitEjdescifrar = test [
     "ejemploPositivo" ~: descifrar "frpsxwdflrq" 3 ~?= "computacion",
     "ejemploNegativo" ~: descifrar "frpsxwdflrq" (-3) ~?= "computacion",
     "exceder26Positivo" ~: descifrar "frpsxwdflrq" 29 ~?= "computacion",
     "exceder26Negativo" ~: descifrar "frpsxwdflrq" (-23) ~?= "computacion",
     "cero" ~: descifrar "frpsxwdflrq" 0 ~?= "frpsxwdflrq",
     "espacio" ~: descifrar " " 3 ~?= " ",
     "mayusculas" ~: descifrar "fOpPxw4C1óq" 3 ~?= "cOmPut4C1ón",
    ]

-- testsEjcifrarLista = test [
--     cifrarLista ["compu", "labo", "intro"] ~?= ["compu", "mbcp", "kpvtq"]
--     ]

testSuiteFrecuencia = test [
    "s = \"\"" ~: frecuencia "" ~?= [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
    "Sin minusculas" ~: frecuencia "ABC[]Z(&)" ~?= [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
    "100% 'a'" ~: frecuencia "aaaaaa" ~?= [100,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
    "Todo minusculas" ~: expectlistProximity (frecuencia "haskell") [14.285714,0.0,0.0,0.0,14.285714,0.0,0.0,14.285714,0.0,0.0,14.285714,28.571429,0.0,0.0,0.0,0.0,0.0,0.0,14.285714,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
    "Minusculas y no minusculas" ~: expectlistProximity (frecuencia "[ha!sQkelPl]") [14.285714,0.0,0.0,0.0,14.285714,0.0,0.0,14.285714,0.0,0.0,14.285714,28.571429,0.0,0.0,0.0,0.0,0.0,0.0,14.285714,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
    ]
-- 7
-- testsEjcifradoMasFrecuente = test [
--     cifradoMasFrecuente "taller" 3 ~?= ('o', 33.333336)
--     ]

-- testsEjesDescifrado = test [
--     esDescifrado "taller" "compu" ~?= False
--     ]

-- testsEjtodosLosDescifrados = test [
--     todosLosDescifrados ["compu", "frpsx", "mywza"] ~?= [("compu", "frpsx"), ("frpsx", "compu")]
--     ]

testSuiteExpandirClave = test [
    "|clave| < n" ~: expandirClave "compu" 9 ~?= "compucomp",
    "|clave| > n" ~: expandirClave "compu" 3 ~?= "com",
    "|clave| == n" ~: expandirClave "compu" 5 ~?= "compu"
    ]

testSuiteCifrarVigenere = test [
    "s = string vacio" ~: cifrarVigenere "" "labo" ~?= "",
    "s no tiene minusculas" ~: cifrarVigenere "ABC[]12" "labo" ~?= "ABC[]12",
    "s tiene minusculas y otros caracteres" ~: cifrarVigenere "h!asQke[ll" "labo" ~?= "s!bgQkf[wl",
    "s solo tiene minusculas" ~: cifrarVigenere "haskell" "labo" ~?= "satyplm"
    ]

testSuiteDescifrarVigenere = test [
    "s = string vacio" ~: descifrarVigenere "" "labo" ~?= "",
    "s no tiene minusculas" ~: descifrarVigenere "ABC[]12" "labo" ~?= "ABC[]12",
    "s tiene minusculas y otros caracteres" ~: descifrarVigenere "s!bgQkf[wl" "labo" ~?= "h!asQke[ll",
    "s solo tiene minusculas" ~: descifrarVigenere "satyplm" "labo" ~?= "haskell"
    ]

testSuitePeorCifrado = test [
    "s = [], |claves| = 2" ~: expectAny (peorCifrado "" ["ip", "labo"]) ["ip", "labo"],
    "Una sola clave tiene la menor distancia" ~: peorCifrado "computacion" ["ip", "a", "labo"] ~?= "a",
    "2 claves iguales tienen la misma distancia y es la menor" ~: peorCifrado "computacion" ["ip", "a", "a"] ~?= "a",
    "2 claves distintas tienen la misma distancia y es la menor" ~: expectAny (peorCifrado "computacion" ["ip", "abaaaaaaaaa", "labo", "aaaaaaaaaba"]) ["aaaaaaaaaba","abaaaaaaaaa"] 
    ]

testSuiteCombinacionesVigenere = test [ -- duda: puede ser (clave, msj)? 
    "Msjs y claves vacias"~: combinacionesVigenere [] [] "ipmb" ~?= [],
    "Ninguna combinacion es igual a cifrado" ~: combinacionesVigenere ["$coMp]u", "haskell"] ["ip", "pc"] "ipmb" ~?= [],
    "Una combinacion es igual a cifrado" ~: combinacionesVigenere ["$coMp]u", "haskell"] ["pc", "ip"] "$rwMx]c" ~?= [("$coMp]u", "ip")],
    "Dos combinaciones son igual a cifrado" ~: expectPermutacion (combinacionesVigenere ["$coMp]u", "haskell", "$phMi]n"] ["ip", "pc"] "$rwMx]c") [("$phMi]n", "pc"), ("$coMp]u", "ip")]
    ]






-- Funciones útiles

-- margetFloat(): Float
-- asegura: res es igual a 0.00001
margenFloat = 0.00001

-- expectAny (actual: a, expected: [a]): Test
-- asegura: res es un Test Verdadero si y sólo si actual pertenece a la lista expected
expectAny :: (Foldable t, Eq a, Show a, Show (t a)) => a -> t a -> Test
expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)


-- expectlistProximity (actual: [Float], expected: [Float]): Test
-- asegura: res es un Test Verdadero si y sólo si:
--                  |actual| = |expected|
--                  para todo i entero tal que 0<=i<|actual|, |actual[i] - expected[i]| < margenFloat()
expectlistProximity:: [Float] -> [Float] -> Test
expectlistProximity actual expected = esParecidoLista actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esParecidoLista :: [Float] -> [Float] -> Bool
esParecidoLista actual expected = (length actual) == (length expected) && (esParecidoUnaAUno actual expected)

esParecidoUnaAUno :: [Float] -> [Float] -> Bool
esParecidoUnaAUno [] [] = True
esParecidoUnaAUno (x:xs) (y:ys) = (aproximado x y) && (esParecidoUnaAUno xs ys)

aproximado :: Float -> Float -> Bool
aproximado x y = abs (x - y) < margenFloat


-- expectAnyTuplaAprox (actual: CharxFloat, expected: [CharxFloat]): Test
-- asegura: res un Test Verdadero si y sólo si:
--                  para algun i entero tal que 0<=i<|expected|,
--                         (fst expected[i]) == (fst actual) && |(snd expected[i]) - (snd actual)| < margenFloat()

expectAnyTuplaAprox :: (Char, Float) -> [(Char, Float)] -> Test
expectAnyTuplaAprox actual expected = elemAproxTupla actual expected ~? ("expected any of: " ++ show expected ++ "\nbut got: " ++ show actual)

elemAproxTupla :: (Char, Float) -> [(Char, Float)] -> Bool
elemAproxTupla _ [] = False
elemAproxTupla (ac,af) ((bc,bf):bs) = sonAprox || (elemAproxTupla (ac,af) bs)
    where sonAprox = (ac == bc) && (aproximado af bf)



-- expectPermutacion (actual: [T], expected[T]) : Test
-- asegura: res es un Test Verdadero si y sólo si:
--            para todo elemento e de tipo T, #Apariciones(actual, e) = #Apariciones(expected, e)

expectPermutacion :: (Ord a, Show a) => [a] -> [a] -> Test
expectPermutacion actual expected = esPermutacion actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esPermutacion :: Ord a => [a] -> [a] -> Bool
esPermutacion a b = (length a == length b) && (sort a == sort b)