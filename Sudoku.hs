
-- Universidad Católica Andrés Bello
-- Escuela de Ingeniería Informática
-- Cátedra de Paradigmas de Programación
-- Profesor: Jesús Larez
-- Alumnos: Oscar Bastardo, Daniel Iturriza, Ronald Suez

-------------------------------------------------------------------------------

-- Programa en Haskell para resolver tableros de Sudoku mediante un algoritmo 
-- recursivo de Backtracking.

-- Instrucciones para ejecutar el programa en GHCi:
--    1.- Modificar la lista "sudoku" del archivo sudoku.hs con los valores 
--        del tablero que se desea resolver
--    2.- Compilar el programa mediante el comando: 
--        > :l Sudoku.hs 
--    3.- 3.- Ejecutar el programa con el tablero como parámetro mediante 
--    el uso de la siguiente expresión:  putStrLn $ imprime $ resuelve' sudoku

module Sudoku where

-- Se importa el tablero de sudoku que se desea resolver
import Tablero 
 
-- Convierte un índice i en una coordenada
iac :: Int -> (Int, Int)
iac i = (calcX i, calcY i)
  where calcX i   = i - 9 * (i `div` 9)
        calcY i   = i `div` 9
 
-- Convierte una coordenada (x,y) en un índice
cai :: (Int, Int) -> Int
cai (x, y) = x + y * 9
 
-- Devuelve los valores de la columna del tablero (s) en el índice (p)
columna :: Int -> [Int] -> [Int]
columna p s = columnaEn (iac p) s
  where columnaEn (x, _) s = map (\y -> s !! cai (x, y)) [0..8]
 
-- Devuelve los valores de la fila del tablero (s) en el índice (p)
fila :: Int -> [Int] -> [Int]
fila p s = filaEn (iac p) s
  where filaEn (_, y) s = map (\x -> s !! cai (x, y)) [0..8]
 
-- Devuelve los valores del cuadro de 3 x 3 del tablero (s) en el índice (p) 
cuadro :: Int -> [Int] -> [Int]
cuadro p s = cuadroEn (iac p) s
  where cuadroEn (x, y) s = [ s !! cai (xx + desplX x, yy + desplY y) | xx <- [0..2], yy <- [0..2] ] 
        desplX x' = 3 * (x' `div` 3)
        desplY y' = 3 * (y' `div` 3)

-- Borra todas las ocurrencias de un valor en una lista
borraOcur :: Int -> [Int] -> [Int]
borraOcur _ []     = []
borraOcur y (x:xs) | x == y    = borraOcur y xs
                   | otherwise = x : borraOcur y xs
 
-- Borra los elementos en la segunda lista de la primera lista
borra' :: [Int] -> [Int] -> [Int]
borra' [] _       = []
borra' xs []      = xs
borra' xs (y:ys)  = borra' (borraOcur y xs) ys
 
-- La lista de soluciones en el índice (p) del tablero (s)
soluciones :: Int -> [Int] -> [Int]
soluciones p s | p > length s  = []
               | (s !! p) == 0 = [1..9] `borra'` (columna p s ++ fila p s ++ cuadro p s)
               | otherwise     = [s !! p]
 
-- Genera una nueva versión del tablero (s) con un valor (x) en el índice (p)
prueba :: Int -> [Int] -> Int -> [Int]
prueba p s x = take p s ++ [x] ++ drop (p + 1) s
 
-- Busca el siguiente valor vacío comenzando por el índice (p) en el tablero (s)
vacio :: Int -> [Int] -> Int
vacio p s | p == 80           = 80
          | s !! (p + 1) == 0 = p + 1
          | otherwise         = vacio (p + 1) s

-- Resuelve mediante un algoritmo de backtracking el tablero (s), 
-- comenzando en el índice (p), con un conjunto de posibles soluciones,
-- en ese punto.
-- 80 es el índice del último elemento del tablero (s)
resuelve :: Int -> [Int] -> [Int] -> [[Int]]
resuelve 80 s []     = []
resuelve 80 s (x:[]) = prueba 80 s x
resuelve _  s []     = []
resuelve p  s (x:xs)  | resuelveloSig == [] = resuelve p s xs
                      | otherwise           = resuelveloSig
  where resuelveSig p s  = resuelve (vacio p s) s (soluciones (vacio p s) s)
        resuelveloSig    = resuelveSig p (prueba p s x)

resuelve' s = resuelve 0 s (soluciones 0 s)

-- entremezcla el elemento (c) a través de la cadena (xs)
juntar :: a -> [a] -> [a]
juntar _ (x:[])  = [x]
juntar c (x:xs)  = x : c : juntar c xs
 
-- Imprime el tablero como una cuadrícula de 9 x 9
imprime [] = []
imprime s  = espaciar s ++ imprime (drop 9 s)
  where mostrar s    = concatMap show s
        espacio      = ' '
        linea        = "\n"
        espaciar s   = juntar espacio (take 9 (mostrar s) ++ linea)


