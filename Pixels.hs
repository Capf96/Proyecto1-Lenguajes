{-|
Módulo      : Pixels
Descripción : Representación de pixeles prendidos y apagados en terminal
Autores     : Maria Bracamonte, 10-11147
              Carlos Perez,     13-11089
Souce       : https://github.com/Capf96/Proyecto1-Lenguajes
Módulo que permite la impresión en pantalla de caracteres ASCII en una
representación sencilla de pixeles en blanco y negro. Los pixeles se 
componen de una lista con 7 elementos normalmente, donde cada elemento
representa una línea de la impresión en pantalla. Cada pixel dentro de un
tipo Pixels esta representado por un carácter, que puede ser '*' que
representa el pixel prendido, y ' ' que representa el pixel apagado.
A su vez, definimos una serie de funciones que trabajan con este nuevo tipo de
dato, realizando operaciones normalmente encontrado en una pantalla de pixeles.
También definimos funciones para transformar dicho Pixels en un String.
-}

{------------
    Módulo
------------}

module Pixels(
    Pixels,
    font,
    showPixels,
    pixelsToString,
    pixelListToPixels,
    pixelListToString,
    concatPixels,
    messageToPixels,
    up,
    down,
    left,
    right,
    upsideDown,
    backwards,
    negative
) where

import Data.Char (ord)

{-----------------------------------------------------------------------------
                            Nuevo tipo de dato
-----------------------------------------------------------------------------}

{-|
El tipo de dato Pixels representa una impresión en pantalla de algún carácter 
o String a través de pixeles prendidos y apagados representados por '*' y ' '
respectivamente. Esta compuesto de 7 elementos normalmente, los cuales 
representan una línea del Pixels.
-}

type Pixels = [String]

{-----------------------------------------------------------------------------
                        Lista de valores predeterminados
-----------------------------------------------------------------------------}

fontBitmap =
  [
    [ 0x00, 0x00, 0x00, 0x00, 0x00 ], --  (space)
    [ 0x00, 0x00, 0x5F, 0x00, 0x00 ], --  !
    [ 0x00, 0x07, 0x00, 0x07, 0x00 ], --  "
    [ 0x14, 0x7F, 0x14, 0x7F, 0x14 ], --  #
    [ 0x24, 0x2A, 0x7F, 0x2A, 0x12 ], --  $
    [ 0x23, 0x13, 0x08, 0x64, 0x62 ], --  %
    [ 0x36, 0x49, 0x55, 0x22, 0x50 ], --  &
    [ 0x00, 0x05, 0x03, 0x00, 0x00 ], --  '
    [ 0x00, 0x1C, 0x22, 0x41, 0x00 ], --  (
    [ 0x00, 0x41, 0x22, 0x1C, 0x00 ], --  )
    [ 0x08, 0x2A, 0x1C, 0x2A, 0x08 ], --  *
    [ 0x08, 0x08, 0x3E, 0x08, 0x08 ], --  +
    [ 0x00, 0x50, 0x30, 0x00, 0x00 ], --  ,
    [ 0x08, 0x08, 0x08, 0x08, 0x08 ], --  -
    [ 0x00, 0x60, 0x60, 0x00, 0x00 ], --  .
    [ 0x20, 0x10, 0x08, 0x04, 0x02 ], --  /
    [ 0x3E, 0x51, 0x49, 0x45, 0x3E ], --  0
    [ 0x00, 0x42, 0x7F, 0x40, 0x00 ], --  1
    [ 0x42, 0x61, 0x51, 0x49, 0x46 ], --  2
    [ 0x21, 0x41, 0x45, 0x4B, 0x31 ], --  3
    [ 0x18, 0x14, 0x12, 0x7F, 0x10 ], --  4
    [ 0x27, 0x45, 0x45, 0x45, 0x39 ], --  5
    [ 0x3C, 0x4A, 0x49, 0x49, 0x30 ], --  6
    [ 0x01, 0x71, 0x09, 0x05, 0x03 ], --  7
    [ 0x36, 0x49, 0x49, 0x49, 0x36 ], --  8
    [ 0x06, 0x49, 0x49, 0x29, 0x1E ], --  9
    [ 0x00, 0x36, 0x36, 0x00, 0x00 ], --  :
    [ 0x00, 0x56, 0x36, 0x00, 0x00 ], --  ;
    [ 0x00, 0x08, 0x14, 0x22, 0x41 ], --  <
    [ 0x14, 0x14, 0x14, 0x14, 0x14 ], --  =
    [ 0x41, 0x22, 0x14, 0x08, 0x00 ], --  >
    [ 0x02, 0x01, 0x51, 0x09, 0x06 ], --  ?
    [ 0x32, 0x49, 0x79, 0x41, 0x3E ], --  @
    [ 0x7E, 0x11, 0x11, 0x11, 0x7E ], --  A
    [ 0x7F, 0x49, 0x49, 0x49, 0x36 ], --  B
    [ 0x3E, 0x41, 0x41, 0x41, 0x22 ], --  C
    [ 0x7F, 0x41, 0x41, 0x22, 0x1C ], --  D
    [ 0x7F, 0x49, 0x49, 0x49, 0x41 ], --  E
    [ 0x7F, 0x09, 0x09, 0x01, 0x01 ], --  F
    [ 0x3E, 0x41, 0x41, 0x51, 0x32 ], --  G
    [ 0x7F, 0x08, 0x08, 0x08, 0x7F ], --  H
    [ 0x00, 0x41, 0x7F, 0x41, 0x00 ], --  I
    [ 0x20, 0x40, 0x41, 0x3F, 0x01 ], --  J
    [ 0x7F, 0x08, 0x14, 0x22, 0x41 ], --  K
    [ 0x7F, 0x40, 0x40, 0x40, 0x40 ], --  L
    [ 0x7F, 0x02, 0x04, 0x02, 0x7F ], --  M
    [ 0x7F, 0x04, 0x08, 0x10, 0x7F ], --  N
    [ 0x3E, 0x41, 0x41, 0x41, 0x3E ], --  O
    [ 0x7F, 0x09, 0x09, 0x09, 0x06 ], --  P
    [ 0x3E, 0x41, 0x51, 0x21, 0x5E ], --  Q
    [ 0x7F, 0x09, 0x19, 0x29, 0x46 ], --  R
    [ 0x46, 0x49, 0x49, 0x49, 0x31 ], --  S
    [ 0x01, 0x01, 0x7F, 0x01, 0x01 ], --  T
    [ 0x3F, 0x40, 0x40, 0x40, 0x3F ], --  U
    [ 0x1F, 0x20, 0x40, 0x20, 0x1F ], --  V
    [ 0x7F, 0x20, 0x18, 0x20, 0x7F ], --  W
    [ 0x63, 0x14, 0x08, 0x14, 0x63 ], --  X
    [ 0x03, 0x04, 0x78, 0x04, 0x03 ], --  Y
    [ 0x61, 0x51, 0x49, 0x45, 0x43 ], --  Z
    [ 0x00, 0x00, 0x7F, 0x41, 0x41 ], --  [
    [ 0x02, 0x04, 0x08, 0x10, 0x20 ], --  \
    [ 0x41, 0x41, 0x7F, 0x00, 0x00 ], --  ]
    [ 0x04, 0x02, 0x01, 0x02, 0x04 ], --  ^
    [ 0x40, 0x40, 0x40, 0x40, 0x40 ], --  _
    [ 0x00, 0x01, 0x02, 0x04, 0x00 ], --  `
    [ 0x20, 0x54, 0x54, 0x54, 0x78 ], --  a
    [ 0x7F, 0x48, 0x44, 0x44, 0x38 ], --  b
    [ 0x38, 0x44, 0x44, 0x44, 0x20 ], --  c
    [ 0x38, 0x44, 0x44, 0x48, 0x7F ], --  d
    [ 0x38, 0x54, 0x54, 0x54, 0x18 ], --  e
    [ 0x08, 0x7E, 0x09, 0x01, 0x02 ], --  f
    [ 0x08, 0x14, 0x54, 0x54, 0x3C ], --  g
    [ 0x7F, 0x08, 0x04, 0x04, 0x78 ], --  h
    [ 0x00, 0x44, 0x7D, 0x40, 0x00 ], --  i
    [ 0x20, 0x40, 0x44, 0x3D, 0x00 ], --  j
    [ 0x00, 0x7F, 0x10, 0x28, 0x44 ], --  k
    [ 0x00, 0x41, 0x7F, 0x40, 0x00 ], --  l
    [ 0x7C, 0x04, 0x18, 0x04, 0x78 ], --  m
    [ 0x7C, 0x08, 0x04, 0x04, 0x78 ], --  n
    [ 0x38, 0x44, 0x44, 0x44, 0x38 ], --  o
    [ 0x7C, 0x14, 0x14, 0x14, 0x08 ], --  p
    [ 0x08, 0x14, 0x14, 0x18, 0x7C ], --  q
    [ 0x7C, 0x08, 0x04, 0x04, 0x08 ], --  r
    [ 0x48, 0x54, 0x54, 0x54, 0x20 ], --  s
    [ 0x04, 0x3F, 0x44, 0x40, 0x20 ], --  t
    [ 0x3C, 0x40, 0x40, 0x20, 0x7C ], --  u
    [ 0x1C, 0x20, 0x40, 0x20, 0x1C ], --  v
    [ 0x3C, 0x40, 0x30, 0x40, 0x3C ], --  w
    [ 0x44, 0x28, 0x10, 0x28, 0x44 ], --  x
    [ 0x0C, 0x50, 0x50, 0x50, 0x3C ], --  y
    [ 0x44, 0x64, 0x54, 0x4C, 0x44 ], --  z
    [ 0x00, 0x08, 0x36, 0x41, 0x00 ], --  {
    [ 0x00, 0x00, 0x7F, 0x00, 0x00 ], --  |
    [ 0x00, 0x41, 0x36, 0x08, 0x00 ]  --  }
  ]

{-----------------------------------------------------------------------------
                                Funciones
-----------------------------------------------------------------------------}

{-|
La función font toma un carácter  y este lo transforma en un tipo pixel a tipo
Pixels, buscando el carácter  introducido en la lista fontBitmap, en donde, en
caso de ser encontrado, trabaja sobre la lista proporcionada que contiene el 
valor hexadecimal de los bits prendidos y apagados de cada columna del pixel;
en caso de no ser encontrado devuelve un Pixels con todos los pixeles prendidos.
-}

font :: Char -> Pixels
font a 
    | 0 <= pos && pos <= (length fontBitmap) = reverse (transformar (busqueda pos))
    | otherwise = transformar (replicate 7 0x7F)
  where pos = (ord a) - 32

{-
La función búsqueda recibe un entero y devuelve el elemento que se encuentra
en esa posición de la lista fontBitmap.
-}

busqueda :: Int -> [Integer]
busqueda pos = fontBitmap !! pos

{-
La función transformar toma una lista de enteros y los transforma en una lista
de Strings que representaran los pixeles prendidos y apagados.
-}

transformar :: [Integer] -> Pixels
transformar x = foldl1 (zipWith (++)) (map (reverse . binary 7) x)

{-
La función binary recibe un número en base decimal y lo transforma a una
representación en pixeles prendidos y apagados en un string de longitud 7.
-}

binary :: Int -> Integer -> Pixels
binary 0 _ = []
binary i a
    | a `mod` 2 == 0 = " " : binary (i-1) (a `div` 2)
    | a `mod` 2 == 1 = "*" : binary (i-1) (a `div` 2)

{-|
La función showPixels recibe un Pixels y lo imprime en pantalla de tal manera
que forme el carácter  que esta representando.
-}

showPixels :: Pixels -> IO ()
showPixels x = mapM_ print x

{-|
La función pixelsToString recibe un Pixels y lo transforma en un String con 
debidos saltos de línea para su correcta representación.
-}

pixelsToString :: Pixels -> String
pixelsToString x = foldl1 (++) (saltosLinea x)

{-
La función saltosLinea es una función auxiliar de pixelsToString, la cual le 
agrega los debidos saltos de línea a las filas de Pixels para su correcta 
representación.
-}

saltosLinea :: Pixels -> Pixels
saltosLinea x = map (\x -> x ++ "\n") (take tamano x) ++ (drop tamano x)
            where tamano = (length x)-1

{-|
La función pixelListToPixels recibe una lista de Pixels y une todos los 
elementos de esta con un string vacio de separación entre estos.
-}

pixelListToPixels :: [Pixels] -> Pixels
pixelListToPixels x = foldl1 (\x y-> x++[""]++y ) x

{-|
La función pixelListToString recibe una lista de Pixels y los transforma en
un String.
-}

pixelListToString :: [Pixels] -> String
pixelListToString x = pixelsToString (concat x)

{-|
La función concatPixels recibe una lista de pixeles y los concatena entre 
ellos formando un solo Pixels.
-}

concatPixels :: [Pixels] -> Pixels
concatPixels x = foldl1 zip' x 
 
{-
La función zip' es una función auxiliar de concatPixels que une los pixeles de
la lista de pixeles.
-}

zip':: [[a]] -> [[a]] -> [[a]]
zip' x y = zipWith (++) x y

{-|
La función messageToPixels recibe un String y lo transforma en un elemento de
tipo Pixels.
-}

messageToPixels :: String -> Pixels
messageToPixels "" = error "El string no puede ser vacio"
messageToPixels x = concatPixels (crearLista x)

{-
La función crearLista es una función auxiliar de messageToPixels que recibe el
String y le realiza las operaciones necesarias para transformarlo a un elemento
de tipo Pixels.
-}

crearLista :: String -> [Pixels]
crearLista x = map agregarEspacio (take tamano lista) ++ drop tamano lista
            where lista  = (map font x)
                  tamano = ((length lista)-1)

{-
La función agregarEspacio es una función auxiliar de crearLista que agrega un 
espacio a cada elemento del Pixels para su debida concatenación.
-}

agregarEspacio :: Pixels -> Pixels
agregarEspacio x = map (\x -> x ++ " ") x

{-|
La función up recibe un Pixels y toma el primer elemento de este y lo coloca
en la última posición.
-}

up :: Pixels -> Pixels
up (x:xs) = xs ++ [x]

{-|
La función down recibe un Pixels y toma el último elemento de este y lo coloca
de primero.
-}

down :: Pixels -> Pixels
down x = reverse ( up (reverse x))

{-|
La función left recibe un Pixels y a toma el primer elemento de cada elemento 
que lo compone y lo coloca de último.
-}

left :: Pixels -> Pixels
left x = map (movementAux) x

{-|
La función left recibe un Pixels y a toma el último elemento de cada elemento 
que lo compone y lo coloca de primero.
-}

right :: Pixels -> Pixels
right x = map (reverse . movementAux . reverse) x

{-
La función movementAux es una función auxiliar de left y right que toma el
primer elemento de un String y lo coloca en la última posición.
-}

movementAux :: String -> String
movementAux (x:xs) = xs ++ [x]

{-|
La función upsideDown recibe un Pixels e intercambia todos los elementos que 
lo componen tal que el primero es el último, el segundo el penúltimo, etc. De
esta manera al realizar la impresión del resultado imprime el Pixels de cabeza.
-}

upsideDown :: Pixels -> Pixels
upsideDown x = reverse x

{-|
La función backwards recibe un Pixels y voltea la posición de cada uno de los
elementos de sus elementos tal que el primero es el último, el segundo el 
penúltimo, etc. De esta forma, el resultado de la impresión del Pixels 
seria la versión espejo del Pixels original.
-}

backwards :: Pixels -> Pixels
backwards p = map (reverse) p

{-|
La función negative recibe un Pixels y transforma todos los pixeles prendidos 
a apagados y todos los pixeles apagados a prendidos, es decir, todos los '*' 
en ' ' y viceversa.
-}

negative :: Pixels -> Pixels
negative l = map (map negativeAux) l

{-
La función negativeAux es una función auxiliar de negative que recibe un 
carácter  '*' o ' ' y lo transforma en su opuesto.
-}

negativeAux :: Char -> Char
negativeAux x
    | x == '*' = ' '
    | x == ' ' = '*'
    | otherwise = error "Solo prende y apaga pixeles"