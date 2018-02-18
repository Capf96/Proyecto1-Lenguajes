{-
Prueba para font
:l Pixels.hs
-}

--Prueba para font

let a = font 'A'
let b = font 'b'
let c = font 'c'
let d = font 'D'
let e = font '!'
let f = font '?'
let g = font 'ñ'
let l = font ' '


showPixels a
showPixels b
showPixels c
showPixels d
showPixels e
showPixels f
showPixels g
showPixels l

--Prueba para up 
 
let h = up a
let i = up b
let j = up e
let k = up f
let m = up g
let n = up l
showPixels h
showPixels i
showPixels j
showPixels k
showPixels m
showPixels n

--Prueba para down
 
let h = down a
let i = down b
let j = down e
let k = down f
let m = down g
let n = down l
showPixels h
showPixels i
showPixels j
showPixels k
showPixels m
showPixels n

--Prueba para left
 
let h = left a
let i = left b
let j = left e
let k = left f
let m = left g
let n = left l
showPixels h
showPixels i
showPixels j
showPixels k
showPixels m
showPixels n

--Prueba para right
 
let h = right a
let i = right b
let j = right e
let k = right f
let m = right g
let n = right l
showPixels h
showPixels i
showPixels j
showPixels k
showPixels m
showPixels n

--Prueba para upsideDown
 
let h = upsideDown a
let i = upsideDown b
let j = upsideDown e
let k = upsideDown f
let m = upsideDown g
let n = upsideDown l
showPixels h
showPixels i
showPixels j
showPixels k
showPixels m
showPixels n

--Prueba para backwards
 
let h = backwards a
let i = backwards b
let j = backwards e
let k = backwards f
let m = backwards g
let n = backwards l
showPixels h
showPixels i
showPixels j
showPixels k
showPixels m
showPixels n

--Prueba para negative
 
let h = negative a
let i = negative b
let j = negative e
let k = negative f
let m = negative g
let n = negative l
showPixels h
showPixels i
showPixels j
showPixels k
showPixels m
showPixels n

--Prueba para pixelsToString

pixelsToString a
pixelsToString b
pixelsToString c
pixelsToString d
pixelsToString e
pixelsToString f
pixelsToString g

--Prueba para pixelListToPixels

let h = pixelListToPixels [a,b,c]
let i =pixelListToPixels [d,e]
let j =pixelListToPixels [f,g]
let k =pixelListToPixels [a]
let m =pixelListToPixels []
showPixels h
showPixels i
showPixels j
showPixels k
showPixels m

--Prueba para pixelListToString

let h = pixelListToString [a,b,c]
let i =pixelListToString [d,e]
let j =pixelListToString[f,g]
let k =pixelListToString [a]
let m =pixelListToString []

--Prueba para oncatPixels

let h = concatPixels [a,b,c]
let i =concatPixels [d,e]
let j =concatPixels [f,g]
let k =concatPixels [a]
let m =concatPixels []
showPixels h
showPixels i
showPixels j
showPixels k
showPixels m

--Prueba messageToPixels
let hola =  "Hola"
let mundo = "mundo"
let adios = "adios!ñ"
let cruel = "cruel"

let h = messageToPixels hola
let i =messageToPixels mundo
let j =messageToPixels cruel
let k =messageToPixels adios
let m =messageToPixels ""
showPixels h
showPixels i
showPixels j
showPixels k
showPixels m


