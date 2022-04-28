
type Producto = (String, Float)

productoDeLujo :: Producto -> Bool
productoDeLujo unProducto = elem 'x' (nombreDe unProducto) || elem 'z' (nombreDe unProducto)

nombreDe :: Producto -> String
nombreDe unProducto = fst unProducto

productoCorriente :: Producto -> Bool
productoCorriente unProducto = esUnaVocal . head $ nombreDe unProducto

esUnaVocal :: Char -> Bool
esUnaVocal unChar = elem unChar "aeiou"

productoCodiciado :: Producto -> Bool
productoCodiciado  unProducto = (> 10) . length $ nombreDe unProducto

productoXL :: Producto -> String 
productoXL unProducto = nombreDe unProducto ++ "XL"

productoDeElite :: Producto -> Bool
productoDeElite unProducto = not (productoCorriente unProducto) && productoCodiciado unProducto && productoDeLujo unProducto

descodiciarProducto :: Producto -> String
descodiciarProducto unProducto = take 10 (nombreDe unProducto)

versionBarata :: Producto -> String
versionBarata  = reverse.descodiciarProducto

entregaSencilla :: Producto -> Bool
entregaSencilla unProducto = cantidadDeLetrasPar (nombreDe unProducto)

cantidadDeLetrasPar :: String -> Bool
cantidadDeLetrasPar = even.length

aplicarDescuento :: Float -> Float -> Float 
aplicarDescuento unPrecio unDescuento = unPrecio - unDescuento

aplicarCostoDeEnvio :: Float -> Float -> Float 
aplicarCostoDeEnvio unPrecio costoDeEnvio = unPrecio + costoDeEnvio

precioTotal :: Producto -> Float -> Float -> Float -> Float 
precioTotal unProducto cantidad descuento costoDeEnvio = (flip aplicarCostoDeEnvio costoDeEnvio) . (* cantidad) . (flip aplicarDescuento descuento) $ precioDe unProducto

precioDe :: Producto -> Float
precioDe unProducto = snd unProducto

------------------------------------------
{-take :: Int -> String -> String

drop :: Int -> String -> String
	
head :: String -> Char

elem :: Char -> String -> Bool

reverse :: String -> String
-}
------------------------------------------