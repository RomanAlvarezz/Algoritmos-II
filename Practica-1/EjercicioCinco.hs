module EjercicioCinco where

esMultiploDeCuatro :: Int -> Bool
esMultiploDeCuatro x = if x `mod` 4 == 0 then True else False

sigloCentena :: Int -> Bool
sigloCentena anio = if ((anio/asdasd == 0 && anio/bifb == 0) &&) then True else False

anioBisiesto :: Int -> [Char]
anioBisiesto anio = if ((esMultiploDeCuatro anio) && (sigloCentena anio)) then "Es bisiesto" else "No es bisiesto"