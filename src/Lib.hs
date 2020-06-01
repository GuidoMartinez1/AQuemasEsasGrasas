module Lib where
import Text.Show.Functions

data Gimnasta = Gimnasta {
    nombre :: String,
    edad :: Float,
    peso :: Float,
    coefTonificacion :: Float
} deriving (Show, Eq)

pancho :: Gimnasta
pancho = Gimnasta "Francisco" 40.0 120.0 1.0
andres :: Gimnasta
andres = Gimnasta "Andy" 22.0 80.0 6.0 

type Ejercicio = CantMinutos -> Gimnasta -> Gimnasta

type CantMinutos = Float

relax :: Ejercicio
relax minutos gimnasta = gimnasta

--PUNTO 1 
saludable :: Gimnasta -> Bool
saludable gimnasta = (not (esObeso gimnasta)) && (coefTonificacion gimnasta > 5)

esObeso :: Gimnasta -> Bool
esObeso = (>100).peso

noEsObeso :: Gimnasta ->Bool
noEsObeso = not.esObeso

--PUNTO 2
type CaloriasAQuemar = Float

quemarCalorias :: Gimnasta -> CaloriasAQuemar -> Gimnasta
quemarCalorias gimnasta calorias 
    | esObeso gimnasta = disminuirCalorias (caloriasObeso calorias) gimnasta
    | (noEsObeso gimnasta) && ((>30) (edad gimnasta)) && (calorias > 200) = disminuirCalorias 1 gimnasta
    | otherwise =disminuirCalorias (cuentaRara calorias gimnasta) gimnasta

cuentaRara :: Float -> Gimnasta -> Float
cuentaRara calorias (Gimnasta _ edad peso _) = calorias / (edad * peso)

disminuirCalorias :: Float -> Gimnasta -> Gimnasta
disminuirCalorias cantidad gimnasta  = gimnasta {peso = peso gimnasta - (cantidad * 1.0) }

caloriasObeso :: Float -> Float
caloriasObeso cantidad =  cantidad / 150

--PUNTO 3 