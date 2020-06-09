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
disminuirCalorias cantidad gimnasta  = gimnasta {peso = peso gimnasta - cantidad  }

caloriasObeso :: Float -> Float
caloriasObeso cantidad =  cantidad / 150

--PUNTO 3 

caminataEnCinta :: Ejercicio
caminataEnCinta minutos gimnasta= quemarCalorias gimnasta (promedioCantidadMinutos minutos)

entrenamientoEnCinta :: Ejercicio
entrenamientoEnCinta minutos gimnasta = quemarCalorias gimnasta (promedioSegunVelocidadMaxima minutos)

type KgALevantar = Float

pesas ::  KgALevantar -> Ejercicio
pesas kg minutos  = tonificar (cantidadATonificar kg minutos) 

type Inclinacion = Float

colina :: Inclinacion -> Ejercicio
colina inclinacion minutos gimnasta = quemarCalorias gimnasta (promedioSegunInclinacion inclinacion minutos)

montania :: Inclinacion -> Ejercicio
montania inclinacion minutos  = flip quemarCalorias (promedioMontania inclinacion minutos).(tonificar 1)

promedioCantidadMinutos :: CantMinutos -> Float
promedioCantidadMinutos cantidadMinutos = cantidadMinutos * 5

promedioSegunVelocidadMaxima :: CantMinutos -> Float
promedioSegunVelocidadMaxima min = min * (6+(min/5)/2)

tonificar :: Float -> Gimnasta -> Gimnasta
tonificar cantidad gimnasta = gimnasta {coefTonificacion = coefTonificacion gimnasta + cantidad}

cantidadATonificar :: Float -> Float -> Float
cantidadATonificar kg minutos 
    | minutos > 10 = kg / 10
    | otherwise = 0

promedioSegunInclinacion :: Inclinacion -> CantMinutos -> Float
promedioSegunInclinacion inclinacion cantMinutos = 2*inclinacion*cantMinutos

promedioMontania :: Inclinacion -> CantMinutos -> Float
promedioMontania inclinacion minutos = (2*inclinacion*(minutos/2)) + (2*(inclinacion+3)*(minutos/2))

--PUNTO 4

data Rutina = Rutina {
      nombreRutina :: String,
      duracionTotal :: Float,
      ejercicios :: [Ejercicio]
      } deriving (Show)

duracionPorEjercicio :: Float -> [Ejercicio] -> Float
duracionPorEjercicio duracionTotal ejercicios= (/) duracionTotal 5 --hardcodie el 5 porque me rompe si uso length de la lista de ejercicios

--type Ejercicio = CantMinutos -> Gimnasta -> Gimnasta
--RECURSIVIDAD

gimnastaLuegoDeHacerRutina :: Rutina -> Gimnasta -> Gimnasta
gimnastaLuegoDeHacerRutina rutina gimnasta = aplicarEjercicio (duracionPorEjercicio (duracionTotal rutina) (ejercicios rutina)) (ejercicios rutina)  gimnasta

aplicarEjercicio :: Float -> [Ejercicio] -> Gimnasta -> Gimnasta
aplicarEjercicio _ [] gimnasta = gimnasta
aplicarEjercicio duracion (ejercicio:ejercicios) gimnasta = (aplicarEjercicio duracion ejercicios) (ejercicio duracion gimnasta )

--FOLD

gimnastaLuegoDeHacerRutina' :: Rutina -> Gimnasta -> Gimnasta
gimnastaLuegoDeHacerRutina' rutina gimnasta = foldr ($) gimnasta (aplicarARutina rutina)

aplicarARutina :: Rutina -> [Gimnasta -> Gimnasta]
aplicarARutina (Rutina _ duracionTotal ejercicios)= map (\ej-> ej (duracionPorEjercicio duracionTotal ejercicios)) ejercicios


--4 B me da alta paja

--PUNTO 5

puedenLlevarASaludable :: [Rutina] -> Gimnasta-> [Rutina]
puedenLlevarASaludable rutinas unGimnasta= filter (\rutina -> saludable (gimnastaLuegoDeHacerRutina rutina unGimnasta))  rutinas