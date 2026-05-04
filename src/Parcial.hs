module Parcial where
import Text.Show.Functions()

--Definición perrito

data Perrito = UnPerrito {
    raza :: string
    jueguetesFav :: [string]
    tiempoenguarderia :: Int
    energia :: Int

}

UnPerrito :: raza -> jueguetesFav -> tiempoenguarderia -> energia -> Perrito

-- Modelo Zara

zara :: Perrito

zara = UnPerrito{
    raza = "Dalmata"
    jueguetesFav =  ["Pelota", "Mantita"]
    tiempoenguarderia = 60
    energia = 80
}

-- Funciones

---- Modificar Energia
modificarEnergia :: (Int -> Int) -> Perrito -> Perrito

modificarEnergia unaOp unPerrito = UnPerrito {
                                            energia = max (unaOp.energia $ unPerrito) 0
                                             }

---- Raza Extravagante

razaExtravagante :: [string]
razaExtravagante = ["Dalmata", "Pomerania"]

---- Es raza extravagante

esRazaExtravagante :: Perrito -> Bool
esRazaExtravagante unPerrito = any elem UnPerrito(raza = raza unPerrito) razaExtravagante

---- Jugar
jugar :: Perrito -> Perrito

jugar unPerrito = ModificarEnergia (-1000) unPerrito

---- Ladrar

ladrar :: int -> Perrito -> Perrito

ladrar ladridos unPerrito = ModificarEnergia (+ (ladridos/2)) unPerrito

---- Regalar

regalar :: String -> Perrito -> Perrito
regalar unRegalo unPerrito = UnPerrito{juguetesFav = : unRegalo jueguetesFav $ unPerrito}

---- Dia de Spa

diaDeSpa :: Perrito -> Perrito

diaDeSpa unPerrito
| UnPerrito {tiempoenguarderia unPerrito} >= 50 = (modificarEnergia (+100)).regalar "Peine De Goma" UnPerrito {energia = 100, juguetesFav = } "Peine de goma" --agregar a la lista la mantita
| esRazaExtravagante unPerrito = UnPerrito{ energia = 100, } --agregar a la lista la mantita

---- Eliminar Primer Juguete

eliminarPrimerJuguete :: Perrito -> Perrito
eliminarPrimerJuguete unPerrito = UnPerrito{
                                    jueguetesFav = drop 1 juguetesFav $ unPerrito
                                    }
---- Dia de Campo

diaDeCampo :: Perrito -> Perrito

diaDeCampo unPerrito = eliminarPrimerJuguete jugar unPerrito 

---- ***PARTE B*** ----

---- Guardería PdePerritos

type RUTINA = Perrito -> Perrito

type RUTINAS = [RUTINA]

RUTINAS = [jugar, ladrar 18, regalar "Pelota", diaDeSpa, diaDeCampo]

---- Tiempo rutina

type TIEMPOS = [Int]
TIEMPOS = (30, 20, 0, 120, 720)
-- Lista actividades con tuplas (actividad / tiempo)
tiempoRutina :: RUTINA -> Perrito -> Int
tiempoRutina unaRutina unPerrito = unaRutina unPerrito

data Guarderia = UnaGuarderia rutinasGuarderia
UnaGuarderia :: RUTINAS -> Guarderia
UnaGuarderia = RUTINAS


--doble :: Int -> Int
--doble = (*2)
