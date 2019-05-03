import Text.Show.Functions -- Hace a las funciones instancia de Show

type FormaDeSaqueo = Tesoro -> Bool

{-
data Tesoro = Tesoro {
    nombre :: String,
    valor :: Int
} deriving (Show, Eq)
-}

--- TESOROS 

brujulaQueApunta = Tesoro "Brujula que apunta" 10000
frascoDeArenaConValorCero = Tesoro "Frasco de arena" 0
cajitaMusical = Tesoro "Cajita musical" 1
doblonesDeOro = Tesoro "Doblones de oro" 1000
frascoDeArenaConValorUno = Tesoro "Frasco de arena" 1
monedaDelCofreDelMuerto = Tesoro "Moneda del cofre del muerto" 100
espadaDeHierro = Tesoro "Espada de hierro" 50
cuchilloDelPadre = Tesoro "Cuchillo del padre" 5
ron = Tesoro "Ron" 25
monedaDePlata = Tesoro "Moneda de plata" 15
mapaDeBarbarroja = Tesoro "Mapa de Barbarroja" 15000
catalejoMagico = Tesoro "Catalejo Magico" 500
sombreroDeAvestruz = Tesoro "Sombrero de avestruz" 199

esValioso :: FormaDeSaqueo
esValioso = (>100) . valor
--esValioso tesoro = valor tesoro > 100

noEsValioso :: FormaDeSaqueo
noEsValioso = not . esValioso


mismoNombre :: Tesoro -> Tesoro -> Bool
mismoNombre unTesoro otroTesoro = nombre unTesoro == nombre otroTesoro 

distintoValor :: Tesoro -> Tesoro -> Bool
distintoValor unTesoro otroTesoro = valor unTesoro /= valor otroTesoro 

mismoNombreDistintoValor :: Tesoro -> Tesoro -> Bool
mismoNombreDistintoValor unTesoro otroTesoro = distintoValor unTesoro otroTesoro && mismoNombre unTesoro otroTesoro

data Pirata = Pirata {
    apodo :: String,
    botin :: [Tesoro]
} deriving Show

--nombresDeTesoros :: Pirata -> [String]
--nombresDeTesoros pirata = map nombre (botin pirata)

-- TESOROS PIRATAS

-- La cantidad de tesoros de un pirata
cantidadTesoros :: Pirata -> Int
cantidadTesoros pirata = length (botin pirata)

valoresDeTesoros :: Pirata -> [Float]
valoresDeTesoros pirata = map valor (botin pirata)

valorTotalBotin :: Pirata -> Float
valorTotalBotin pirata = sum (valoresDeTesoros pirata)

-- Si un pirata es afortunado
esAfortunado :: Pirata -> Bool
esAfortunado pirata = valorTotalBotin pirata > 10000

-- El valor del tesoro más valioso 
valorTesoroMasValioso :: Pirata -> Float
valorTesoroMasValioso pirata = maximum (valoresDeTesoros pirata)

-- Adquirir un nuevo tesoro
agregarTesoro :: Tesoro -> Pirata -> Pirata
agregarTesoro tesoro pirata = pirata { botin = tesoro : botin pirata }

perderTesorosSegun :: FormaDeSaqueo -> Pirata -> Pirata
perderTesorosSegun condTesoro pirata = pirata {botin = filter condTesoro (botin pirata)}

-- Perder todos los tesoros valiosos
perderTesorosValiosos :: Pirata -> Pirata
perderTesorosValiosos pirata = perderTesorosSegun noEsValioso pirata

-- Perder todos los tesoros con un nombre dado.
perderTesorosPorNombre :: String -> Pirata -> Pirata
perderTesorosPorNombre nombre  pirata = perderTesorosSegun (not . seLlama nombre)  pirata

seLlama :: String -> Tesoro -> Bool
seLlama nombreBuscado tesoro = nombreBuscado == nombre tesoro
--esParteDelBotinConOtroValor :: Pirata -> FormaDeSaqueo
--esParteDelBotinConOtroValor pirata tesoro = any (mismoNombreDistintoValor tesoro) (botin pirata)

-- Dos piratas tienen un mismo tesoro, pero de valor diferente
tienenMismoTesoroConOtroValor :: Pirata -> Pirata -> Bool
--tienenMismoTesoroConOtroValor unPirata otroPirata = any (esParteDelBotinConOtroValor otroPirata) $ botin unPirata
tienenMismoTesoroConOtroValor unPirata otroPirata = hayMismoTesoroConOtroValor (botin unPirata) (botin otroPirata)

hayMismoTesoroConOtroValor :: [Tesoro] -> [Tesoro] -> Bool
hayMismoTesoroConOtroValor unBotin otroBotin = any (esParteDelBotin unBotin) otroBotin

esParteDelBotin :: [Tesoro] -> Tesoro -> Bool
esParteDelBotin botin tesoro = any (mismoNombreDistintoValor tesoro) botin


-- TEMPORADA DE SAQUEOS

-- Tesoros con objetos específicos
saqueoEspecifico :: String -> FormaDeSaqueo
saqueoEspecifico = seLlama 
-- Piratas con corazón 
saqueoBuenCorazon :: FormaDeSaqueo
--saqueoBuenCorazon = const False
saqueoBuenCorazon _ = False

saqueoValioso :: FormaDeSaqueo
saqueoValioso = esValioso
-- Saqueos complejos 
saqueoComplejo :: [FormaDeSaqueo] -> FormaDeSaqueo
--saqueoComplejo saqueos tesoro = any ($tesoro) saqueos 
saqueoComplejo saqueos tesoro = any (verificaTesoro tesoro) saqueos 

verificaTesoro:: Tesoro -> FormaDeSaqueo -> Bool
verificaTesoro tesoro saqueo = saqueo tesoro

jackSparrow = Pirata {
    apodo = "Jack Sparrow",
    botin = [brujulaQueApunta, frascoDeArenaConValorCero]
}

davidJones = Pirata {
    apodo = "David Jones",
    botin = [cajitaMusical]
}

anneBonny = Pirata {
    apodo = "Anne Bonny",
    botin = [doblonesDeOro, frascoDeArenaConValorUno]
}

elizabethSwann = Pirata {
    apodo = "Elizabeth Swann",
    botin = [monedaDelCofreDelMuerto, espadaDeHierro]
}

willTurner = Pirata {
    apodo = "Will Turner",
    botin = [cuchilloDelPadre]
}

saquear :: FormaDeSaqueo -> Tesoro -> Pirata  -> Pirata
saquear saqueo tesoro pirata
    | saqueo tesoro = agregarTesoro tesoro pirata
    | otherwise     = pirata

-- Navegando los siete mares

data Barco = Barco {
    descripcion :: String,
    tripulacion :: [Pirata],
    formaSaqueo :: FormaDeSaqueo
} deriving Show

perlaNegra = Barco {
    descripcion = "Perla Negra",
    tripulacion = [jackSparrow, anneBonny],
    formaSaqueo = saqueoComplejo [esValioso, saqueoEspecifico "sombrero"]
}
holandesErrante = Barco {
    descripcion = "Holandes Errante",
    tripulacion = [davidJones],
    formaSaqueo = saqueoEspecifico "cajita"
}
incorporarPirata :: Pirata -> Barco -> Barco
incorporarPirata pirata barco = barco {tripulacion = pirata : tripulacion barco}

abandonarBarco :: Pirata -> Barco -> Barco
abandonarBarco pirata barco = barco {tripulacion = filter (not . validarIdentidad pirata) (tripulacion barco)}

validarIdentidad :: Pirata -> Pirata -> Bool
validarIdentidad unPirata otroPirata = apodo unPirata == apodo otroPirata

------------------------

type Isla = Tesoro

islaDelRon = ron
islaTortuga = frascoDeArenaConValorUno

anclarEnUnaIsla ::  Isla -> Barco -> Barco
anclarEnUnaIsla  isla barco = barco {tripulacion = map (agregarTesoro isla) (tripulacion barco)}

type Ciudad = [Tesoro]

portRoyal = [monedaDePlata, mapaDeBarbarroja, catalejoMagico]

carmenDePatagones = [sombreroDeAvestruz]

atacarCiudad ::  Ciudad -> Barco -> Barco
atacarCiudad ciudad barco = barco {tripulacion = zipWith (saquear (formaSaqueo barco)) ciudad (tripulacion barco)}

-- Abordar barco 
-- No hace nada interesante, solo para probar

abordarBarco :: Barco -> Barco -> Barco
abordarBarco atacante defensor = atacante

-----------------------------------------------------
-- SEGUNDA PARTE

data Tesoro = 
    Tesoro String Float |
    Bono [Float] | 
    LeLiq Float String deriving (Eq, Show)

tasas = [("Argentina",0.74),("Brasil",0.05)]

nombre :: Tesoro -> String
nombre (Tesoro nombre _) = nombre
nombre (Bono _) = "Bono"
nombre (LeLiq _ pais ) = "LeLiq " ++ pais

valor :: Tesoro -> Float
valor (Tesoro _ v) = v
valor (Bono cotizaciones) = (maximum cotizaciones - minimum cotizaciones) * 1.5
valor (LeLiq importe pais) = (1 + tasaDe pais) * importe 

tasaDe::String->Float
tasaDe pais = (snd.head.filter ((pais==).fst)) tasas

    --2

saqueoBuitre :: FormaDeSaqueo
saqueoBuitre (Bono _) = True
saqueoBuitre _ = False

saqueoFobico :: String -> FormaDeSaqueo
saqueoFobico palabra = not.saqueoEspecifico palabra

--3

uade :: Barco -> Barco
uade barco = barco {formaSaqueo = not.formaSaqueo barco}

uba :: Barco -> Barco
uba barco = barco {formaSaqueo = saqueoComplejo [saqueoBuitre, saqueoValioso, formaSaqueo barco]}

uai :: Barco -> Barco
uai = id 

--4
--a
type Situacion = Barco -> Barco

peliculearBarco :: [Situacion] -> Barco -> Barco
peliculearBarco situaciones barco = foldr ($) barco situaciones

--b
instance Eq Pirata where 
    unPirata == otroPirata = apodo unPirata == apodo otroPirata 

instance Eq Barco where 
    unBarco == otroBarco = descripcion unBarco == descripcion otroBarco && mismosElementos (tripulacion unBarco)  (tripulacion otroBarco) 

mismosElementos::Eq a => [a] -> [a] -> Bool
mismosElementos conjunto1 conjunto2 = incluido conjunto1 conjunto2 && incluido conjunto2 conjunto1

incluido conjunto1 conjunto2 = all (flip elem conjunto2) conjunto1

barcosConHistoriaInofensiva :: [Situacion] -> [Barco] -> [Barco]
barcosConHistoriaInofensiva historia barcos = filter (esInofensiva historia) barcos

esInofensiva :: [Situacion] -> Barco -> Bool
esInofensiva historia barco = barco == peliculearBarco historia barco

--c

barcoTripulacionMasNumerosa :: [Situacion] -> [Barco] -> Barco
barcoTripulacionMasNumerosa historia barcos = maximum (map (peliculearBarco historia) barcos)

instance Ord Barco where 
    unBarco <= otroBarco = cantidadTripulantes unBarco == cantidadTripulantes otroBarco 

cantidadTripulantes :: Barco -> Int
cantidadTripulantes barco  = length (tripulacion barco)

--5 
barcoInfinito :: Barco
barcoInfinito = Barco "Infinito" (map  inventarPirata  [1..]) saqueoValioso

inventarPirata::Float -> Pirata
inventarPirata valor = Pirata "Pirata" [Tesoro "Sombrero" valor]

