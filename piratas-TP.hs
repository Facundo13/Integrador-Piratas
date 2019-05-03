import Text.Show.Functions -- Hace a las funciones instancia de Show

type CondicionTesoro = Tesoro -> Bool

data Tesoro = Tesoro {
    nombreTesoro :: String,
    valor :: Int
} deriving (Show, Eq)

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

esValioso :: CondicionTesoro
esValioso = (>100) . valor

noEsValioso :: CondicionTesoro
noEsValioso = not . esValioso

seLlama :: String -> CondicionTesoro
seLlama nombre tesoro = nombre == nombreTesoro tesoro

mismoNombre :: Tesoro -> CondicionTesoro
mismoNombre unTesoro otroTesoro = nombreTesoro unTesoro == nombreTesoro otroTesoro 

mismoValor :: Tesoro -> CondicionTesoro
mismoValor unTesoro otroTesoro = valor unTesoro == valor otroTesoro 

mismoNombreDistintoValor :: Tesoro -> CondicionTesoro
mismoNombreDistintoValor unTesoro otroTesoro = not (mismoValor unTesoro otroTesoro) && mismoNombre unTesoro otroTesoro

data Pirata = Pirata {
    nombrePirata :: String,
--    condicionSaqueo :: CondicionTesoro,
    botin :: [Tesoro]
} deriving (Show)

nombresDeTesoros :: Pirata -> [String]
nombresDeTesoros pirata = map nombreTesoro (botin pirata)

-- TESOROS PIRATAS

-- La cantidad de tesoros de un pirata
cantidadTesoros :: Pirata -> Int
cantidadTesoros pirata = length (botin pirata)

valoresDeTesoros :: Pirata -> [Int]
valoresDeTesoros pirata = map valor (botin pirata)

valorTotalBotin :: Pirata -> Int
valorTotalBotin pirata = sum (valoresDeTesoros pirata)

-- Si un pirata es afortunado
esAfortunado :: Pirata -> Bool
esAfortunado pirata = valorTotalBotin pirata > 10000

-- El valor del tesoro más valioso 
valorTesoroMasValioso :: Pirata -> Int
valorTesoroMasValioso pirata = maximum (valoresDeTesoros pirata)

-- Adquirir un nuevo tesoro
agregarTesoro :: Tesoro -> Pirata -> Pirata
agregarTesoro tesoro pirata = pirata { botin = tesoro : botin pirata }

perderTesorosSegun :: CondicionTesoro -> Pirata -> Pirata
perderTesorosSegun condTesoro pirata = pirata {botin = filter condTesoro (botin pirata)}

-- Perder todos los tesoros valiosos
perderTesorosValiosos :: Pirata -> Pirata
perderTesorosValiosos pirata = perderTesorosSegun noEsValioso pirata

-- Perder todos los tesoros con un nombre dado.
perderTesorosPorNombre :: String -> Pirata -> Pirata
perderTesorosPorNombre nombre  pirata = perderTesorosSegun (not . seLlama nombre)  pirata

--esParteDelBotinConOtroValor :: Pirata -> CondicionTesoro
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
saqueoPorPalabraClave :: String -> CondicionTesoro
--saqueoPorPalabraClave palabra = elem palabra . words . nombreTesoro
--saqueoPorPalabraClave palabra = (palabra ==) . nombreTesoro
saqueoPorPalabraClave palabra tesoro = nombreTesoro tesoro == palabra

-- Piratas con corazón 
saqueoBuenCorazon :: CondicionTesoro
--saqueoBuenCorazon = const False
saqueoBuenCorazon _ = False

-- Saqueos complejos 
saqueoComplejo :: [CondicionTesoro] -> CondicionTesoro
--saqueoComplejo saqueos = flip any saqueos . flip ($)
saqueoComplejo saqueos tesoro = any ( verificaTesoro tesoro) saqueos 

verificaTesoro:: Tesoro -> CondicionTesoro -> Bool
verificaTesoro tesoro saqueo = saqueo tesoro

jackSparrow = Pirata {
    nombrePirata = "Jack Sparrow",
--    condicionSaqueo = saqueoComplejo [esValioso, saqueoPorPalabraClave "sombrero"],
    botin = [brujulaQueApunta, frascoDeArenaConValorCero]
}

davidJones = Pirata {
    nombrePirata = "David Jones",
    botin = [cajitaMusical]
}

anneBonny = Pirata {
    nombrePirata = "Anne Bonny",
    botin = [doblonesDeOro, frascoDeArenaConValorUno]
}

elizabethSwann = Pirata {
    nombrePirata = "Elizabeth Swann",
    botin = [monedaDelCofreDelMuerto, espadaDeHierro]
}

willTurner = Pirata {
    nombrePirata = "Will Turner",
    botin = [cuchilloDelPadre]
}

saquear :: CondicionTesoro -> Pirata -> Tesoro -> Pirata
saquear condTesoro pirata tesoro
    | condTesoro tesoro = agregarTesoro tesoro pirata
    | otherwise         = pirata

-- Navegando los siete mares

type Tripulacion = [Pirata]

data Barco = Barco {
    nombreBarco :: String,
    tripulacion :: Tripulacion,
    modoSaqueo :: CondicionTesoro
} deriving (Show)

perlaNegra = Barco {
    nombreBarco = "Perla Negra",
    tripulacion = [jackSparrow, anneBonny],
    modoSaqueo = saqueoComplejo [esValioso, saqueoPorPalabraClave "sombrero"]
}

holandesErrante = Barco {
    nombreBarco = "Holandes Errante",
    tripulacion = [davidJones],
    modoSaqueo = saqueoPorPalabraClave "cajita"
}

incorporarPirata :: Pirata -> Barco -> Barco
incorporarPirata pirata barco = barco {tripulacion = pirata : tripulacion barco}

abandonarBarco :: Pirata -> Barco -> Barco
abandonarBarco pirata barco = barco {tripulacion = filter (not . validarIdentidad pirata) (tripulacion barco)}

validarIdentidad :: Pirata -> Pirata -> Bool
validarIdentidad unPirata otroPirata = nombrePirata unPirata == nombrePirata otroPirata

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
atacarCiudad  ciudad barco = barco {tripulacion = zipWith (saquear (modoSaqueo barco)) (tripulacion barco) ciudad}

-- Abordar barco 
-- No hace nada interesante, solo para probar

abordarBarco :: Barco -> Barco -> Barco
abordarBarco atacante defensor = atacante


