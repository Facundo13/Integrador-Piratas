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
    condicionSaqueo :: CondicionTesoro,
    botin :: [Tesoro]
} deriving (Show)

nombresDeTesoros :: Pirata -> [String]
nombresDeTesoros pirata = map nombreTesoro $ botin pirata

-- TESOROS PIRATAS

-- La cantidad de tesoros de un pirata
cantidadTesoros :: Pirata -> Int
cantidadTesoros = length . botin

valoresDeTesoros :: Pirata -> [Int]
valoresDeTesoros pirata = map valor (botin pirata)

valorTotalBotin :: Pirata -> Int
valorTotalBotin = sum . valoresDeTesoros

-- Si un pirata es afortunado
esAfortunado :: Pirata -> Bool
esAfortunado = (> 10000) . valorTotalBotin

-- El valor del tesoro más valioso 
valorTesoroMasValioso :: Pirata -> Int
valorTesoroMasValioso = maximum . valoresDeTesoros

-- Adquirir un nuevo tesoro
agregarTesoro :: Tesoro -> Pirata -> Pirata
agregarTesoro tesoro pirata = pirata { botin = tesoro : botin pirata }

perderTesorosSegun :: CondicionTesoro -> Pirata -> Pirata
perderTesorosSegun condTesoro pirata = pirata {botin = filter condTesoro $ botin pirata}

-- Perder todos los tesoros valiosos
perderTesorosValiosos :: Pirata -> Pirata
perderTesorosValiosos = perderTesorosSegun noEsValioso

-- Perder todos los tesoros con un nombre dado.
perderTesorosPorNombre :: String -> Pirata -> Pirata
perderTesorosPorNombre nombre  = perderTesorosSegun (not . seLlama nombre) 

esParteDelBotinConOtroValor :: Pirata -> CondicionTesoro
esParteDelBotinConOtroValor pirata tesoro = any (mismoNombreDistintoValor tesoro) (botin pirata)

-- Dos piratas tienen un mismo tesoro, pero de valor diferente
tienenMismoTesoroConOtroValor :: Pirata -> Pirata -> Bool
tienenMismoTesoroConOtroValor unPirata otroPirata = any (esParteDelBotinConOtroValor otroPirata) (botin unPirata)

-- TEMPORADA DE SAQUEOS

-- Tesoros con objetos específicos
saqueoPorPalabraClave :: String -> CondicionTesoro
saqueoPorPalabraClave palabra = elem palabra . words . nombreTesoro

-- Piratas con corazón 
saqueoBuenCorazon :: CondicionTesoro
saqueoBuenCorazon = const False

-- Saqueos complejos 
saqueoComplejo :: [CondicionTesoro] -> CondicionTesoro
saqueoComplejo saqueos = flip any saqueos . flip ($)

jackSparrow = Pirata {
    nombrePirata = "Jack Sparrow",
    condicionSaqueo = saqueoComplejo [esValioso, saqueoPorPalabraClave "sombrero"],
    botin = [brujulaQueApunta, frascoDeArenaConValorCero]
}

davidJones = Pirata {
    nombrePirata = "David Jones",
    condicionSaqueo = saqueoBuenCorazon,
    botin = [cajitaMusical]
}

anneBonny = Pirata {
    nombrePirata = "Anne Bonny",
    condicionSaqueo = saqueoPorPalabraClave "oro",
    botin = [doblonesDeOro, frascoDeArenaConValorUno]
}

elizabethSwann = Pirata {
    nombrePirata = "Elizabeth Swann",
    condicionSaqueo = saqueoBuenCorazon,
    botin = [monedaDelCofreDelMuerto, espadaDeHierro]
}

willTurner = Pirata {
    nombrePirata = "Will Turner",
    condicionSaqueo = saqueoBuenCorazon,
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

type Isla = [Tesoro]

islaDelRon = repeat ron
islaTortuga = repeat frascoDeArenaConValorUno

anclarEnUnaIsla :: Barco -> Isla -> Barco
anclarEnUnaIsla barco isla = barco {tripulacion = zipWith agregarTesoro isla (tripulacion barco)}

type Ciudad = [Tesoro]

portRoyal = [monedaDePlata, mapaDeBarbarroja, catalejoMagico]

carmenDePatagones = [sombreroDeAvestruz]

atacarCiudad :: Barco -> Ciudad -> Barco
atacarCiudad barco ciudad = barco {tripulacion = zipWith (saquear (modoSaqueo barco)) (tripulacion barco) ciudad}


-- saquear :: Ciudad -> Tripulacion -> Tripulacion
-- saquear = zipWithRemanente (flip robar)

data Ciudadano = Ciudadano {
    nombreCiudadano :: String,
    joyas :: [Tesoro]
} deriving (Show, Eq)

robar :: Pirata -> Ciudadano -> Pirata 
robar pirata = agregarTesoros pirata . filter (condicionSaqueo pirata) . joyas

agregarTesoros :: Pirata -> [Tesoro] -> Pirata
agregarTesoros pirata tesoros = pirata { botin = botin pirata ++ tesoros }


-- elizabeth = Ciudadano {
--    nombreCiudadano = "Elizabeth Swann",
--    joyas = [Tesoro "moneda del cofre muerto" 100]
--}

-- will = Ciudadano {
--    nombreCiudadano = "Will Turner",
--    joyas = [Tesoro "cuchillo" 5]
-- }

-- portRoyal = [elizabeth, will]



----------------------------------------------


convertir :: CondicionTesoro -> Ciudadano -> Pirata
convertir condSaqueo ciudadano = Pirata {
    nombrePirata = nombreCiudadano ciudadano,
    condicionSaqueo = condSaqueo,
    botin = joyas ciudadano
}

pelear :: Pirata -> Pirata -> Pirata
pelear pirataMalo pirataBueno
    |nivelDeSaqueo pirataMalo > nivelDeSaqueo pirataBueno = vaciarBotin pirataBueno
    |otherwise = pirataBueno

nivelDeSaqueo :: Pirata -> Int
nivelDeSaqueo = sum . map valor . botin

vaciarBotin :: Pirata -> Pirata
vaciarBotin pirata = pirata { botin = [] }

luchar :: Tripulacion -> Tripulacion -> Tripulacion
luchar piratasMalos = zipWithRemanente pelear piratasMalos . filter (not . sinBotin)

sinBotin :: Pirata -> Bool
sinBotin = null . botin



zipWithRemanente f lista1 lista2 = listaNueva ++ drop (length listaNueva) lista2
    where listaNueva = zipWith f lista1 lista2

-------------------------------


-- willYElizabethPiratas = map (convertir saqueoBuenCorazon) portRoyal

-- piratasDelCaribe = luchar holandesErrante . anclar islaDelRon . flip (++) willYElizabethPiratas . saquear portRoyal $ perlaNegra