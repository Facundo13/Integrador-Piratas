import Text.Show.Functions -- Hace a las funciones instancia de Show

type CondicionTesoro = Tesoro -> Bool

data Tesoro = Tesoro {
    nombreTesoro :: String,
    valor :: Int
} deriving (Show, Eq)

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

cantidadTesoros :: Pirata -> Int
cantidadTesoros = length . botin

valoresDeTesoros :: Pirata -> [Int]
valoresDeTesoros pirata = map valor (botin pirata)

valorTotalBotin :: Pirata -> Int
valorTotalBotin = sum . valoresDeTesoros

esAfortunado :: Pirata -> Bool
esAfortunado = (> 10000) . valorTotalBotin

valorTesoroMasValioso :: Pirata -> Int
valorTesoroMasValioso = maximum . valoresDeTesoros

agregarTesoro :: Tesoro -> Pirata -> Pirata
agregarTesoro tesoro pirata = pirata { botin = tesoro : botin pirata }

perderTesorosSegun :: CondicionTesoro -> Pirata -> Pirata
perderTesorosSegun condTesoro pirata = pirata {botin = filter condTesoro $ botin pirata}

perderTesorosValiosos :: Pirata -> Pirata
perderTesorosValiosos = perderTesorosSegun noEsValioso

perderTesorosPorNombre :: String -> Pirata -> Pirata
perderTesorosPorNombre nombre  = perderTesorosSegun (not . seLlama nombre) 

esParteDelBotinConOtroValor :: Pirata -> CondicionTesoro
esParteDelBotinConOtroValor pirata tesoro = any (mismoNombreDistintoValor tesoro) (botin pirata)

tienenMismoTesoroConOtroValor :: Pirata -> Pirata -> Bool
tienenMismoTesoroConOtroValor unPirata otroPirata = any (esParteDelBotinConOtroValor otroPirata) (botin unPirata)

saqueoPorPalabraClave :: String -> CondicionTesoro
saqueoPorPalabraClave palabra = elem palabra . words . nombreTesoro

saqueoBuenCorazon :: CondicionTesoro
saqueoBuenCorazon = const False

saqueoComplejo :: [CondicionTesoro] -> CondicionTesoro
saqueoComplejo saqueos = flip any saqueos . flip ($)

jack = Pirata {
    nombrePirata = "Jack Sparrow",
    condicionSaqueo = saqueoComplejo [esValioso, saqueoPorPalabraClave "sombrero"],
    botin = [Tesoro "brujula" 10000, Tesoro "frasco de arena" 0]
}

pulpo = Pirata {
    nombrePirata = "David Jones",
    condicionSaqueo = saqueoBuenCorazon,
    botin = [Tesoro "cajita musical" 1]
}

anne = Pirata {
    nombrePirata = "Anne Bonny",
    condicionSaqueo = saqueoBuenCorazon,
    botin = [Tesoro "doblones" 1000, Tesoro "frasco de arena" 1]
}


------------------------


type Tripulacion = [Pirata]
type Isla = [Tesoro]

anclar :: Isla -> Tripulacion -> Tripulacion
anclar = zipWithRemanente agregarTesoro

type Ciudad = [Ciudadano]

data Ciudadano = Ciudadano {
    nombreCiudadano :: String,
    joyas :: [Tesoro]
} deriving (Show, Eq)

saquear :: Ciudad -> Tripulacion -> Tripulacion
saquear = zipWithRemanente (flip robar)

robar :: Pirata -> Ciudadano -> Pirata 
robar pirata = agregarTesoros pirata . filter (condicionSaqueo pirata) . joyas

agregarTesoros :: Pirata -> [Tesoro] -> Pirata
agregarTesoros pirata tesoros = pirata { botin = botin pirata ++ tesoros }

elizabeth = Ciudadano {
    nombreCiudadano = "Elizabeth Swann",
    joyas = [Tesoro "moneda del cofre muerto" 100]
}

will = Ciudadano {
    nombreCiudadano = "Will Turner",
    joyas = [Tesoro "cuchillo" 5]
}

perlaNegra = [jack, anne]

holandesErrante = [pulpo]

islaDelRon = repeat (Tesoro "Ron" 25)

portRoyal = [elizabeth, will]


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


willYElizabethPiratas = map (convertir saqueoBuenCorazon) portRoyal

piratasDelCaribe = luchar holandesErrante . anclar islaDelRon . flip (++) willYElizabethPiratas . saquear portRoyal $ perlaNegra