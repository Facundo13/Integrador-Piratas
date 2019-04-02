import Text.Show.Functions -- Hace a las funciones instancia de Show

data Tesoro = Tesoro {
    nombre :: String,
    valor :: Int
} deriving (Show, Eq)


data Pirata = Pirata {
    nombrePirata :: String,
    saqueo :: Saqueo,
    botin :: [Tesoro]
} deriving (Show)

type Saqueo = Tesoro -> Bool

valioso :: Saqueo
valioso = (>100) . valor

buscador :: String -> Saqueo
buscador palabra = elem palabra . words . nombre

sinSaqueo :: Saqueo
sinSaqueo = const False

complejo :: [Saqueo] -> Saqueo
complejo saqueos = flip any saqueos . flip ($)

jack = Pirata {
    nombrePirata = "Jack Sparrow",
    saqueo = complejo [valioso, buscador "sombrero"],
    botin = [Tesoro "brujula" 10000, Tesoro "frasco de arena" 0]
}

pulpo = Pirata {
    nombrePirata = "David Jones",
    saqueo = sinSaqueo,
    botin = [Tesoro "cajita musical" 1]
}

anne = Pirata {
    nombrePirata = "Anne Bonny",
    saqueo = sinSaqueo,
    botin = [Tesoro "doblones" 1000, Tesoro "frasco de arena" 1]
}


------------------------


type Tripulacion = [Pirata]
type Isla = [Tesoro]

anclar :: Isla -> Tripulacion -> Tripulacion
anclar = zipWithRemanente agregarTesoro

agregarTesoro :: Tesoro -> Pirata -> Pirata
agregarTesoro tesoro pirata = pirata { botin = tesoro : botin pirata }

type Ciudad = [Ciudadano]

data Ciudadano = Ciudadano {
    nombreCiudadano :: String,
    joyas :: [Tesoro]
} deriving (Show, Eq)

saquear :: Ciudad -> Tripulacion -> Tripulacion
saquear = zipWithRemanente (flip robar)

robar :: Pirata -> Ciudadano -> Pirata 
robar pirata = agregarTesoros pirata . filter (saqueo pirata) . joyas

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


convertir :: Saqueo -> Ciudadano -> Pirata
convertir saqueo ciudadano = Pirata {
    nombrePirata = nombreCiudadano ciudadano,
    saqueo = saqueo,
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


willYElizabethPiratas = map (convertir sinSaqueo) portRoyal

piratasDelCaribe = luchar holandesErrante . anclar islaDelRon . flip (++) willYElizabethPiratas . saquear portRoyal $ perlaNegra