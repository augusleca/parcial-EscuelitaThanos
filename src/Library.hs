module Library where
import PdePreludat

-- PARTE 1

--1)

data Guantelete = UnGuantelete {
    material :: String
,   gemas :: [Gema]
}deriving (Show,Eq)

data Personaje = UnPersonaje {
    edad :: Number
,   energia :: Number
,   habilidades :: [String]
,   nombre :: String
,   planeta :: String
}deriving (Show,Eq)

type Universo = [Personaje]

guanteCompleto = UnGuantelete{
    material = "uru"
,   gemas = [laMente 20,elAlma "Volar",elEspacio,elPoder,elTiempo,gemaLoca elTiempo]
}

ironMan = UnPersonaje {
    edad = 50
,   energia = 100
,   habilidades = ["Volar","Laser","Saltar","Carisma"]
,   nombre = "Tony Stark"
,   planeta = "Tierra"
}

drStrange = UnPersonaje {
    edad = 45
,   energia = 200
,   habilidades = ["Tepearse","Curarse"]
,   nombre = "Stephen Strange"
,   planeta = "Tierra"
}

universoEj :: Universo
universoEj = [ironMan,drStrange,ironMan]

chasquidoUniverso :: Guantelete -> Universo -> Universo
chasquidoUniverso guante
    | length (gemas guante) == 6 = reducirAMitadUniverso
    | otherwise = id

reducirAMitadUniverso :: Universo -> [Personaje]
reducirAMitadUniverso universo = take (tomarMitadDeUniverso universo) universo

tomarMitadDeUniverso :: Universo -> Number
tomarMitadDeUniverso = floor.(/2).length

-- 2)

--a)
aptoPendex :: Universo -> Bool
aptoPendex universo = any (<45) (map edad universo)

--b)
energiaTotalUniverso :: Universo -> Number
energiaTotalUniverso = sum.(map energia).cualesTienenMasDe1Habilidad

cualesTienenMasDe1Habilidad :: Universo -> [Personaje]
cualesTienenMasDe1Habilidad universo = filter (tieneMasDeNHabilidades 1) universo

tieneMasDeNHabilidades :: Number -> Personaje -> Bool
tieneMasDeNHabilidades n = (>n).length.habilidades

-- PARTE 2

-- 3)

type Gema = Personaje -> Personaje
type Energia = Number
type Planeta = String

--a)
laMente :: Energia -> Gema
laMente = debilitarEnergia 

debilitarEnergia :: Energia -> Personaje -> Personaje
debilitarEnergia cantidadEnergia usuario = usuario {energia = (energia usuario) - cantidadEnergia}

--b)
elAlma :: String -> Gema --probar con filter (/=habilidad) listaHabilidades :D
elAlma habilidadABorrar = debilitarEnergia 10 . eliminarHabilidad habilidadABorrar

eliminarHabilidad :: String -> Personaje -> Personaje
eliminarHabilidad habilidadABorrar usuario = usuario {habilidades = (take (posicionDeHabilidad habilidadABorrar usuario) (habilidades usuario)) ++ (drop (1 + (posicionDeHabilidad habilidadABorrar usuario)) (habilidades usuario))}

posicionDeHabilidad :: String -> Personaje -> Number
posicionDeHabilidad habilidad usuario = length (takeWhile (/=habilidad) (habilidades usuario))

existeHabilidad :: String -> Personaje -> Bool
existeHabilidad habilidad usuario = elem habilidad (habilidades usuario)

--c)
elEspacio :: Gema
elEspacio = (debilitarEnergia 20).(teletransportarAUnPlaneta "Saturno")

teletransportarAUnPlaneta :: String -> Personaje -> Personaje
teletransportarAUnPlaneta planetaNuevo usuario = usuario {planeta = planetaNuevo}

--d)
elPoder :: Gema
elPoder usuario
    | tieneMasDeNHabilidades 2 usuario  =  usuario {habilidades = [], energia = 0}
    | otherwise = debilitarEnergia (energia usuario) usuario

tieneMenosDeNHabilidades :: Number -> Personaje -> Bool
tieneMenosDeNHabilidades n = (<n).length.habilidades

--e)
elTiempo :: Gema
elTiempo = (debilitarEnergia 50).reducirMitadEdad

reducirMitadEdad :: Personaje -> Personaje
reducirMitadEdad personaje = personaje {edad = max 18 (mitadDeEdad personaje)}

mitadDeEdad :: Personaje -> Number
mitadDeEdad = floor.(/2).edad

--f)
gemaLoca :: Gema -> Gema
gemaLoca gema = gema.gema

-- 4)

--a)
guanteGoma = UnGuantelete {
    material = "goma"
,   gemas = [elTiempo,elAlma "usar Mjolnir",gemaLoca (elAlma "programacion Haskell")]
}

-- 5)

--a)
listaGemasEj = [elTiempo,elTiempo,elAlma "usar Mjolnir"]

utilizarGemas :: Personaje -> [Gema] -> Personaje
utilizarGemas enemigo listaGemas = foldl utilizarUnaGema enemigo listaGemas

utilizarUnaGema :: Personaje -> Gema -> Personaje
utilizarUnaGema enemigo gema = gema enemigo

-- 6)

--a) 
listaGemas :: [Gema]
listaGemas = [elAlma "Hola",elEspacio,elPoder,elTiempo,laMente 10]

gemaMasPoderosa :: Guantelete -> Personaje -> Gema
gemaMasPoderosa guante victima = buscarGemaMasPoderosa victima (gemas guante)

buscarGemaMasPoderosa :: Personaje -> [Gema] -> Gema
buscarGemaMasPoderosa victima [gema] = gema -- Paso base si queda una gema
buscarGemaMasPoderosa victima [] = id -- Paso base si el guante no tiene gemas
buscarGemaMasPoderosa victima (gema1:gema2:gemas)
    | cualGemaEsMasPoderosa victima gema1 gema2 = buscarGemaMasPoderosa victima (gema2:gemas)
    | otherwise = buscarGemaMasPoderosa victima (gema1:gemas)

cualGemaEsMasPoderosa :: Personaje -> Gema -> Gema -> Bool
cualGemaEsMasPoderosa victima gema1 gema2
    | energia (gema1 victima) < energia (gema2 victima) = True
    | otherwise = False    

{- RESOLUCION DE PIPE USANDO MENOR SEGUN DE MINIGOLFITO:

minimoSegun :: Ord b => (a->b)->[a]->a
minimoSegun f = foldl1 (menorSegun f) 

menorSegun :: Ord x => (t->x)->(t->t->t)
menorSegun f a b
  | f a < f b = a
  | otherwise = b

gemaMasPoderosa :: Guantelete -> Personaje -> Gema
gemaMasPoderosa unGuantelete unPersonaje = minimoSegun (nivelEnergiaPostGema unPersonaje) (gemasEnGuantelete unGuantelete)

nivelEnergiaPostGema :: Personaje -> Gema -> Number
nivelEnergiaPostGema unPersonaje = energiaPersonaje . (implementarGema unPersonaje)

-}

-- 7) 

--a)
infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = UnGuantelete "vesconite" (infinitasGemas elTiempo)

{- Se puede utilizar debido que a Haskell es un lenguaje "Lazy evaluation",
   lo que indica que podra resolver las opciones planteadas ya que son las 
   primeras tres, podra resolver infinitamente, el tema es que la consola
   se trabara bastante. -}












