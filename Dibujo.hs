module Dibujo where


-- Se define los sinonimos de tipo
type Radio = Float 
type Lado = Float
type Base = Float
type Altura = Float


-- Se define el tipo Figura
data Figura = Circulo Radio        
            | Cuadrado Lado        
            | Rectangulo Lado Lado     
            | Triangulo Base Altura        
              deriving (Eq, Show)


-- Se define el lenguaje via constructores de tipo
data Dibujo a =  
             Basica a 
            | Rotar (Dibujo a) 
            | Rotar45 (Dibujo a)  
            | Espejar (Dibujo a) 
            | Apilar Float Float (Dibujo a) (Dibujo a) 
            | Juntar Float Float (Dibujo a) (Dibujo a)
            | Encimar (Dibujo a) (Dibujo a)
              deriving (Eq, Show)

 
-- Composición n-veces de una función con sí misma.
comp :: (a -> a) -> Int -> a -> a
comp f 0 x = x
comp f n x = comp f (n - 1) (f x)


r45 :: Dibujo a -> Dibujo a
r45 dibujo = Rotar45 dibujo

r90 :: Dibujo a -> Dibujo a
r90 dibujo = Rotar45 (Rotar45 dibujo)

-- Rotaciones de múltiplos de 90.
r180 :: Dibujo a -> Dibujo a
r180 dibujo = comp r45 4 dibujo


r270 :: Dibujo a -> Dibujo a
r270 dibujo = comp r45 6 dibujo


-- Pone una figura sobre la otra, ambas ocupan el mismo espacio.
(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) dibujo1 dibujo2 = Apilar 1 1 dibujo1 dibujo2


-- Pone una figura al lado de la otra, ambas ocupan el mismo espacio.
(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///) dibujo1 dibujo2 = Juntar 1 1 dibujo1 dibujo2


-- Superpone una figura con otra.
(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^) dibujo1 dibujo2 = Encimar dibujo1 dibujo2


-- Dadas cuatro dibujos las ubica en los cuatro cuadrantes.
cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto dibujo1 dibujo2 dibujo3 dibujo4 = (///) ((.-.) dibujo1 dibujo2) ((.-.) dibujo3 dibujo4)

-- Una dibujo repetido con las cuatro rotaciones, superpuestas.
encimar4 :: Dibujo a -> Dibujo a
encimar4 dibujo1 =  (^^^) ((^^^) (r270 dibujo1) (r180 dibujo1)) ((^^^) (r90 dibujo1) dibujo1)


-- Cuadrado con la misma figura rotada i * 90, para i ∈ {0, ..., 3}.
-- No confundir con encimar4!
ciclar :: Dibujo a -> Dibujo a
ciclar dibujo1 = cuarteto dibujo1 (r90 dibujo1) (r180 dibujo1) (r270 dibujo1) 


-- Transfomar un valor de tipo a como una Basica.
pureDib :: a -> Dibujo a
pureDib a = Basica a

-- map para nuestro lenguaje.
mapDib :: (a -> b) -> Dibujo a -> Dibujo b
mapDib f (Basica x) = Basica (f x)
mapDib f (Rotar45 x) = Rotar45 (mapDib f x)
mapDib f (Rotar x) = Rotar (mapDib f x)
mapDib f (Espejar x) = Espejar (mapDib f x)
mapDib f (Apilar p q x y) = Apilar p q (mapDib f x) (mapDib f y)
mapDib f (Juntar p q x y) = Juntar p q (mapDib f x) (mapDib f y)
mapDib f (Encimar x y) = Encimar (mapDib f x) (mapDib f y)

-- Función de fold para Dibujos a
foldDib :: (a -> b) -- Función para 'Basica'
        -> (b -> b) -- Función para 'Rotar'
        -> (b -> b) -- Función para 'Rotar45'
        -> (b -> b) -- Función para 'Espejar'
        -> (Float -> Float -> b -> b -> b) -- Función para 'Apilar'
        -> (Float -> Float -> b -> b -> b) -- Función para 'Juntar'
        -> (b -> b -> b) -- Función para 'Encinar'
        -> Dibujo a -- Dibujo a plegar
        -> b

foldDib fBasica fRotar fRotar45 fEspejar fApilar fJuntar fEncimar dibujo =
  case dibujo of
    Basica x -> fBasica x
    Rotar x -> fRotar (fold x)
    Rotar45 x -> fRotar45 (fold x)
    Espejar x -> fEspejar (fold x)
    Apilar p q x y -> fApilar p q (fold x) (fold y)
    Juntar p q x y -> fJuntar p q (fold x) (fold y)
    Encimar x y -> fEncimar (fold x) (fold y)
  where
    fold = foldDib fBasica fRotar fRotar45 fEspejar fApilar fJuntar fEncimar