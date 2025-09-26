-- 1. Tipos a definir:
-- Se utiliza Float porque algunas funciones requerían este tipo

--      - Point. Un punto 2D en el espacio.
type Point = (Float,Float)

--      - Vector. Vector siempre se considera que empieza en (0,0)
type Vector = (Float,Float)

--      - Angle. Un angulo con decimales
type Angle = Float

--      - Distance. Un valor de distancia con decimales.
type Distance = Float

--      - Position. Representa la posición de objeto en un mundo 2D.
type Position = Point

--      - Size. Tipo necesario para la función isInBounds. Representa el ancho y alto del rectángulo que delimita el espacio
type Size = (Float,Float)


--   2. Definir las siguientes funciones:

--       - distanceBetween :: Position -> Position -> Distance. Calcula la distancia euclidiana entre dos posiciones en el espacio. Toma dos puntos como entrada y devuelve la distancia lineal que los separa.
-- Teorema de Pitágoras: sqrt((x1 - x2)^2 + (y1 - y2)^2)
distanceBetween :: Position -> Position -> Distance
distanceBetween p1 p2 = sqrt ((fst p1 - fst p2)**2 + (snd p1 - snd p2)**2)

--       - angleToTarget :: Position -> Position -> Angle. Determina el ángulo desde una posición origen hacia una posición objetivo. Útil para calcular la dirección en la que debe apuntar o moverse un objeto.
-- El ángulo viene dado por la fórmula: arctan ((y2 - y1) / (x2 - x1))
angleToTarget :: Position -> Position -> Angle
angleToTarget p1 p2 = atan ((snd p2 - snd p1) / (fst p2 - fst p1))

--       - deg2rad :: Angle -> Angle. Convierte un ángulo expresado en grados a su equivalente en radianes.
-- Utilizamos el factor de conversión adecuado
deg2rad :: Angle -> Angle
deg2rad a = a * 2*pi / 360

--       - rad2deg :: Angle -> Angle. Convierte un ángulo expresado en radianes a su equivalente en grados.
-- Utilizamos el factor de conversión adecuado
rad2deg :: Angle -> Angle
rad2deg a = a * 360 / (2*pi)

--       - subVec :: Vector -> Vector -> Vector. Realiza la resta de dos vectores, devolviendo un nuevo vector que representa la diferencia entre ellos.
-- La resta de vectores viene dada por la fórmula: (x1 - x2, y1 - y2)
subVec :: Vector -> Vector -> Vector
subVec v1 v2 = (fst v1 - fst v2, snd v1 - snd v2)

--       - getVertices :: (Point, Point, Point, Point, Angle) -> [Point]. Genera una lista de vértices (puntos) a partir de cuatro puntos base y un ángulo de rotación.

-- rotarPunto :: Point -> Angle -> Point. Función auxiliar. Rota un punto el ángulo dado.
{- Las coordenadas del nuevo punto se calculan de la siguiente manera:
        Coordenada x: cos(a + b) * sqrt(x^2 + y^2)
        Coordenada y: sin(a + b) * sqrt(x^2 + y^2)
    siendo b el ángulo entre el punto p y el eje X -}
rotarPunto :: Point -> Angle -> Point
rotarPunto p a = (cos (a + angleToTarget (0,0) p) * sqrt ((fst p)**2 + (snd p)**2), sin (a + angleToTarget (0,0) p) * sqrt ((fst p)**2 + (snd p)**2))

-- Utiliza rotarPunto para cada uno de los 4 puntos.
getVertices :: (Point, Point, Point, Point, Angle) -> [Point]
getVertices (p1, p2, p3, p4, a) = [rotarPunto p1 a, rotarPunto p2 a, rotarPunto p3 a, rotarPunto p4 a]

--       - dot :: Point -> Point -> Float. Calcula el producto escalar (dot product) entre dos puntos tratados como vectores
dot :: Point -> Point -> Float
-- La formula del producto escalar es: (x1*x2) + (y1*y2)
dot p1 p2 = fst p1 * fst p2 + snd p1 * snd p2

--       - sub :: Point -> Point -> Point. Resta un punto de otro, devolviendo un nuevo punto que representa la diferencia entre las coordenadas.
-- La resta de puntos viene dada por la fórmula: (x1 - x2, y1 - y2)
sub :: Point -> Point -> Point
sub p1 p2 = (fst p1 - fst p2, snd p1 - snd p2)

--       - perp :: Vector -> Vector. Calcula el vector perpendicular a un punto dado (tratado como vector).
-- El vector perpendicular a (x, y) es (-y, x)
perp :: Vector -> Vector
perp v = (- snd v, fst v)

--       - isInBounds :: Point -> Size -> Bool. Verifica si un punto se encuentra dentro de los límites definidos por un tamaño dado.
-- Hay que comprobar que x <= anchura and y <= altura
isInBounds :: Point -> Size -> Bool
isInBounds p s = (fst p <= fst s) && (snd p <= snd s)
