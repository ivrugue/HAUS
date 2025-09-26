-- Tipos a definir:
-- Se utiliza Float porque algunas funciones requerían este tipo

-- Point. Un punto 2D en el espacio.
type Point = (Float,Float)

-- Vector. Vector siempre se considera que empieza en (0,0)
type Vector = (Float,Float)

-- Angle. Un angulo con decimales
type Angle = Float
-- Añadido en HAUS2: Creamos dos "subtipos" para diferenciar cuándo el ángulo está en grados y cuándo en radianes
type Degree = Angle
type Radian = Angle

-- Distance. Un valor de distancia con decimales.
type Distance = Float

-- Position. Representa la posición de objeto en un mundo 2D.
type Position = Point

-- Size. Tipo necesario para la función isInBounds. Representa el ancho y alto del rectángulo que delimita el espacio
type Size = (Float,Float)


-- Acciones de movimiento
data AccionAvance = Forwards | Backwards | StopAvance
    deriving (Show, Eq)

data AccionGiro = TurnRight | TurnLeft | StopGiro
    deriving (Show, Eq)


-- Analiza el funcionamiento del juego y piensa en los tipos que son necesarios. Realiza una lista durante el análisis visual y posteriormente implementa dichos TADs.

{- Tipo Torreta: Representa la torreta del robot
Sus atributos son:
    imagen_torreta: Ruta a la imagen de la torreta
    puntos_torreta: Posición de los 4 puntos de la torreta en la pantalla
    eje: Eje de rotación de la torreta, se corresponderá con el centro del robot
    angulo_torreta: Dirección a la que apunta la torreta, que puede ser distinta a la del robot
-}
data Torreta =
    Torreta {
        imagen_torreta :: String,
        puntos_torreta :: [Position],
        eje :: Position,
        angulo_torreta :: Radian
    }
    deriving (Show, Eq)

{- Tipo Robot: Representa el robot que se mueve por la pantalla y lanza proyectiles.
Sus atributos son:
    imagen_robot: Ruta a la imagen del robot
    vida: Representa los puntos de vida que posee el robot
    puntos_robot: Posición de los 4 puntos del robot en la pantalla. De ellos se obtiene la posición del robot (centroide)
    angulo_robot: Dirección a la que apunta el robot
    velocidad_robot: Velocidad (escalar) que lleva el robot. Si es positiva implica avanzar, si es negativa implica retroceder
    vmax: Velocidad máxima que puede alcanzar el robot
    paso_giro: Ángulo en el que variará la dirección del robot al girar una vez
    aceleracion_avance: Magnitud en la que variará la velocidad al modificarse esta una vez
    cooldown: Tiempo que es necesario esperar para lanzar un proyectil después de otro
    radar: Radio de detección del robot
    torreta: Torreta asociada al robot
    barra: Barra de vida asociada al robot
-}
{- Nota: hemos decidido separar el vector velocidad en angulo_robot (dirección) y velocidad_robot (módulo y sentido) para facilitar las modificaciones en cada propiedad individualmente.
Para obtener el vector se puede emplear la función vecFromModuleAngle -}
data Robot =
    Robot {
        imagen_robot :: String,
        vida :: Float,
        puntos_robot :: [Position],
        angulo_robot :: Radian,
        velocidad_robot :: Float,
        vmax :: Float,
        paso_giro :: Radian,
        aceleracion_avance :: Float,
        cooldown :: Float,
        radar :: Distance,
        torreta :: Torreta,
        barra :: Barra
    }
    deriving (Show, Eq)


{- Tipo Proyectil: Representa el proyectil que lanzan los robots
Sus atributos son:
    imagen_proyectil: Ruta a la imagen del proyectil
    puntos_proyectil: Posición de los 4 puntos del proyectil en la pantalla. De ellos se obtiene la posición del proyectil (centroide).
    angulo_proyectil: Dirección a la que apunta el proyectil. Será la misma que la de la torreta al lanzarse el proyectil
    velocidad_proyectil: Velocidad (escalar) que lleva el proyectil. Siempre debes ser positiva, que implica avanzar
    alcance: Distancia a la que llega el proyectil desde la posición desde la que se lanza
    daño_proyectil: Puntos de vida que quita el proyectil al estar en contacto con un robot (distinto del que lo ha lanzado)
    robot_proyectil: Robot al que pertenece el proyectil
-}
data Proyectil =
    Proyectil {
        imagen_proyectil :: String,
        puntos_proyectil :: [Position],
        angulo_proyectil :: Radian,
        velocidad_proyectil :: Float,
        alcance :: Distance,
        daño_proyectil :: Float,
        robot_proyectil :: Robot
    }
    deriving (Show, Eq)

{- Tipo Explosión: Representa la explosión de un robot
Sus atributos son:
    imagen_explosion: Ruta a la imagen actual de la explosión (cuando aparezca la explosión cambiará continuamente hasta desaparecer)
    puntos_explosion: Posición de los 4 puntos de la explosión en la pantalla. De ellos se obtiene la posición de la explosión (centroide)
    daño_explosion: Puntos de vida que quita la explosión al estar en contacto con un robot
-}
data Explosion =
    Explosion {
        imagen_explosion :: String,
        puntos_explosion :: [Position],
        daño_explosion :: Float
    }
    deriving (Show, Eq)

{- Tipo Barra: Representa la vida de cada robot en cada instante (aparece arriba de cada robot)
Su atributo es:
    puntos_barra: Posición de los 4 puntos de la barra en la pantalla. De ellos se obtiene la posición de la barra (centroide)
-}
-- Nota: terminaremos de definir la barra cuando tengamos más información de cómo se implementará
data Barra =
    Barra {
        puntos_barra :: [Position]
    }
    deriving (Show, Eq)

{- Tipo Game: Representa el estado actual del juego
Sus atributos son:
    robots: Lista con los robots en el juego
    proyectiles: Lista con los proyectiles en el juego
    explosiones: Lista con las explosiones en el juego
    size: Tamaño del rectángulo que delimita el escenario
    fondo: Ruta a la imagen del fondo
-}
data Game =
    Game {
        robots :: [Robot],
        proyectiles :: [Proyectil],
        explosiones :: [Explosion],
        size :: Size,
        fondo :: String
    } 


--  Definir las siguientes funciones:

--  distanceBetween :: Position -> Position -> Distance. Calcula la distancia euclidiana entre dos posiciones en el espacio. Toma dos puntos como entrada y devuelve la distancia lineal que los separa.
-- Teorema de Pitágoras: sqrt((x1 - x2)^2 + (y1 - y2)^2)
distanceBetween :: Position -> Position -> Distance
distanceBetween (x1,y1) (x2,y2) = sqrt ((x1 - x2)**2 + (y1 - y2)**2)

--  angleToTarget :: Position -> Position -> Angle. Determina el ángulo desde una posición origen hacia una posición objetivo. Útil para calcular la dirección en la que debe apuntar o moverse un objeto.
-- El ángulo viene dado por la fórmula: arctan ((y2 - y1) / (x2 - x1))
angleToTarget :: Position -> Position -> Radian
angleToTarget (x1,y1) (x2, y2) = atan2 (y2 - y1) (x2 - x1)


--  deg2rad :: Angle -> Angle. Convierte un ángulo expresado en grados a su equivalente en radianes.
-- Utilizamos el factor de conversión adecuado
deg2rad :: Degree -> Radian
deg2rad a = a * 2*pi / 360

--  rad2deg :: Angle -> Angle. Convierte un ángulo expresado en radianes a su equivalente en grados.
-- Utilizamos el factor de conversión adecuado
rad2deg :: Radian -> Degree
rad2deg a = a * 360 / (2*pi)

--  sumVec :: Vector -> Vector -> Vector. Función auxiliar. Realiza la suma de dos vectores, devolviendo un nuevo vector que representa la suma entre ellos.
-- La suma de vectores viene dada por la fórmula: (x1 + x2, y1 + y2)
-- Añadido en el Haus2
sumVec :: Vector -> Vector -> Vector
sumVec (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

--  subVec :: Vector -> Vector -> Vector. Realiza la resta de dos vectores, devolviendo un nuevo vector que representa la diferencia entre ellos.
-- La resta de vectores viene dada por la fórmula: (x1 - x2, y1 - y2)
subVec :: Vector -> Vector -> Vector
subVec (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

-- centroide: media de las coordenadas (x,y) de una lista de puntos
centroide :: [Point] -> Point
centroide [] = 0
centroide ps = ((sum [x | (x,_) <- ps])/(fromIntegral (length ps)), (sum [y | (_,y) <- ps])/(fromIntegral (length ps)))
-- posicion: centroide de una lista de posiciones, por claridad para calcular la posición de un objeto
posicion :: [Position] -> Position
posicion = centroide

--  getVertices :: (Point, Point, Point, Point, Angle) -> [Point]. Genera una lista de vértices (puntos) a partir de cuatro puntos base y un ángulo de rotación.
{- Las coordenadas del nuevo punto se calculan de la siguiente manera:
        Coordenada x: cx + cos(a + b) * sqrt((x-cx)^2 + (y-cy)^2)
        Coordenada y: cy + sin(a + b) * sqrt((x-cx)^2 + (y-cy)^2)
    siendo b el ángulo entre el punto p y la horizontal (paralela al eje X) que pasa por el centroide -}
{- Añadido en HAUS2: En la primera entrega supusimos que el giro se hacía con respecto al origen de coordenadas.
Como intuimos que esta función se va a utilizar para girar los tanques del juego, ahora hemos decidido cambiar 
la función para calcular los giros con respecto al centroide (cx,yx) de la figura definida por los puntos. -}
-- Añadido en HAUS3: Hemos decidido generalizar la función añadiendo un parámetro adicional para especificar el centro de rotación
getVertices :: (Point, Point, Point, Point, Radian) -> Point -> [Point]
getVertices (p1, p2, p3, p4, a) (cx,cy) = [rotarPunto p | p <- [p1, p2, p3, p4]]
    where
        rotarPunto (x,y) =
            let r = sqrt ((x-cx)**2 + (y-cy)**2)
                b = angleToTarget (cx,cy) (x,y)
            in (cx + cos (a + b) * r, cy + sin (a + b) * r)

--  dot :: Point -> Point -> Float. Calcula el producto escalar (dot product) entre dos puntos tratados como vectores
-- La formula del producto escalar es: (x1*x2) + (y1*y2)
dot :: Point -> Point -> Float
dot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

--  sub :: Point -> Point -> Point. Resta un punto de otro, devolviendo un nuevo punto que representa la diferencia entre las coordenadas.
-- La resta de puntos viene dada por la fórmula: (x1 - x2, y1 - y2)
sub :: Point -> Point -> Point
sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

--  perp :: Vector -> Vector. Calcula el vector perpendicular a un punto dado (tratado como vector).
-- El vector perpendicular a (x, y) es (-y, x)
perp :: Vector -> Vector
perp (x,y) = (-y,x)

--  isInBounds :: Point -> Size -> Bool. Verifica si un punto se encuentra dentro de los límites definidos por un tamaño dado.
-- Hay que comprobar que 0 <= x <= anchura y 0 <= y <= altura.
-- Añadido en HAUS2: Suponemos que estamos siempre en el primer cuadrante (cuando construyamos el juego, estableceremos el origen de coordenadas en la esquina inferior izquierda)
isInBounds :: Point -> Size -> Bool
isInBounds (x,y) (w,h) = (0 <= x) && (x <= w) && (0 <= y) && (y <= h)


-- Implementa las siguientes funciones usando pattern matching con los TADs definidos anteriormente:
  
-- detectedAgent: Determinar si un agente ha detectado a otro en caso de encontrarse dentro del rango de su radar.
-- Comparamos a partir de distanceBetween la distancia que hay entre el robot que detecta y el robot objetivo, y si esa distancia es menor 
-- que la distancia máxima a la que un robot puede detectar a otro.
detectedAgent :: Robot -> Robot -> Bool
detectedAgent robot objetivo =
    distanceBetween (posicion (puntos_robot robot)) (posicion (puntos_robot objetivo)) <= radar robot 
  
-- isRobotAlive: True si la energía del robot es mayor a 0.
-- Toma la vida del robot y la compara si es mayor que 0.
isRobotAlive :: Robot -> Bool
isRobotAlive robot = vida robot > 0

-- countActiveRobots: Contar los robots que están vivos.
-- Filtramos los robots que están vivos a través de una lista por compresión, y contamos los que cumplen la condición con length.
countActiveRobots :: Game -> Int 
countActiveRobots game = length [r | r <- (robots game), isRobotAlive r]
  
-- updateRobotVelocity: Actualiza la velocidad de un robot con una velocidad dada.
-- Actualiza el módulo y el ángulo del vector velocidad, y además rota los puntos del robot según el nuevo ángulo
-- Nota: devuelve un Robot con el valor actualizado, pero no cambia el valor del Robot original.
updateRobotVelocity :: Robot -> Float -> Radian -> Robot
updateRobotVelocity robot newV newA = robot {velocidad_robot = newV, angulo_robot = newA, puntos_robot = newP}
    where
        incr_angulo = newA - (angulo_robot robot)
        [p1,p2,p3,p4] = puntos_robot robot
        newP = getVertices (p1,p2,p3,p4,incr_angulo) (posicion [p1,p2,p3,p4])

-- updateVelocity: Actualizar velocidad basada en la acción de movimiento informada por el bot (ver notas).
-- Nota: devuelve un Robot con el valor actualizado, pero no cambia el valor del Robot original.
{- Dada la acción de avance (Forwards, Backwards, StopAvance) cambiará su velocidad según el sentido correspondiente y la aceleración, 
y dada la acción de giro (TurnRight, TurnLeft, StopGiro), cambiará la dirección a la que avanza-}
updateVelocity :: Robot -> AccionAvance -> AccionGiro -> Robot
updateVelocity robot accion_a accion_g = updateRobotVelocity robot newV newA
    where
        v = velocidad_robot robot
        velMax = vmax robot
        acel = aceleracion_avance robot
        newV = case accion_a of
            StopAvance
                | (v == 0) || (abs v < abs acel) -> 0    -- si ya está parado o está a punto de parar, se para
                | v > 0 -> v - acel                      -- si se está moviendo, desacelera (= acelerar en la dirección opuesta)
                | v < 0 -> v + acel 
            Forwards
                | v > 0 -> min (v + acel) velMax         -- si ya está avanzando, acelerar solo si no ha llegado a vmax
                | otherwise -> v + acel                  -- si no, acelerar hacia delante
            Backwards
                | v < 0 -> max (v - acel) (-velMax)      -- si ya está retrocediendo, acelerar solo si no ha llegado a -vmax
                | otherwise -> v - acel                  -- si no, acelerar hacia atrás
        dirGiro
            | accion_g == TurnRight = 1
            | accion_g == TurnLeft = -1
            | otherwise = 0
        newA = dirGiro * (paso_giro robot) + (angulo_robot robot)

-- vecFromModuleAngle: Función auxiliar. Devuelve un vector dados el módulo (con signo según la dirección) y el ángulo del vector
vecFromModuleAngle :: Float -> Radian -> Vector
vecFromModuleAngle m a = (m * cos a, m * sin a)

-- updatePosition: Actualizar una posición en función de la velocidad y el incremento de tiempo.
-- Calcula para cada punto del robot la posición actualizada a través de la fórmula: posición + velocidad * tiempo
-- Nota: devuelve un Robot con el valor actualizado, pero no cambia el valor del Robot original.
updatePosition :: Robot -> Float -> Game -> Robot
updatePosition robot dt game = updatedRobot
    where
        (vx, vy) = vecFromModuleAngle (velocidad_robot robot) (angulo_robot robot)
        newP ps = [(px + vx * dt, py + vy * dt) | (px,py) <- ps]
        newPRobot = newP (puntos_robot robot)
        newPTorreta = newP (puntos_torreta (torreta robot))
        updatedRobot
            | isInBounds (posicion (newPRobot)) (size game) = robot {  -- el robot se moverá solo si su centro seguirá dentro de la pantalla
                puntos_robot = newPRobot, 
                torreta = (torreta robot) {puntos_torreta = newPTorreta, eje = (posicion newPRobot)}
              }
            | otherwise = robot

-- mul: tal que (w,h) `mul` (sw,sh) = (w * sw, h * sh)
-- Multiplica los componentes de dos tuplas.
mul :: Fractional a => (a, a) -> (a, a) -> (a, a)
mul (w, h) (sw, sh) = (w * sw, h * sh)