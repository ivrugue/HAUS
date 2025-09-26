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


-- Analiza el funcionamiento del juego y piensa en los tipos que son necesarios. Realiza una lista durante el análisis visual y posteriormente implementa dichos TADs.
{- Tipo Robot: Representa el robot (compuesto por el cuerpo y la torreta, que hemos separado porque se mueven de manera diferente) que se mueve por la pantalla y lanza proyectiles.
Sus atributos son:
    vida: Representa los puntos de vida que posee el robot
    posicion_robot: Posición del robot en la pantalla
    velocidad_robot: Velocidad que lleva el robot. En nuestra implementación está representada con un vector
    cooldown: Tiempo que es necesario esperar para lanzar un proyectil después de otro
    radar: Radio de detección del robot
-}
data Robot =
    Robot {
        vida :: Float,
        posicion_robot :: Position,
        velocidad_robot :: Vector,
        cooldown :: Float,
        radar :: Distance
    }
    deriving (Show, Eq)

{- Tipo Cuerpo: Representa el cuerpo del robot
Sus atributos son:
    imagen_cuerpo: Ruta a la imagen del cuerpo
    angulo_cuerpo: Ángulo del cuerpo con respecto a la horizontal
    robot_cuerpo: Robot al que pertenece el cuerpo
-}
data Cuerpo =
    Cuerpo {
        imagen_cuerpo :: String,
        angulo_cuerpo :: Angle,
        robot_cuerpo :: Robot
    }
    deriving (Show, Eq)

{- Tipo Torreta: Representa la torreta del robot
Sus atributos son:
    imagen_torreta: Ruta a la imagen de la torreta
    angulo_torreta: Ángulo de la torreta con respecto a la horizontal
    robot_torreta: Robot al que pertenece la torreta
-}
data Torreta =
    Torreta {
        imagen_torreta :: String,
        angulo_torreta :: Angle,
        robot_torreta :: Robot
    }
    deriving (Show, Eq)

{- Tipo Proyectil: Representa el proyectil que lanzan los robots
Sus atributos son:
    imagen_proyectil: Ruta a la imagen del proyectil
    posicion_proyectil: Posición del proyectil en la pantalla
    angulo_proyectil: Ángulo que tiene el proyectil con respecto a la horizontal
    velocidad_proyectil: Velocidad que lleva el proyectil. En nuestra implementación está representada con un vector
    alcance: Distancia a la que llega el proyectil desde la posición desde la que se lanza
    daño_proyectil: Puntos de vida que quita el proyectil al estar en contacto con un robot (distinto del que lo ha lanzado)
    robot_proyectil: Robot al que pertenece el proyectil
    activo_proyectil: Determina si el proyectil se está mostrando o no
-}
data Proyectil =
    Proyectil {
        imagen_proyectil :: String,
        posicion_proyectil :: Position,
        angulo_proyectil :: Angle,
        velocidad_proyectil :: Vector,
        alcance :: Distance,
        daño_proyectil :: Float,
        robot_proyectil :: Robot,
        activo_proyectil :: Bool
    }
    deriving (Show, Eq)

{- Tipo Explosión: Representa la explosión de un robot
Sus atributos son:
    imagen_explosion: Ruta a la imagen de la explosión
    posicion_explosion: Posición de la explosión en la pantalla
    daño_explosion: Puntos de vida que quita la explosión al estar en contacto con un robot
    activo_explosion: Determina si la explosión se está mostrando o no
-}
data Explosion =
    Explosion {
        imagen_explosion :: String,
        posicion_explosion :: Position,
        daño_explosion :: Float,
        activo_explosion :: Bool
    }
    deriving (Show, Eq)

{- Tipo Barra: Representa la vida de cada robot en cada instante (aparece arriba de cada robot)
Su atributo es:
    robot: Robot al que pertenece la barra de vida
-}
data Barra =
    Barra {
        robot :: Robot
    }
    deriving (Show, Eq)

-- Aclaración: No hemos considerado necesario incluir tipos para el marcador o el escenario 


--  Definir las siguientes funciones:

--  distanceBetween :: Position -> Position -> Distance. Calcula la distancia euclidiana entre dos posiciones en el espacio. Toma dos puntos como entrada y devuelve la distancia lineal que los separa.
-- Teorema de Pitágoras: sqrt((x1 - x2)^2 + (y1 - y2)^2)
distanceBetween :: Position -> Position -> Distance
distanceBetween (x1,y1) (x2,y2) = sqrt ((x1 - x2)**2 + (y1 - y2)**2)

--  angleToTarget :: Position -> Position -> Angle. Determina el ángulo desde una posición origen hacia una posición objetivo. Útil para calcular la dirección en la que debe apuntar o moverse un objeto.
-- El ángulo viene dado por la fórmula: arctan ((y2 - y1) / (x2 - x1))
angleToTarget :: Position -> Position -> Radian
angleToTarget (x1,y1) (x2, y2) = atan ((y2 - y1) / (x2 - x1))

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

--  getVertices :: (Point, Point, Point, Point, Angle) -> [Point]. Genera una lista de vértices (puntos) a partir de cuatro puntos base y un ángulo de rotación.
{- Las coordenadas del nuevo punto se calculan de la siguiente manera:
        Coordenada x: cos(a + b) * sqrt(x^2 + y^2)
        Coordenada y: sin(a + b) * sqrt(x^2 + y^2)
    siendo b el ángulo entre el punto p y la horizontal (paralela al eje X) que pasa por el centroide -}
{- Añadido en HAUS2: En la primera entrega supusimos que el giro se hacía con respecto al origen de coordenadas.
Como intuimos que esta función se va a utilizar para girar los tanques del juego, para esta segunda tarea hemos
decidido cambiar la función para calcular los giros con respecto al centroide de la figura definida por los puntos.
Para calcularlo de una manera sencilla, hacemos la media de las coordenadas x e y -}
getVertices :: (Point, Point, Point, Point, Radian) -> [Point]
getVertices ((x1,y1), (x2,y2), (x3,y3), (x4,y4), a) = [rotarPunto p | p <- [(x1,y1), (x2,y2), (x3,y3), (x4,y4)]]
    where
        centroide = ((sum [x1, x2, x3, x4])/4, (sum [y1, y2, y3, y4])/4)
        rotarPunto (x,y) = (cos (a + angleToTarget (x,y) centroide) * sqrt (x**2 + y**2), sin (a + angleToTarget (x,y) centroide) * sqrt (x**2 + y**2))

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
isInBounds (x,y) (w,h)
    | x < 0 || y < 0 = False
    | otherwise = (x <= w) && (y <= h)


-- Implementa las siguientes funciones usando pattern matching con los TADs definidos anteriormente:
  
-- detectedAgent: Determinar si un agente ha detectado a otro en caso de encontrarse dentro del rango de su radar.
-- Comparamos a partir de distanceBetween la distancia que hay entre el robot que detecta y el robot objetivo, y si esa distancia es menor 
-- que la distancia máxima a la que un robot puede detectar a otro.
detectedAgent :: Robot -> Robot -> Bool
detectedAgent robot objetivo =
    distanceBetween (posicion_robot robot) (posicion_robot objetivo) <= radar robot 
  
-- isRobotAlive: True si la energía del robot es mayor a 0.
-- Toma la vida del robot y la compara si es mayor que 0.
isRobotAlive :: Robot -> Bool
isRobotAlive robot = vida robot > 0

-- countActiveRobots: Contar los robots que están vivos.
-- Filtramos los robots que están vivos a través de una lista por compresión, y contamos los que cumplen la condición con length.
countActiveRobots :: [Robot] -> Int 
countActiveRobots robots = length [r | r <- robots, isRobotAlive r]
  
-- updateRobotVelocity: Actualiza la velocidad de un robot con una velocidad dada.
-- Nota: devuelve un Robot con el valor actualizado, pero no cambia el valor del Robot original.
updateRobotVelocity :: Robot -> Vector -> Robot
updateRobotVelocity robot newV = robot {velocidad_robot = newV}

-- updateVelocity: Actualizar velocidad basada en la acción de movimiento informada por el bot (ver notas).
-- Nota: devuelve un Robot con el valor actualizado, pero no cambia el valor del Robot original.
-- Dada la dirección (derecha=r, izquierda=l, arriba=u, abajo=d), incrementará su velocidad en n unidades en esa dirección
updateVelocity :: Robot -> Char -> Float -> Robot
updateVelocity robot instruccion n
    | null dir = robot
    | otherwise = updateRobotVelocity robot (sumVec (velocidad_robot robot) (head dir))
        where dir = [d | (i,d) <- [('r',(n,0)),('l',(-n,0)),('u',(0,n)),('d',(0,-n))], i == instruccion]

-- updatePosition: Actualizar una posición en función de la velocidad y el incremento de tiempo.
-- Nota: devuelve un Robot con el valor actualizado, pero no cambia el valor del Robot original.
-- Calcula la posición actualizada a través de la fórmula: posición + velocidad * tiempo
updatePosition :: Robot -> Float -> Robot
updatePosition robot dt = robot {posicion_robot = newPosicion}
    where
        (px, py) = posicion_robot robot
        (vx, vy) = velocidad_robot robot
        newPosicion = (px + vx * dt, py + vy * dt)

-- mul: tal que (w,h) `mul` (sw,sh) = (w * sw, h * sh)
-- Multiplica los componentes de dos tuplas.
mul :: Fractional a => (a, a) -> (a, a) -> (a, a)
mul (w, h) (sw, sh) = (w * sw, h * sh)