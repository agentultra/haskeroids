{-# LANGUAGE DataKinds#-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asteroids.Game where

import Control.Monad (unless, when)
import Data.Foldable
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Deque.Strict (Deque)
import qualified Deque.Strict as D
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Vector.Storable as VS
import Foreign.C.Types
import qualified GHC.Exts as Exts
import Linear
import qualified SDL
import SDL (($=), WindowConfig (..))
import qualified SDL.Font as Font
import SDL.Mixer (Chunk)
import qualified SDL.Mixer as Mixer
import System.Random

import qualified Asteroids.Linear.Vector as AV

windowConfig :: WindowConfig
windowConfig
  = SDL.defaultWindow
  { windowHighDPI = True
  , windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL
  , windowPosition = SDL.Centered
  }

delta :: Float
delta = 0.1

topLeftV :: V2 Float
topLeftV = V2 (-0.8485288306602362) (-0.8485274441869115)

bottomRightV :: V2 Float
bottomRightV = V2 0.8485282760711782 0.8485279987765132

topRightV :: V2 Float
topRightV = V2 0.8485305610016184 (-0.8485257138391733)

bottomLeftV :: V2 Float
bottomLeftV = V2 (-0.8485243273607558) 0.8485319474698503

playFieldWidth :: Int
playFieldWidth = 800

playFieldHeight :: Int
playFieldHeight = 600

data KeyState = KeyDown | KeyRelease
  deriving (Eq, Show)

instance Semigroup KeyState where
  KeyDown <> KeyDown = KeyDown
  KeyDown <> KeyRelease = KeyDown
  KeyRelease <> KeyDown = KeyRelease
  KeyRelease <> KeyRelease = KeyRelease

keyDown :: KeyState -> Bool
keyDown KeyDown = True
keyDown _       = False

keyRelease :: KeyState -> Bool
keyRelease KeyRelease = True
keyRelease _          = False

data ButtonState
  = ButtonState
  { buttonStateUp           :: KeyState
  , buttonStateUpPressed    :: Bool
  , buttonStateDown         :: KeyState
  , buttonStateDownPressed  :: Bool
  , buttonStateLeft         :: KeyState
  , buttonStateLeftPressed  :: Bool
  , buttonStateRight        :: KeyState
  , buttonStateRightPressed :: Bool
  , buttonStateFire         :: KeyState
  , buttonStateFirePressed  :: Bool
  }
  deriving (Eq, Show)

initButtonState :: ButtonState
initButtonState
  = ButtonState
  { buttonStateUp           = KeyRelease
  , buttonStateUpPressed    = False
  , buttonStateDown         = KeyRelease
  , buttonStateDownPressed  = False
  , buttonStateLeft         = KeyRelease
  , buttonStateLeftPressed  = False
  , buttonStateRight        = KeyRelease
  , buttonStateRightPressed = False
  , buttonStateFire         = KeyRelease
  , buttonStateFirePressed  = False
  }

resetPressedFlags :: ButtonState -> ButtonState
resetPressedFlags buttons =
  buttons { buttonStateUpPressed    = False
          , buttonStateDownPressed  = False
          , buttonStateLeftPressed  = False
          , buttonStateRightPressed = False
          , buttonStateFirePressed  = False
          }

data GameStatus
  = GameOver
  | Main
  | MainMenu
  | ShouldQuit
  deriving (Eq, Show)

data Scene
  = Scene
  { sceneUpdate :: GameState -> GameState
  , sceneRender :: GameState -> IO ()
  }

runScene :: GameState -> Scene -> IO GameState
runScene gameState scene@Scene {..} = do
  let gameState' = (sceneUpdate gameState)
        { gameStateAccumulator = gameStateAccumulator gameState - delta
        , gameStateTicks = gameStateTicks gameState + delta
        }
  sceneRender gameState'
  if gameStateAccumulator gameState' <= delta
    then pure gameState'
    else runScene gameState' scene

data GameState
  = GameState
  { gameStateRenderer            :: SDL.Renderer
  , gameStateFps                 :: Float
  , gameStateTicks               :: Float
  , gameStateCurrentTime         :: Float
  , gameStateAccumulator         :: Float
  , gameStateButtonState         :: ButtonState
  , gameStatePlayerShip          :: Ship
  , gameStateBullets             :: Deque Bullet
  , gameStateBulletTimer         :: Float
  , gameStateBulletTimerMax      :: Float
  , gameStateBulletAgeMax        :: Int
  , gameStateAsteroids           :: Deque Asteroid
  , gameStateAsteroidSpawnTimer  :: Timer
  , gameStateAsteroidSpawnDelta  :: Float
  -- ^ The elapsed time interval before spawning a new asteroid
  , gameStateAsteroidSpawnFactor :: Float
  -- ^ The amount to decrease the spawn delta after detecting a bullet
  -- collision
  , gameStateScore               :: Int
  , gameStateFont32              :: Font.Font
  , gameStateFont16              :: Font.Font
  , gameStateRandGen             :: StdGen
  , gameStateStatus              :: GameStatus
  , gameStateScene               :: Scene
  , gameStateMainMenu            :: MainMenuState
  , gameStateSoundMap            :: Map Text Chunk
  , gameStateIOEvents            :: [IOEvent]
  }

data MainMenuState
  = MainMenuState
  { mainMenuCursorPosition :: Int
  , mainMenuChoices :: [MenuChoice]
  }
  deriving (Eq, Show)

data MenuChoice
  = Start
  | Quit
  deriving (Eq, Show)

menuDown :: MainMenuState -> MainMenuState
menuDown mainMenu =
  let nextCursorPosition =
        (mainMenuCursorPosition mainMenu + 1)
        `mod`
        length (mainMenuChoices mainMenu)
  in mainMenu { mainMenuCursorPosition = nextCursorPosition }

menuUp :: MainMenuState -> MainMenuState
menuUp mainMenu =
  let nextCursorPosition =
        (mainMenuCursorPosition mainMenu - 1)
        `mod`
        length (mainMenuChoices mainMenu)
  in mainMenu { mainMenuCursorPosition = nextCursorPosition }

menuSelect :: MainMenuState -> MenuChoice
menuSelect MainMenuState {..} =
  mainMenuChoices !! mainMenuCursorPosition

-- | Safely construct a main menu, avoid using the type constructor.
mkMenu :: [MenuChoice] -> MainMenuState
mkMenu = MainMenuState 0

data CollisionBox
  = CollisionBox
  { collisionBoxPosition :: V2 Float
  , collisionBoxSize     :: V2 Float
  }
  deriving (Eq, Show)

isCollidingBox :: CollisionBox -> CollisionBox -> Bool
isCollidingBox box other =
  let (V2 boxX boxY) = collisionBoxPosition box
      (V2 otherX otherY) = collisionBoxPosition other
      (V2 boxW boxH) = collisionBoxSize box
      (V2 otherW otherH) = collisionBoxSize other
  in overLap boxX boxW otherX otherW
     && overLap boxY boxH otherY otherH
  where
    overLap x xw y yw = x + xw >= y && x <= y + yw

collidingBoxRect :: CollisionBox -> SDL.Rectangle CInt
collidingBoxRect CollisionBox {..} = SDL.Rectangle
  (SDL.P (truncate <$> collisionBoxPosition))
  (truncate <$> collisionBoxSize)

class HasPosition a where
  getPosition :: a -> V2 Float
  setPosition :: a -> V2 Float -> a

class HasVelocity a where
  getVelocity :: a -> V2 Float
  setVelocity :: a -> V2 Float -> a

class HasCollisionBox a where
  getCollisionBox :: a -> CollisionBox

data Timer
  = Timer
  { timerStart   :: Float
  , timerElapsed :: Float
  }
  deriving (Eq, Show)

initTimer :: Float -> Timer
initTimer startTime = Timer startTime 0.0

updateTimer :: Timer -> Float -> Timer
updateTimer timer currentTime =
  let elapsed = currentTime - timerStart timer
  in timer { timerElapsed = elapsed }

resetTimer :: Timer -> Float -> Timer
resetTimer timer currentTime
  = timer
  { timerStart = currentTime
  , timerElapsed = 0.0
  }

data Ship
  = Ship
  { shipPosition      :: V2 Float
  , shipSize          :: CInt
  , shipRotation      :: Float
  , shipRotationSpeed :: Float
  , shipThrust        :: Float
  , shipThrustSpeed   :: Float
  , shipThrustMax     :: Float
  , shipAcceleration  :: V2 Float
  , shipMaxVelocity   :: Float
  , shipVelocity      :: V2 Float
  }
  deriving (Eq, Show)

instance HasPosition Ship where
  getPosition = shipPosition
  setPosition ship p = ship { shipPosition = p }

instance HasVelocity Ship where
  getVelocity = shipVelocity
  setVelocity ship v = ship { shipVelocity = v }

instance HasCollisionBox Ship where
  getCollisionBox Ship {..} =
    CollisionBox shipPosition (V2 5 5)

maxBullets :: Int
maxBullets = 5

data Bullet
  = Bullet
  { bulletPosition :: V2 Float
  , bulletVelocity :: V2 Float
  , bulletTick     :: Int
  , bulletIsDead   :: Bool
  }
  deriving (Eq, Show)

instance HasPosition Bullet where
  getPosition = bulletPosition
  setPosition bullet p = bullet { bulletPosition = p }

instance HasVelocity Bullet where
  getVelocity = bulletVelocity
  setVelocity bullet v = bullet { bulletVelocity = v }

instance HasCollisionBox Bullet where
  getCollisionBox Bullet {..} =
    CollisionBox bulletPosition (V2 10 10)

updatePosition :: (HasPosition a, HasVelocity a) => a -> Float -> a
updatePosition object dt =
  let position = getPosition object + getVelocity object ^* dt
  in setPosition object (wrapTorus position 10)

fireBullet
  :: V2 Float
  -> Float
  -> Int
  -> Deque Bullet
  -> Deque Bullet
fireBullet pos@(V2 px py) rot t bullets =
  let bx = 25 * cos rot + px
      by = 25 * sin rot + py
      bpos = V2 bx by
      bdir = bpos ^-^ pos
      bullet
        = Bullet
        { bulletPosition = bpos
        -- the unit vector * magnitude scalar
        , bulletVelocity = (bdir ^/ AV.length bdir) * 70
        , bulletTick  = t
        , bulletIsDead = False
        }
      bullets' = bullet `D.cons` bullets
  in D.take maxBullets bullets'

updateBulletPhysics :: Deque Bullet -> Deque Bullet
updateBulletPhysics = fmap (`updatePosition` delta)

updateBulletAge :: GameState -> GameState
updateBulletAge state = state
  { gameStateBullets = D.takeWhile young . gameStateBullets $ state
  }
  where
    young :: Bullet -> Bool
    young Bullet {..} =
      (truncate . gameStateTicks $ state) - bulletTick < gameStateBulletAgeMax state

newtype AsteroidPoints a
  = AsteroidPoints (a, a, a, a, a, a)
  deriving (Eq, Show)

instance Functor AsteroidPoints where
  fmap f (AsteroidPoints (a, b, c, d, e, f')) =
    AsteroidPoints (f a, f b, f c, f d, f e, f f')

maxAsteroids :: Int
maxAsteroids = 10

data AsteroidSize = Big | Small
  deriving (Eq, Show)

data Asteroid
  = Asteroid
  { asteroidPoints        :: AsteroidPoints (V2 Float)
  , asteroidPosition      :: V2 Float
  , asteroidVelocity      :: V2 Float
  , asteroidRotation      :: Float
  , asteroidRotationSpeed :: Float
  , asteroidIsDead        :: Bool
  , asteroidSize          :: AsteroidSize
  }
  deriving (Eq, Show)

instance HasPosition Asteroid where
  getPosition = asteroidPosition
  setPosition asteroid p = asteroid { asteroidPosition = p }

instance HasVelocity Asteroid where
  getVelocity = asteroidVelocity
  setVelocity asteroid v = asteroid { asteroidVelocity = v }

instance HasCollisionBox Asteroid where
  getCollisionBox Asteroid {..} =
    case asteroidSize of
      Small -> CollisionBox (smallOffsetPosition <$> asteroidPosition) (V2 40 50)
      Big -> CollisionBox (bigOffsetPosition asteroidPosition) (V2 80 60)
    where
      smallOffsetPosition x = x - 20
      bigOffsetPosition (V2 x y) = V2 (x - 40) (y - 30)

spawnAsteroid :: AsteroidSize -> V2 Float -> V2 Float -> Float -> Asteroid
spawnAsteroid size position velocity rotationSpeed
  = Asteroid
  { asteroidPoints = asteroidPointsOf size
  , asteroidPosition = position
  , asteroidVelocity = velocity
  , asteroidRotation = 0.0
  , asteroidRotationSpeed = rotationSpeed
  , asteroidIsDead = False
  , asteroidSize = size
  }
  where
    asteroidPointsOf :: AsteroidSize -> AsteroidPoints (V2 Float)
    asteroidPointsOf = \case
      Small -> AsteroidPoints
        ( V2 (-10) (-10)
        , V2 8 (-22)
        , V2 19 13
        , V2 (-10) 18
        , V2 (-18) 8
        , V2 (-20) 0
        )
      Big -> AsteroidPoints
        ( V2 (-20) (-20)
        , V2 16 (-38)
        , V2 44 27
        , V2 (-20) 42
        , V2 (-38) 18
        , V2 (-40) 2
        )

updateAsteroid :: Asteroid -> Asteroid
updateAsteroid a@Asteroid {..} =
  let asteroid = updatePosition a delta
  in asteroid { asteroidRotation = asteroidRotation + asteroidRotationSpeed * delta }

updateAsteroids :: Deque Asteroid ->  Deque Asteroid
updateAsteroids = fmap updateAsteroid

renderAsteroid :: Asteroid -> SDL.Renderer -> IO ()
renderAsteroid Asteroid {..} renderer = do
  SDL.rendererDrawColor renderer $= V4 255 0 0 255
  SDL.drawLines renderer
    . VS.fromList
    . rotatePoints asteroidPosition
    . translatePoints asteroidPosition
    . asteroidPointsToList
    $ asteroidPoints
    where
      asteroidPointsToList :: AsteroidPoints (V2 Float) -> [V2 Float]
      asteroidPointsToList (AsteroidPoints (p1, p2, p3, p4, p5, p6))
        = [p1, p2, p3, p4, p5, p6, p1]
      translatePoints :: V2 Float -> [V2 Float] -> [SDL.Point V2 CInt]
      translatePoints (V2 originX originY) ps =
        [ SDL.P (V2 (truncate $ px + originX) (truncate $ py + originY)) | (V2 px py) <- ps ]
      rotatePoints :: V2 Float -> [SDL.Point V2 CInt] -> [SDL.Point V2 CInt]
      rotatePoints origin =
        map (rotateOrigin (truncate <$> origin) asteroidRotation)

renderAsteroids :: Deque Asteroid -> SDL.Renderer -> IO ()
renderAsteroids asteroids renderer = forM_ asteroids $ \a -> renderAsteroid a renderer

spawnRandomAsteroid :: GameState -> GameState
spawnRandomAsteroid gameState
  | ((< maxAsteroids) . length . Exts.toList $ gameStateAsteroids gameState)
    && spawnTimerElapsed gameState =
    let randGen = gameStateRandGen gameState
        asteroids = gameStateAsteroids gameState
        -- 0: spawn from sides, 1: spawn from top
        (spawnSide, randGen') = uniformR (0, 1 :: Int) randGen
        (aPosX, randGen'')
            | spawnSide == 0 = uniformR (-70, 0 :: Int) randGen'
            | spawnSide == 1 = uniformR (-70, playFieldWidth + 70) randGen'
            | otherwise      =  error "Not possible"
        (aPosY, randGen''')
            | spawnSide == 0 = uniformR (-70, playFieldHeight + 70) randGen''
            | spawnSide == 1 = uniformR (-70, 0 :: Int) randGen''
            | otherwise      = error "Not possible"
        asteroidP = V2 (fromIntegral aPosX) (fromIntegral aPosY)
        asteroidV = asteroidP ^-^ V2 (fromIntegral $ playFieldWidth `div` 2) (fromIntegral $ playFieldHeight `div` 2)
        asteroidV' = asteroidV ^/ fromIntegral (length asteroidV) ^* 0.009
        asteroid = spawnAsteroid Big asteroidP asteroidV' 0.2
    in gameState { gameStateAsteroids = D.snoc asteroid asteroids
                 , gameStateRandGen = randGen'''
                 , gameStateAsteroidSpawnTimer = resetTimer (gameStateAsteroidSpawnTimer gameState) (gameStateCurrentTime gameState)
                 }
  | otherwise = gameState
  where
    spawnTimerElapsed :: GameState -> Bool
    spawnTimerElapsed GameState {..} =
      let Timer {..} = gameStateAsteroidSpawnTimer
      in timerElapsed >= gameStateAsteroidSpawnDelta

initAsteroids :: Deque Asteroid
initAsteroids = Exts.fromList [smallAsteroid, largeAsteroid]
  where
    smallAsteroid = spawnAsteroid Small (V2 200 200) (V2 1.2 1.2) 0.2
    largeAsteroid = spawnAsteroid Big (V2 400 100) (V2 1.2 1.2) 0.2

initAsteroidSpawnDelta :: Float
initAsteroidSpawnDelta = 15.0

initAsteroidSpawnFactor :: Float
initAsteroidSpawnFactor = 0.1

initPlayerShip :: Ship
initPlayerShip
  = Ship
  { shipPosition      = V2 20 20
  , shipSize          = 20
  , shipRotation      = 0.0
  , shipRotationSpeed = 0.1
  , shipThrust        = 0.0
  , shipThrustSpeed   = 0.2
  , shipThrustMax     = 3.0
  , shipAcceleration  = V2 0.0 0.0
  , shipMaxVelocity   = 49.0
  , shipVelocity      = V2 0.0 0.0
  }

initMainMenu :: MainMenuState
initMainMenu = mkMenu [Start, Quit]

initGameState :: SDL.Renderer -> Font.Font -> Font.Font -> Map Text Chunk -> IO GameState
initGameState renderer font32 font16 soundMap = do
  currentTime <- SDL.time
  let randGen = mkStdGen $ floor currentTime
  pure
    $ GameState
    { gameStateRenderer            = renderer
    , gameStateFps                 = 0.0
    , gameStateTicks               = 0.0
    , gameStateCurrentTime         = currentTime
    , gameStateAccumulator         = 0.0
    , gameStatePlayerShip          = initPlayerShip
    , gameStateButtonState         = initButtonState
    , gameStateBullets             = mempty
    , gameStateBulletTimer         = 0.0
    , gameStateBulletTimerMax      = 3.0
    , gameStateBulletAgeMax        = 150
    , gameStateAsteroids           = initAsteroids
    , gameStateAsteroidSpawnTimer  = initTimer currentTime
    , gameStateAsteroidSpawnDelta  = initAsteroidSpawnDelta
    , gameStateAsteroidSpawnFactor = initAsteroidSpawnFactor
    , gameStateScore               = 0
    , gameStateFont32              = font32
    , gameStateFont16              = font16
    , gameStateRandGen             = randGen
    , gameStateStatus              = MainMenu
    , gameStateScene               = mainMenuScene
    , gameStateMainMenu            = initMainMenu
    , gameStateIOEvents            = mempty
    , gameStateSoundMap            = soundMap
    }

run :: IO ()
run = do
  SDL.initializeAll

  window <- SDL.createWindow "Asteroids" windowConfig
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  Mixer.openAudio Mixer.defaultAudio 1024
  Font.initialize
  fNoticia32 <- Font.load "./assets/fonts/NoticiaText-Bold.ttf" 32
  fNoticia16 <- Font.load "./assets/fonts/NoticiaText-Bold.ttf" 16
  bulletSound <- Mixer.load "./assets/sounds/laserShoot.wav"
  smallExplosionSound <- Mixer.load "./assets/sounds/small-explosion.wav"
  bigExplosionSound <- Mixer.load "./assets/sounds/big-explosion.wav"
  let sounds
        = M.fromList
        [ ("fire-bullet", bulletSound)
        , ("small-explosion", smallExplosionSound)
        , ("big-explosion", bigExplosionSound)
        ]
  state <- initGameState renderer fNoticia32 fNoticia16 sounds

  loop state

  Font.quit
  Mixer.free bulletSound
  Mixer.free smallExplosionSound
  Mixer.free bigExplosionSound
  Mixer.closeAudio
  SDL.destroyRenderer renderer
  SDL.destroyWindow window

loop :: GameState -> IO ()
loop state@GameState {..} = do
  events <- SDL.pollEvents
  let eventIsQPress event = isKeyboardKey event SDL.Pressed SDL.KeycodeQ
      eventIsAPress event = isKeyboardKey event SDL.Pressed SDL.KeycodeA
      eventIsARelease event = isKeyboardKey event SDL.Released SDL.KeycodeA
      eventIsDPress event = isKeyboardKey event SDL.Pressed SDL.KeycodeD
      eventIsDRelease event = isKeyboardKey event SDL.Released SDL.KeycodeD
      eventIsWPress event = isKeyboardKey event SDL.Pressed SDL.KeycodeW
      eventIsWRelease event = isKeyboardKey event SDL.Released SDL.KeycodeW
      eventIsSPress event = isKeyboardKey event SDL.Pressed SDL.KeycodeS
      eventIsSRelease event = isKeyboardKey event SDL.Released SDL.KeycodeS
      eventIsSpacePress event = isKeyboardKey event SDL.Pressed SDL.KeycodeSpace
      eventIsSpaceRelease event = isKeyboardKey event SDL.Released SDL.KeycodeSpace
      qPressed = any eventIsQPress events
      aPressed = any eventIsAPress events
      aUp = any eventIsARelease events
      dPressed = any eventIsDPress events
      dUp = any eventIsDRelease events
      wPressed = any eventIsWPress events
      wUp = any eventIsWRelease events
      sPressed = any eventIsSPress events
      sUp = any eventIsSRelease events
      spacePressed = any eventIsSpacePress events
      spaceUp = any eventIsSpaceRelease events
      upButtonState
        | wPressed && keyRelease (buttonStateUp gameStateButtonState) = KeyDown
        | wUp && keyDown (buttonStateUp gameStateButtonState) = KeyRelease
        | otherwise = buttonStateUp gameStateButtonState
      leftButtonState
        | aPressed && keyRelease (buttonStateLeft gameStateButtonState) = KeyDown
        | aUp && keyDown (buttonStateLeft gameStateButtonState) = KeyRelease
        | otherwise = buttonStateLeft gameStateButtonState
      rightButtonState
        | dPressed && keyRelease (buttonStateRight gameStateButtonState) = KeyDown
        | dUp && keyDown (buttonStateRight gameStateButtonState) = KeyRelease
        | otherwise = buttonStateRight gameStateButtonState
      downButtonState
        | sPressed && keyRelease (buttonStateDown gameStateButtonState) = KeyDown
        | sUp && keyDown (buttonStateDown gameStateButtonState) = KeyRelease
        | otherwise = buttonStateDown gameStateButtonState
      fireButtonState
        | spacePressed && keyRelease (buttonStateFire gameStateButtonState) = KeyDown
        | spaceUp && keyDown (buttonStateFire gameStateButtonState) = KeyRelease
        | otherwise = buttonStateFire gameStateButtonState
      buttonState'
        = gameStateButtonState
        { buttonStateUp = upButtonState
        , buttonStateUpPressed = wPressed
        -- TODO (james): give this button states too
        , buttonStateDown = downButtonState
        , buttonStateDownPressed = sPressed
        , buttonStateLeft = leftButtonState
        , buttonStateLeftPressed = aPressed
        , buttonStateRight = rightButtonState
        , buttonStateRightPressed = dPressed
        , buttonStateFire = fireButtonState
        , buttonStateFirePressed = spacePressed
        }

  newTime <- SDL.time
  let frameTime = newTime - gameStateCurrentTime
      fps = 1 / frameTime
  state' <- runScene (state { gameStateCurrentTime = newTime
                            , gameStateAccumulator = gameStateAccumulator + frameTime
                            , gameStateButtonState = buttonState'
                            , gameStateFps         = fps
                            }) gameStateScene
  state'' <- runIOEvents state'
  unless (qPressed || gameStateStatus == ShouldQuit) $ do
    loop state'' { gameStatePlayerShip = (getShip state') { shipAcceleration = V2 0.0 0.0 }
                 , gameStateButtonState = resetPressedFlags buttonState'
                 }
  where
    getShip :: GameState -> Ship
    getShip GameState {..} = gameStatePlayerShip

newtype IOEvent
  = PlaySound Text
  deriving (Eq, Show)

addEvent :: GameState -> IOEvent -> GameState
addEvent gs event = gs { gameStateIOEvents = event : gameStateIOEvents gs }

runIOEvents :: GameState -> IO GameState
runIOEvents gs@GameState {..} = do
  forM_ gameStateIOEvents $ \case
    PlaySound soundName -> case M.lookup soundName gameStateSoundMap of
      Nothing -> pure ()
      Just chunk -> Mixer.play chunk
  pure gs { gameStateIOEvents = mempty }

mainScene :: Scene
mainScene
  = Scene
  { sceneUpdate = update
  , sceneRender = render
  }

update :: GameState -> GameState
update state@GameState {..} =
  let (V2 ax ay) = shipAcceleration gameStatePlayerShip
      turnAmount
        | keyDown $ buttonStateLeft gameStateButtonState = -(shipRotationSpeed gameStatePlayerShip)
        | keyDown $ buttonStateRight gameStateButtonState = shipRotationSpeed gameStatePlayerShip
        | otherwise = 0.0
      thrust
        | keyDown $ buttonStateUp gameStateButtonState =
          min
          (shipThrust gameStatePlayerShip + shipThrustSpeed gameStatePlayerShip)
          (shipThrustMax gameStatePlayerShip)
        | otherwise = 0.0
      acceleration
        | keyDown $ buttonStateUp gameStateButtonState
        = V2
          (ax + thrust * cos (shipRotation gameStatePlayerShip))
          (ay + thrust * sin (shipRotation gameStatePlayerShip))
        | otherwise = shipAcceleration gameStatePlayerShip
      velocity = shipVelocity gameStatePlayerShip + acceleration ^* delta
      bulletTimer
        | keyDown (buttonStateFire gameStateButtonState) && (gameStateBulletTimer + delta < gameStateBulletTimerMax) = gameStateBulletTimer + delta
        | otherwise = 0.0
      shouldFireBullet = keyDown (buttonStateFire gameStateButtonState) && (bulletTimer == 0)
      bullets = if shouldFireBullet
        then fireBullet
             (shipPosition gameStatePlayerShip)
             (shipRotation gameStatePlayerShip)
             (truncate gameStateTicks)
             gameStateBullets
        else gameStateBullets
      collisions = checkAsteroidCollisions (updateAsteroids gameStateAsteroids) (updateBulletPhysics bullets)
      shipCollision = checkShipCollision gameStatePlayerShip gameStateAsteroids
      state' = handleCollisionResults state $ collisions : maybeToList shipCollision
      ship = updatePosition gameStatePlayerShip delta
      nextState = state' { gameStatePlayerShip = ship
                           { shipRotation = shipRotation gameStatePlayerShip + turnAmount
                           , shipThrust = thrust
                           , shipAcceleration = acceleration
                           , shipVelocity = AV.clampVector velocity $ shipMaxVelocity gameStatePlayerShip
                          }
                         , gameStateAsteroidSpawnTimer = updateTimer gameStateAsteroidSpawnTimer gameStateCurrentTime
                         , gameStateBulletTimer = bulletTimer
                         }
      nextState'
        | shouldFireBullet = addEvent nextState $ PlaySound "fire-bullet"
        | otherwise = nextState
  in spawnRandomAsteroid . updateBulletAge $ nextState'

data CollisionResult
  = BulletAndAsteroidCollisions Int AsteroidSize (Deque Bullet) (Deque Asteroid)
  | ShipCollision
  deriving (Eq, Show)

handleCollisionResults :: GameState -> [CollisionResult] -> GameState
handleCollisionResults = foldl' handleCollision
  where
    handleCollision :: GameState -> CollisionResult -> GameState
    handleCollision gameState (BulletAndAsteroidCollisions numCols siz bs as) =
      let (randVelocityScale, randGen) = uniformR (2.0, 3.0) $ gameStateRandGen gameState
          nextSpawnDelta
            | numCols /= 0 =
              max (gameStateAsteroidSpawnDelta gameState - gameStateAsteroidSpawnFactor gameState) 1.0
            | otherwise = gameStateAsteroidSpawnDelta gameState
          nextSpawnFactor
            | numCols /= 0 = gameStateAsteroidSpawnFactor gameState * 1.1
            | otherwise    = gameStateAsteroidSpawnFactor gameState
          ioEvents
            | numCols /= 0 = PlaySound (asteroidSoundFromSize siz) : gameStateIOEvents gameState
            | otherwise    = mempty
      in gameState
         { gameStateBullets = filterDeadBullets bs
         , gameStateAsteroids = filterDeadAsteroids randVelocityScale as
         , gameStateScore = gameStateScore gameState + numCols
         , gameStateRandGen = randGen
         , gameStateAsteroidSpawnDelta = nextSpawnDelta
         , gameStateAsteroidSpawnFactor = nextSpawnFactor
         , gameStateIOEvents = ioEvents
         }
    handleCollision gameState ShipCollision
      = gameState
      { gameStateStatus = GameOver
      , gameStateScene = gameOverScene
      }

    asteroidSoundFromSize Big = "big-explosion"
    asteroidSoundFromSize Small = "small-explosion"

checkAsteroidCollisions :: Deque Asteroid -> Deque Bullet -> CollisionResult
checkAsteroidCollisions asteroids bullets =
  let (bs, as, colNum, siz) = foldl' handleBulletCollision (mempty, asteroids, 0, Small) bullets
  in BulletAndAsteroidCollisions colNum siz bs as
  where
    handleBulletCollision
      :: (Deque Bullet, Deque Asteroid, Int, AsteroidSize)
      -> Bullet
      -> (Deque Bullet, Deque Asteroid, Int, AsteroidSize)
    handleBulletCollision (bs, as, colNum, siz) bullet =
      let (b, updatedAs, colNum', siz') = go (bullet, [], 0, siz) bullet (Exts.toList as)
      in (D.snoc b bs, Exts.fromList updatedAs, colNum + colNum', siz')

    -- | The state of this recursion is @(Final state of the Input
    -- Bullet, List of Asteroids that may have dead flag set)@
    go :: (Bullet, [Asteroid], Int, AsteroidSize) -> Bullet -> [Asteroid] -> (Bullet, [Asteroid], Int, AsteroidSize)
    go (b, accAs, colNum, siz) _ [] = (b, accAs, colNum, siz)
    go (accBullet, accAsteroids, colNum, siz) b (a:as)
      | isColliding a b =
        (b { bulletIsDead = True }, (a { asteroidIsDead = True } : accAsteroids) ++ as, 1, getNextSizeUp a siz)
      | otherwise = go (accBullet, a : accAsteroids, colNum, siz) b as

    isColliding :: Asteroid -> Bullet -> Bool
    isColliding a b
      | asteroidIsDead a || bulletIsDead b = False
      | otherwise =
        let aBox = getCollisionBox a
            bBox = getCollisionBox b
        in isCollidingBox aBox bBox

    getNextSizeUp :: Asteroid -> AsteroidSize -> AsteroidSize
    getNextSizeUp _ Big = Big
    getNextSizeUp Asteroid {..} Small
      | asteroidSize == Big = Big
      | otherwise = Small

checkShipCollision :: Ship -> Deque Asteroid -> Maybe CollisionResult
checkShipCollision ship asteroids
  | any (isShipColliding ship) $ Exts.toList asteroids = Just ShipCollision
  | otherwise                                          = Nothing
  where
    isShipColliding :: Ship -> Asteroid -> Bool
    isShipColliding shp ast = isCollidingBox (getCollisionBox shp) (getCollisionBox ast)

filterDeadBullets :: Deque Bullet -> Deque Bullet
filterDeadBullets = D.filter isAlive
  where
    isAlive Bullet {..} = not bulletIsDead

filterDeadAsteroids :: Float -> Deque Asteroid -> Deque Asteroid
filterDeadAsteroids spawnedAsteroidVelocityScale = foldl handleAsteroid mempty
  where
    isAlive Asteroid {..} = not asteroidIsDead
    handleAsteroid :: Deque Asteroid -> Asteroid -> Deque Asteroid
    handleAsteroid newAsteroids asteroid =
      case asteroidSize asteroid of
        Small | isAlive asteroid -> asteroid `D.snoc` newAsteroids
              | otherwise -> newAsteroids
        Big | isAlive asteroid -> asteroid `D.snoc` newAsteroids
            | otherwise ->
              let tlV = topLeftV ^* spawnedAsteroidVelocityScale
                  brV = bottomRightV ^* spawnedAsteroidVelocityScale
                  trV = topRightV ^* spawnedAsteroidVelocityScale
                  blV = bottomLeftV ^* spawnedAsteroidVelocityScale
              in D.snoc (spawnAsteroid Small ((+ (-30)) <$> asteroidPosition asteroid) tlV (asteroidRotationSpeed asteroid))
                 . D.snoc (spawnAsteroid Small ((+ 30) <$> asteroidPosition asteroid) brV (asteroidRotationSpeed asteroid))
                 . D.snoc (spawnAsteroid Small ((\(V2 x y) -> V2 (x + 30) (y - 30)) $ asteroidPosition asteroid) trV (asteroidRotationSpeed asteroid))
                 . D.snoc (spawnAsteroid Small ((\(V2 x y) -> V2 (x - 30) (y + 30)) $ asteroidPosition asteroid) blV (asteroidRotationSpeed asteroid))
                 $ newAsteroids

render :: GameState -> IO ()
render GameState {..} = do
  SDL.rendererDrawColor gameStateRenderer $= V4 0 0 0 255
  SDL.clear gameStateRenderer

  renderPlayerShip
    gameStatePlayerShip
    gameStateRenderer
  renderBullets gameStateBullets gameStateRenderer
  renderAsteroids gameStateAsteroids gameStateRenderer

  renderText (T.pack . show $ gameStateScore) (V2 10 10) gameStateFont32 gameStateRenderer

  SDL.present gameStateRenderer

gameOverScene :: Scene
gameOverScene
  = Scene
  { sceneUpdate = gameOverUpdate
  , sceneRender = gameOverRender
  }

gameOverUpdate :: GameState -> GameState
gameOverUpdate gameState
  | buttonStateFirePressed $ gameStateButtonState gameState = startNewGame gameState
  | otherwise = gameState
  where
    startNewGame state@GameState{..}
      = state
      { gameStateStatus              = Main
      , gameStateScene               = mainScene
      , gameStateAsteroids           = initAsteroids
      , gameStateBullets             = mempty
      , gameStateBulletTimer         = 0
      , gameStateScore               = 0
      , gameStatePlayerShip          = initPlayerShip
      , gameStateAsteroidSpawnTimer  = initTimer gameStateCurrentTime
      , gameStateAsteroidSpawnDelta  = initAsteroidSpawnDelta
      , gameStateAsteroidSpawnFactor = initAsteroidSpawnFactor
      , gameStateButtonState         = initButtonState
      }

gameOverRender :: GameState -> IO ()
gameOverRender GameState {..} = do
  SDL.rendererDrawColor gameStateRenderer $= V4 0 0 0 255
  SDL.clear gameStateRenderer
  renderText (T.pack $ "Final score: " ++ show gameStateScore) (V2 200 140) gameStateFont32 gameStateRenderer
  renderText (T.pack "GAME OVER") (V2 200 200) gameStateFont32 gameStateRenderer
  renderText (T.pack "Press Fire to start a new game.") (V2 200 260) gameStateFont32 gameStateRenderer
  SDL.present gameStateRenderer

mainMenuScene :: Scene
mainMenuScene
  = Scene
  { sceneUpdate = mainMenuUpdate
  , sceneRender = mainMenuRender
  }

mainMenuUpdate :: GameState -> GameState
mainMenuUpdate gameState
  | buttonStateDownPressed $ gameStateButtonState gameState =
    let mainMenu = gameStateMainMenu gameState
    in gameState { gameStateMainMenu = menuDown mainMenu }
  | buttonStateUpPressed $ gameStateButtonState gameState =
    let mainMenu = gameStateMainMenu gameState
    in gameState { gameStateMainMenu = menuUp mainMenu }
  | buttonStateFirePressed $ gameStateButtonState gameState =
    case menuSelect $ gameStateMainMenu gameState of
      Start -> gameState { gameStateScene = mainScene }
      Quit  -> gameState { gameStateStatus = ShouldQuit }
  | otherwise = gameState

mainMenuRender :: GameState -> IO ()
mainMenuRender gs@GameState {..} = do
  SDL.rendererDrawColor gameStateRenderer $= V4 0 0 0 255
  SDL.clear gameStateRenderer
  renderText "HASKEROIDS" (V2 200 200) gameStateFont32 gameStateRenderer
  renderMenu gs (V2 200 280)
  SDL.present gameStateRenderer

renderMenu :: GameState -> V2 Int -> IO ()
renderMenu GameState {..} (V2 px py) = do
  let MainMenuState {..} = gameStateMainMenu
  forM_ (zip [0..] mainMenuChoices) $ uncurry (renderMenuItem px py mainMenuCursorPosition)
  where
    renderMenuItem :: Int -> Int -> Int -> Int -> MenuChoice -> IO ()
    renderMenuItem x y selected i choice = do
      let menuItemY = y + (i * 28)
      when (selected == i) $ do
        SDL.rendererDrawColor gameStateRenderer $= V4 255 255 255 255
        let r = SDL.Rectangle (SDL.P (fromIntegral <$> V2 (x - 20) (menuItemY + 8))) (V2 5 5)
        SDL.fillRect gameStateRenderer (Just r)
      renderText (T.pack $ show choice) (V2 x menuItemY) gameStateFont16 gameStateRenderer

renderText :: Text -> V2 Int -> Font.Font -> SDL.Renderer -> IO ()
renderText txt position fnt renderer = do
  textSurface <- Font.shaded fnt (V4 255 255 255 255) (V4 0 0 0 255) txt
  textDims <- SDL.surfaceDimensions textSurface
  textTex <- SDL.createTextureFromSurface renderer textSurface
  SDL.copy renderer textTex Nothing (Just $ SDL.Rectangle (SDL.P $ fromIntegral <$> position) textDims)
  SDL.destroyTexture textTex
  SDL.freeSurface textSurface

renderPlayerShip :: Ship -> SDL.Renderer -> IO ()
renderPlayerShip Ship {..} renderer = do
  SDL.rendererDrawColor renderer $= V4 0 255 0 255
  let shipPoints
        = VS.map (rotateOrigin (truncate <$> shipPosition) shipRotation)
        . playerShipPoints (truncate <$> shipPosition) $ shipSize
  SDL.drawLines renderer shipPoints

playerShipPoints :: V2 CInt -> CInt -> VS.Vector (SDL.Point V2 CInt)
playerShipPoints position size
  = VS.fromList [ SDL.P $ position + V2 size 0
                , SDL.P $ position + V2 (-size) ((-size) `div` 2)
                , SDL.P $ position + V2 (-size) (size `div` 2)
                , SDL.P $ position + V2 size 0
                ]

renderBullets :: Deque Bullet -> SDL.Renderer -> IO ()
renderBullets bullets renderer = do
  SDL.rendererDrawColor renderer $= V4 255 0 0 255
  forM_ bullets $ \Bullet {..} -> do
    let br = SDL.Rectangle (SDL.P (truncate <$> bulletPosition)) (V2 10 10)
    SDL.fillRect renderer (Just br)

rotateOrigin :: V2 CInt -> Float -> SDL.Point V2 CInt -> SDL.Point V2 CInt
rotateOrigin origin@(V2 originX originY) rotation (SDL.P inputP) =
  let s = sin rotation
      c = cos rotation
      (SDL.P (V2 pointX pointY)) = SDL.P $ inputP - origin
      x = fromIntegral pointX * c - fromIntegral pointY * s
      y = fromIntegral pointX * s + fromIntegral pointY * c
  in SDL.P (V2 (round x + originX) (round y + originY))

isKeyboardKey :: SDL.Event -> SDL.InputMotion -> SDL.Keycode -> Bool
isKeyboardKey event keyState keyCode =
  case SDL.eventPayload event of
    SDL.KeyboardEvent keyboardEvent ->
      SDL.keyboardEventKeyMotion keyboardEvent == keyState &&
      SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == keyCode &&
      not (SDL.keyboardEventRepeat keyboardEvent)
    _ -> False

-- | Wrap an object position around the edges of the screen. `size` is
-- the boundary over the edge of the screen.
wrapTorus :: V2 Float -> Int -> V2 Float
wrapTorus (V2 px py) size = V2 (wrapX px size) (wrapY py size)
  where
    wrapX :: Float -> Int -> Float
    wrapX x s
      | (x - fromIntegral s) > 800 = fromIntegral (-s)
      | (x + fromIntegral s) < 0 = 800 + fromIntegral s
      | otherwise = x
    wrapY :: Float -> Int -> Float
    wrapY y s
      | (y - fromIntegral s) > 600 = fromIntegral (-s)
      | (y + fromIntegral s) < 0 = 600 + fromIntegral s
      | otherwise = y
