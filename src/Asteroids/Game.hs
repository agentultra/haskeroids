{-# LANGUAGE DataKinds#-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asteroids.Game where

import Control.Monad (unless)
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text as T
import Deque.Strict (Deque)
import qualified Deque.Strict as D
import qualified Data.Vector.Storable as VS
import Foreign.C.Types
import qualified GHC.Exts as Exts
import Linear
import qualified SDL
import qualified SDL.Raw.Video as Video
import SDL (($=), WindowConfig (..))
import qualified SDL.Font as Font

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
  { buttonStateUp    :: KeyState
  , buttonStateDown  :: KeyState
  , buttonStateLeft  :: KeyState
  , buttonStateRight :: KeyState
  , buttonStateFire  :: KeyState
  }
  deriving (Eq, Show)

initButtonState :: ButtonState
initButtonState
  = ButtonState
  { buttonStateUp    = KeyRelease
  , buttonStateDown  = KeyRelease
  , buttonStateLeft  = KeyRelease
  , buttonStateRight = KeyRelease
  , buttonStateFire  = KeyRelease
  }

data GameState
  = GameState
  { gameStateRenderer       :: SDL.Renderer
  , gameStateFps            :: Float
  , gameStateTicks          :: Float
  , gameStateCurrentTime    :: Float
  , gameStateAccumulator    :: Float
  , gameStateButtonState    :: ButtonState
  , gameStatePlayerShip     :: Ship
  , gameStateBullets        :: Deque Bullet
  , gameStateBulletTimer    :: Float
  , gameStateBulletTimerMax :: Float
  , gameStateBulletAgeMax   :: Int
  , gameStateAsteroids      :: Deque Asteroid
  , gameStateScore          :: Int
  , gameStateFont           :: Font.Font
  }
  deriving (Eq, Show)

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

class HasPosition a where
  getPosition :: a -> V2 Float
  setPosition :: a -> V2 Float -> a

class HasVelocity a where
  getVelocity :: a -> V2 Float
  setVelocity :: a -> V2 Float -> a

class HasCollisionBox a where
  getCollisionBox :: a -> CollisionBox

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

maxBullets :: Int
maxBullets = 15

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

initGameState :: SDL.Renderer -> Font.Font -> IO GameState
initGameState renderer font = do
  currentTime <- SDL.time
  let initPlayerShip = Ship
        { shipPosition      = V2 20 20
        , shipSize          = 20
        , shipRotation      = 0.0
        , shipRotationSpeed = 0.1
        , shipThrust        = 0.0
        , shipThrustSpeed   = 0.2
        , shipThrustMax     = 3.0
        , shipAcceleration  = V2 0.0 0.0
        , shipMaxVelocity   = 49.0
        , shipVelocity = V2 0.0 0.0
        }
  let smallAsteroid = spawnAsteroid Small (V2 200 200) (V2 1.2 1.2) 0.2
      largeAsteroid = spawnAsteroid Big (V2 400 100) (V2 1.2 1.2) 0.2
  pure
    $ GameState
    { gameStateRenderer       = renderer
    , gameStateFps            = 0.0
    , gameStateTicks          = 0.0
    , gameStateCurrentTime    = currentTime
    , gameStateAccumulator    = 0.0
    , gameStatePlayerShip     = initPlayerShip
    , gameStateButtonState    = initButtonState
    , gameStateBullets        = mempty
    , gameStateBulletTimer    = 0.0
    , gameStateBulletTimerMax = 1.0
    , gameStateBulletAgeMax   = 50
    , gameStateAsteroids      = Exts.fromList [smallAsteroid, largeAsteroid]
    , gameStateScore          = 0
    , gameStateFont           = font
    }

run :: IO ()
run = do
  SDL.initializeAll

  window <- SDL.createWindow "Asteroids" windowConfig
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  Font.initialize
  fNoticia <- Font.load "./assets/fonts/NoticiaText-Bold.ttf" 32
  state <- initGameState renderer fNoticia

  loop state

  Font.quit
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
        -- TODO (james): give this button states too
        , buttonStateDown = downButtonState
        , buttonStateLeft = leftButtonState
        , buttonStateRight = rightButtonState
        , buttonStateFire = fireButtonState
        }

  newTime <- SDL.time
  let frameTime = newTime - gameStateCurrentTime
      fps = 1 / frameTime
      state' = update state { gameStateCurrentTime = newTime
                            , gameStateAccumulator = gameStateAccumulator + frameTime
                            , gameStateButtonState = buttonState'
                            , gameStateFps         = fps
                            }
  render state'
  let state'' = updateBulletAge state'
  unless qPressed $ do
    loop state'' { gameStatePlayerShip = (getShip state'') { shipAcceleration = V2 0.0 0.0 } }
  where
    getShip :: GameState -> Ship
    getShip GameState {..} = gameStatePlayerShip

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
      bullets = if keyDown (buttonStateFire gameStateButtonState) && (bulletTimer == 0)
        then fireBullet
             (shipPosition gameStatePlayerShip)
             (shipRotation gameStatePlayerShip)
             (truncate gameStateTicks)
             gameStateBullets
        else gameStateBullets
      collisions = checkAsteroidCollisions (updateAsteroids gameStateAsteroids) (updateBulletPhysics bullets)
      state' = handleCollisionResults state [collisions]
      ship = updatePosition gameStatePlayerShip delta
      nextState = state' { gameStateAccumulator = gameStateAccumulator - delta
                         , gameStateTicks = gameStateTicks + delta
                         , gameStatePlayerShip = ship
                           { shipRotation = shipRotation gameStatePlayerShip + turnAmount
                           , shipThrust = thrust
                           , shipAcceleration = acceleration
                           , shipVelocity = AV.clampVector velocity $ shipMaxVelocity gameStatePlayerShip
                          }
                         , gameStateBulletTimer = bulletTimer
                         }
  in
    if gameStateAccumulator <= delta
    then nextState
    else update nextState

data CollisionResult
  = BulletAndAsteroidCollisions Int (Deque Bullet) (Deque Asteroid)
  deriving (Eq, Show)

handleCollisionResults :: GameState -> [CollisionResult] -> GameState
handleCollisionResults = foldl' handleCollision
  where
    handleCollision :: GameState -> CollisionResult -> GameState
    handleCollision gameState (BulletAndAsteroidCollisions numCols bs as) =
      gameState
       { gameStateBullets = filterDeadBullets bs
       , gameStateAsteroids = filterDeadAsteroids as
       , gameStateScore = gameStateScore gameState + numCols
       }

checkAsteroidCollisions :: Deque Asteroid -> Deque Bullet -> CollisionResult
checkAsteroidCollisions asteroids bullets =
  let (bs, as, colNum) = foldl' handleBulletCollision (mempty, asteroids, 0) bullets
  in BulletAndAsteroidCollisions colNum bs as
  where
    handleBulletCollision
      :: (Deque Bullet, Deque Asteroid, Int)
      -> Bullet
      -> (Deque Bullet, Deque Asteroid, Int)
    handleBulletCollision (bs, as, colNum) bullet =
      let (b, updatedAs, colNum') = go (bullet, [], 0) bullet (Exts.toList as)
      in (D.snoc b bs, Exts.fromList updatedAs, colNum + colNum')

    -- | The state of this recursion is @(Final state of the Input
    -- Bullet, List of Asteroids that may have dead flag set)@
    go :: (Bullet, [Asteroid], Int) -> Bullet -> [Asteroid] -> (Bullet, [Asteroid], Int)
    go (b, accAs, colNum) _ [] = (b, accAs, colNum)
    go (accBullet, accAsteroids, colNum) b (a:as)
      | isColliding a b =
        (b { bulletIsDead = True }, (a { asteroidIsDead = True } : accAsteroids) ++ as, 1)
      | otherwise = go (accBullet, a : accAsteroids, colNum) b as

    isColliding :: Asteroid -> Bullet -> Bool
    isColliding a b
      | asteroidIsDead a || bulletIsDead b = False
      | otherwise =
        let aBox = getCollisionBox a
            bBox = getCollisionBox b
        in isCollidingBox aBox bBox

filterDeadBullets :: Deque Bullet -> Deque Bullet
filterDeadBullets = D.filter isAlive
  where
    isAlive Bullet {..} = not bulletIsDead

filterDeadAsteroids :: Deque Asteroid -> Deque Asteroid
filterDeadAsteroids = foldl handleAsteroid mempty
  where
    isAlive Asteroid {..} = not asteroidIsDead
    handleAsteroid :: Deque Asteroid -> Asteroid -> Deque Asteroid
    handleAsteroid newAsteroids asteroid =
      case asteroidSize asteroid of
        Small | isAlive asteroid -> asteroid `D.snoc` newAsteroids
              | otherwise -> newAsteroids
        Big | isAlive asteroid -> asteroid `D.snoc` newAsteroids
            | otherwise ->
              D.snoc (spawnAsteroid Small ((+ (-30)) <$> asteroidPosition asteroid) (V2 (-0.8485288306602362) (-0.8485274441869115)) (asteroidRotationSpeed asteroid))
              . D.snoc (spawnAsteroid Small ((+ 30) <$> asteroidPosition asteroid) (V2 0.8485282760711782 0.8485279987765132) (asteroidRotationSpeed asteroid))
              . D.snoc (spawnAsteroid Small ((\(V2 x y) -> V2 (x + 30) (y - 30)) $ asteroidPosition asteroid) (V2 0.8485305610016184 (-0.8485257138391733)) (asteroidRotationSpeed asteroid))
              . D.snoc (spawnAsteroid Small ((\(V2 x y) -> V2 (x - 30) (y + 30)) $ asteroidPosition asteroid) (V2 (-0.8485243273607558) 0.8485319474698503) (asteroidRotationSpeed asteroid))
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

  renderText (T.pack . show $ gameStateScore) (V2 10 10) gameStateFont gameStateRenderer

  SDL.present gameStateRenderer

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
      SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == keyCode
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
