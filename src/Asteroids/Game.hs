{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asteroids.Game where

import Control.Monad (forM_, unless)
import Deque.Strict (Deque)
import qualified Deque.Strict as D
import qualified Data.Vector.Storable as VS
import Foreign.C.Types
import qualified GHC.Exts as Exts
import Linear
import qualified SDL
import SDL (($=), WindowConfig (..))

import qualified Asteroids.Linear.Vector as AV
import Asteroids.Random

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
  { gameStateRenderer            :: SDL.Renderer
  , gameStateFps                 :: Float
  , gameStateTicks               :: Float
  , gameStateCurrentTime         :: Float
  , gameStateAccumulator         :: Float
  , gameStatePlayerPosition      :: V2 Float
  , gameStatePlayerSize          :: CInt
  , gameStatePlayerRotation      :: Float
  , gameStatePlayerRotationSpeed :: Float
  , gameStatePlayerThrust        :: Float
  , gameStatePlayerThrustSpeed   :: Float
  , gameStatePlayerThrustMax     :: Float
  , gameStatePlayerAcceleration  :: V2 Float
  , gameStatePlayerMaxVelocity   :: Float
  , gameStatePlayerVelocity      :: V2 Float
  , gameStateButtonState         :: ButtonState
  , gameStateBullets             :: Deque Bullet
  , gameStateBulletTimer         :: Float
  , gameStateBulletTimerMax      :: Float
  , gameStateBulletAgeMax        :: Int
  , gameStateRandomValues        :: PseudoRandom Float
  , gameStateAsteroids           :: Deque Asteroid
  }
  deriving (Eq, Show)

class HasPosition a where
  getPosition :: a -> V2 Float
  setPosition :: a -> V2 Float -> a

class HasVelocity a where
  getVelocity :: a -> V2 Float
  setVelocity :: a -> V2 Float -> a

maxBullets :: Int
maxBullets = 15

data Bullet
  = Bullet
  { bulletPosition     :: V2 Float
  , bulletVelocity     :: V2 Float
  , bulletTick         :: Int
  }
  deriving (Eq, Show)

instance HasPosition Bullet where
  getPosition = bulletPosition
  setPosition bullet p = bullet { bulletPosition = p }

instance HasVelocity Bullet where
  getVelocity = bulletVelocity
  setVelocity bullet v = bullet { bulletVelocity = v }

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

data Asteroid
  = Asteroid
  { asteroidPoints        :: AsteroidPoints (V2 Float)
  , asteroidPosition      :: V2 Float
  , asteroidVelocity      :: V2 Float
  , asteroidRotation      :: Float
  , asteroidRotationSpeed :: Float
  }
  deriving (Eq, Show)

instance HasPosition Asteroid where
  getPosition = asteroidPosition
  setPosition asteroid p = asteroid { asteroidPosition = p }

instance HasVelocity Asteroid where
  getVelocity = asteroidVelocity
  setVelocity asteroid v = asteroid { asteroidVelocity = v }

updateAsteroid :: Asteroid -> Asteroid
updateAsteroid a@Asteroid {..} =
  let asteroid = updatePosition a delta
  in asteroid { asteroidRotation = asteroidRotation + asteroidRotationSpeed * delta }

updateAsteroids :: Deque Asteroid ->  Deque Asteroid
updateAsteroids = fmap updateAsteroid

renderAsteroid :: Asteroid -> SDL.Renderer -> IO ()
renderAsteroid Asteroid {..} renderer = do
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

initGameState :: SDL.Renderer -> IO GameState
initGameState renderer = do
  currentTime <- SDL.time
  pseudoRandomFloats <- generatePseudoFloats 40
  let apoints
        = AsteroidPoints
        ( V2 (-10) (-10)
        , V2 8 (-49)
        , V2 21 11
        , V2 (-10) 23
        , V2 (-10) 8
        , V2 (-20) 0
        )
      asteroid = Asteroid apoints (V2 200 200) (V2 1.2 1.2) 20 0.2
  pure
    $ GameState
    { gameStateRenderer            = renderer
    , gameStateFps                 = 0.0
    , gameStateTicks               = 0.0
    , gameStateCurrentTime         = currentTime
    , gameStateAccumulator         = 0.0
    , gameStatePlayerPosition      = V2 20 20
    , gameStatePlayerSize          = 20
    , gameStatePlayerRotation      = 0.0
    , gameStatePlayerRotationSpeed = 0.1
    , gameStatePlayerThrust        = 0.0
    , gameStatePlayerThrustSpeed   = 0.2
    , gameStatePlayerThrustMax     = 3.0
    , gameStatePlayerAcceleration  = V2 0.0 0.0
    , gameStatePlayerMaxVelocity   = 49.0
    , gameStateButtonState         = initButtonState
    , gameStatePlayerVelocity      = V2 0.0 0.0
    , gameStateBullets             = mempty
    , gameStateBulletTimer         = 0.0
    , gameStateBulletTimerMax      = 1.0
    , gameStateBulletAgeMax        = 50
    , gameStateRandomValues        = pseudoRandomFloats
    , gameStateAsteroids           = Exts.fromList [asteroid]
    }

run :: IO ()
run = do
  SDL.initializeAll

  window <- SDL.createWindow "Asteroids" windowConfig
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  state <- initGameState renderer

  loop state

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
  unless qPressed $ loop state'' { gameStatePlayerAcceleration = V2 0.0 0.0 }

update :: GameState -> GameState
update state@GameState {..} =
  let (V2 ax ay) = gameStatePlayerAcceleration
      turnAmount
        | keyDown $ buttonStateLeft gameStateButtonState = -gameStatePlayerRotationSpeed
        | keyDown $ buttonStateRight gameStateButtonState = gameStatePlayerRotationSpeed
        | otherwise = 0.0
      thrust
        | keyDown $ buttonStateUp gameStateButtonState =
          min
          (gameStatePlayerThrust + gameStatePlayerThrustSpeed)
          gameStatePlayerThrustMax
        | otherwise = 0.0
      acceleration
        | keyDown $ buttonStateUp gameStateButtonState
        = V2
          (ax + thrust * cos gameStatePlayerRotation)
          (ay + thrust * sin gameStatePlayerRotation)
        | otherwise = gameStatePlayerAcceleration
      position = gameStatePlayerPosition + gameStatePlayerVelocity ^* delta
      velocity = gameStatePlayerVelocity + acceleration ^* delta
      bulletTimer
        | keyDown (buttonStateFire gameStateButtonState) && (gameStateBulletTimer + delta < gameStateBulletTimerMax) = gameStateBulletTimer + delta
        | otherwise = 0.0
      bullets = if keyDown (buttonStateFire gameStateButtonState) && (bulletTimer == 0)
        then fireBullet
             gameStatePlayerPosition
             gameStatePlayerRotation
             (truncate gameStateTicks)
             gameStateBullets
        else gameStateBullets
      nextState = state { gameStateAccumulator = gameStateAccumulator - delta
                        , gameStateTicks = gameStateTicks + delta
                        , gameStatePlayerPosition = wrapTorus position (fromIntegral gameStatePlayerSize)
                        , gameStatePlayerRotation = gameStatePlayerRotation + turnAmount
                        , gameStatePlayerThrust = thrust
                        , gameStatePlayerAcceleration = acceleration
                        , gameStatePlayerVelocity = AV.clampVector velocity gameStatePlayerMaxVelocity
                        , gameStateBulletTimer = bulletTimer
                        , gameStateBullets = updateBulletPhysics bullets
                        , gameStateAsteroids = updateAsteroids gameStateAsteroids
                        }
  in
    if gameStateAccumulator <= delta
    then nextState
    else update nextState

render :: GameState -> IO ()
render GameState {..} = do
  SDL.rendererDrawColor gameStateRenderer $= V4 0 0 0 255
  SDL.clear gameStateRenderer

  renderPlayerShip
    gameStatePlayerPosition
    gameStatePlayerSize
    gameStatePlayerRotation
    gameStateRenderer
  renderBullets gameStateBullets gameStateRenderer
  renderAsteroids gameStateAsteroids gameStateRenderer
  SDL.present gameStateRenderer

renderPlayerShip :: V2 Float -> CInt -> Float -> SDL.Renderer -> IO ()
renderPlayerShip position size rotation renderer = do
  SDL.rendererDrawColor renderer $= V4 0 255 0 255
  let shipPoints
        = VS.map (rotateOrigin (truncate <$> position) rotation)
        . playerShipPoints (truncate <$> position) $ size
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
