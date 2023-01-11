{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asteroids.Game where

import Control.Monad (forM_, unless)
import Deque.Strict (Deque)
import qualified Deque.Strict as D
import qualified Data.Vector.Storable as VS
import Foreign.C.Types
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
  }
  deriving (Eq, Show)

maxBullets :: Int
maxBullets = 15

data Bullet
  = Bullet
  { bulletPosition     :: V2 Float
  , bulletVelocity     :: V2 Float
  , bulletTick         :: Int
  }
  deriving (Eq, Show)

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
updateBulletPhysics = fmap updateBullet
  where
    updateBullet :: Bullet -> Bullet
    updateBullet b@Bullet {..} =
      let position = bulletPosition + bulletVelocity ^* delta
      in b { bulletPosition = wrapTorus position 10
           }

updateBulletAge :: GameState -> GameState
updateBulletAge state = state
  { gameStateBullets = D.takeWhile young . gameStateBullets $ state
  }
  where
    young :: Bullet -> Bool
    young Bullet {..} =
      (truncate . gameStateTicks $ state) - bulletTick < gameStateBulletAgeMax state

initGameState :: SDL.Renderer -> IO GameState
initGameState renderer = do
  currentTime <- SDL.time
  pseudoRandomFloats <- generatePseudoFloats 40
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
