{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asteroids.Game where

import Control.Monad (unless)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Foreign.C.Types
import Linear
import qualified SDL
import SDL (($=), WindowConfig (..))

windowConfig :: WindowConfig
windowConfig
  = SDL.defaultWindow
  { windowHighDPI = True
  , windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL
  , windowPosition = SDL.Centered
  }

delta :: Double
delta = 0.0

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
  }
  deriving (Eq, Show)

initButtonState :: ButtonState
initButtonState
  = ButtonState
  { buttonStateUp    = KeyRelease
  , buttonStateDown  = KeyRelease
  , buttonStateLeft  = KeyRelease
  , buttonStateRight = KeyRelease
  }

data GameState
  = GameState
  { gameStateRenderer              :: SDL.Renderer
  , gameStateTicks                 :: Double
  , gameStateCurrentTime           :: Double
  , gameStateAccumulator           :: Double
  , gameStatePlayerPosition        :: V2 CInt
  , gameStatePlayerSize            :: CInt
  , gameStatePlayerRotation        :: Float
  , gameStatePlayerRotationSpeed   :: Float
  , gameStatePlayerThrust          :: Float
  , gameStatePlayerThrustSpeed     :: Float
  , gameStatePlayerThrustMax       :: Float
  , gameStatePlayerAcceleration    :: Float
  , gameStatePlayerMaxAcceleration :: Float
  , gameStateButtonState           :: ButtonState
  }
  deriving (Eq, Show)

initGameState :: SDL.Renderer -> IO GameState
initGameState renderer = do
  currentTime <- SDL.time
  pure
    $ GameState
    { gameStateRenderer              = renderer
    , gameStateTicks                 = 0.0
    , gameStateCurrentTime           = currentTime
    , gameStateAccumulator           = 0.0
    , gameStatePlayerPosition        = V2 20 20
    , gameStatePlayerSize            = 20
    , gameStatePlayerRotation        = 0.0
    , gameStatePlayerRotationSpeed   = 0.1
    , gameStatePlayerThrust          = 0.0
    , gameStatePlayerThrustSpeed     = 0.2
    , gameStatePlayerThrustMax       = 3.0
    , gameStatePlayerAcceleration    = 0.0
    , gameStatePlayerMaxAcceleration = 3.0
    , gameStateButtonState           = initButtonState
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
      qPressed = any eventIsQPress events
      aPressed = any eventIsAPress events
      aUp = any eventIsARelease events
      dPressed = any eventIsDPress events
      dUp = any eventIsDRelease events
      wPressed = any eventIsWPress events
      wUp = any eventIsWRelease events
      sPressed = any eventIsSPress events
      sUp = any eventIsSRelease events
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
      buttonState'
        = gameStateButtonState
        { buttonStateUp = upButtonState
        -- TODO (james): give this button states too
        , buttonStateDown = downButtonState
        , buttonStateLeft = leftButtonState
        , buttonStateRight = rightButtonState
        }

  newTime <- SDL.time
  let frameTime = newTime - gameStateCurrentTime
      state' = update state { gameStateCurrentTime = newTime
                            , gameStateAccumulator = gameStateAccumulator + frameTime
                            , gameStateButtonState = buttonState'
                            }
  render state'
  unless qPressed $ loop state'

update :: GameState -> GameState
update state@GameState {..} =
  let turnAmount
        | keyDown $ buttonStateLeft gameStateButtonState = -gameStatePlayerRotationSpeed
        | keyDown $ buttonStateRight gameStateButtonState = gameStatePlayerRotationSpeed
        | otherwise = 0.0
      thrust
        | keyDown $ buttonStateUp gameStateButtonState =
          clamp
          (gameStatePlayerThrust + gameStatePlayerThrustSpeed)
          gameStatePlayerThrustMax
        | otherwise = 0.0
      nextState = state { gameStateAccumulator = gameStateAccumulator - delta
                        , gameStateTicks = gameStateTicks + delta
                        , gameStatePlayerRotation = gameStatePlayerRotation + turnAmount
                        , gameStatePlayerThrust = thrust
                        }
  in
    if gameStateAccumulator >= delta
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
  SDL.present gameStateRenderer

renderPlayerShip :: V2 CInt -> CInt -> Float -> SDL.Renderer -> IO ()
renderPlayerShip position size rotation renderer = do
  SDL.rendererDrawColor renderer $= V4 0 255 0 255
  let shipPoints = V.map (rotateOrigin position rotation) . playerShipPoints position $ size
  SDL.drawLines renderer shipPoints

playerShipPoints :: V2 CInt -> CInt -> Vector (SDL.Point V2 CInt)
playerShipPoints position size
  = V.fromList [ SDL.P $ position + V2 0 (-size)
               , SDL.P $ position + V2 (size `div` 2) size
               , SDL.P $ position + V2 (-(size `div` 2)) size
               , SDL.P $ position + V2 0 (-size)
               ]

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

clamp :: Ord a => a -> a -> a
clamp clampMax value
  | value > clampMax = clampMax
  | otherwise        = value
