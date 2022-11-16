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

data ButtonState
  = ButtonState
  { buttonStateUp    :: Bool
  , buttonStateDown  :: Bool
  , buttonStateLeft  :: Bool
  , buttonStateRight :: Bool
  }
  deriving (Eq, Show)

initButtonState :: ButtonState
initButtonState = ButtonState False False False False

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
      eventIsAUp event = isKeyboardKey event SDL.Released SDL.KeycodeA
      eventIsDPress event = isKeyboardKey event SDL.Pressed SDL.KeycodeD
      eventIsDUp event = isKeyboardKey event SDL.Released SDL.KeycodeD
      eventIsWPress event = isKeyboardKey event SDL.Pressed SDL.KeycodeW
      eventIsWUp event = isKeyboardKey event SDL.Released SDL.KeycodeW
      eventIsSPress event = isKeyboardKey event SDL.Pressed SDL.KeycodeS
      eventIsSUp event = isKeyboardKey event SDL.Released SDL.KeycodeS
      qPressed = any eventIsQPress events
      aPressed = any eventIsAPress events
      aUp = any eventIsAUp events
      dPressed = any eventIsDPress events
      dUp = any eventIsDUp events
      wPressed = any eventIsWPress events
      wUp = any eventIsWUp events
      sPressed = any eventIsSPress events
      sUp = any eventIsSUp events
      upButtonState
        | wPressed && not (buttonStateUp gameStateButtonState) = True
        | wUp && buttonStateUp gameStateButtonState = False
        | otherwise = buttonStateUp gameStateButtonState
      leftButtonState
        | aPressed && not (buttonStateLeft gameStateButtonState) = True
        | aUp && buttonStateLeft gameStateButtonState = False
        | otherwise = buttonStateLeft gameStateButtonState
      rightButtonState
        | dPressed && not (buttonStateRight gameStateButtonState) = True
        | dUp && buttonStateRight gameStateButtonState = False
        | otherwise = buttonStateRight gameStateButtonState
      downButtonState
        | sPressed && not (buttonStateDown gameStateButtonState) = True
        | sUp && buttonStateDown gameStateButtonState = False
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
        | buttonStateLeft gameStateButtonState = -gameStatePlayerRotationSpeed
        | buttonStateRight gameStateButtonState = gameStatePlayerRotationSpeed
        | otherwise = 0.0
      thrust
        | buttonStateUp gameStateButtonState =
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
