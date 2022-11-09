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

data GameState
  = GameState
  { gameStateRenderer       :: SDL.Renderer
  , gameStateTicks          :: Double
  , gameStateCurrentTime    :: Double
  , gameStateAccumulator    :: Double
  , gameStatePlayerPosition :: V2 CInt
  , gameStatePlayerSize     :: CInt
  , gameStatePlayerRotation :: Float
  }
  deriving (Eq, Show)

initGameState :: SDL.Renderer -> IO GameState
initGameState renderer
  = GameState
  <$> pure renderer
  <*> pure 0.0
  <*> SDL.time
  <*> pure 0.0
  <*> pure (V2 20 20)
  <*> pure 10
  <*> pure 0.0

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
  let eventIsQPress event =
        case SDL.eventPayload event of
          SDL.KeyboardEvent keyboardEvent ->
            SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
            SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events

  newTime <- SDL.time
  let frameTime = newTime - gameStateCurrentTime
      state' = update state { gameStateCurrentTime = newTime
                            , gameStateAccumulator = gameStateAccumulator + frameTime
                            }
  render state'
  unless qPressed $ loop state'

update :: GameState -> GameState
update state@GameState {..} =
  let nextState = state { gameStateAccumulator = gameStateAccumulator - delta
                        , gameStateTicks = gameStateTicks + delta
                        , gameStatePlayerRotation = gameStatePlayerRotation + 0.01
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
