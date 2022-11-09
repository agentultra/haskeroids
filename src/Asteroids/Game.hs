{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asteroids.Game where

import Control.Monad (unless)
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
  { gameStateRenderer    :: SDL.Renderer
  , gameStateTicks       :: Double
  , gameStateCurrentTime :: Double
  , gameStateAccumulator :: Double
  , gameStatePlayerShip  :: V2 CInt
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
                        , gameStatePlayerShip = gameStatePlayerShip + V2 1 0
                        }
  in
    if gameStateAccumulator >= delta
    then nextState
    else update nextState

render :: GameState -> IO ()
render GameState {..} = do
  SDL.rendererDrawColor gameStateRenderer $= V4 0 0 0 255
  SDL.clear gameStateRenderer

  let shipRect = SDL.Rectangle (SDL.P gameStatePlayerShip) (V2 20 20)
  SDL.rendererDrawColor gameStateRenderer $= V4 255 0 0 255
  SDL.fillRect gameStateRenderer (Just shipRect)
  SDL.present gameStateRenderer
