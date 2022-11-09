{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asteroids.Game where

import Control.Monad (unless)
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
  }
  deriving (Eq, Show)

initGameState :: SDL.Renderer -> IO GameState
initGameState renderer
  = GameState
  <$> pure renderer
  <*> pure 0.0
  <*> SDL.time
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
  render gameStateRenderer
  unless qPressed $ loop state'

update :: GameState -> GameState
update state@GameState {..} =
  let nextState = state { gameStateAccumulator = gameStateAccumulator - delta
                        , gameStateTicks = gameStateTicks + delta
                        }
  in
    if gameStateAccumulator >= delta
    then state
    else update nextState

render :: SDL.Renderer -> IO ()
render renderer = do
  SDL.rendererDrawColor renderer $= V4 0 0 0 255
  SDL.clear renderer
  SDL.present renderer
