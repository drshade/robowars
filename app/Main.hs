{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

{-
Improvement ideas:
- Robot scripts could be arrows, and executing them could be done in parallel
  rather than sequentially (game itself could be arrows too)
- Clean up the Types to be nicer (e.g. Position = Position *= Acceleration *^ Time)
-}

module Main where

import Control.Monad (join)
import Data.Coerce (coerce)
import Game
import Input (getInteractiveInputs)
import Raylib.Core (beginMode2D, clearBackground, closeWindow, endMode2D, getFrameTime, getMonitorRefreshRate, getTime, initWindow, setTargetFPS, windowShouldClose)
import Raylib.Core.Shapes (drawLine, drawRectanglePro)
import Raylib.Core.Text (drawFPS, drawText)
import Raylib.Types (Camera2D (..), Color, pattern Vector2)
import Raylib.Types.Core (Rectangle (..))
import Raylib.Util (WindowResources, drawing, raylibApplication)
import Raylib.Util.Colors (black, darkGreen, rayWhite)
import Script
import Types

screenWidth, screenHeight, old_targetFPS :: Int
screenWidth = 1000
screenHeight = 1000
old_targetFPS = 60

data GameState = GameState [Entity]

type RaylibState = (GameState, WindowResources)

initGameState :: GameState
initGameState =
    GameState
        [ Tank
            (Dynamics (RotationRate 0) (Acceleration 0))
            (MovingPlatform (Position 500 500) (Rotation 180) (Speed 0))
            (Dynamics (RotationRate 0) (Acceleration 0))
            (MountedPlatform (Rotation 0))
            tankLimits
            tankHumanInput
        , Tank
            (Dynamics (RotationRate 0) (Acceleration 0))
            (MovingPlatform (Position 250 250) (Rotation 180) (Speed 0))
            (Dynamics (RotationRate 0) (Acceleration 0))
            (MountedPlatform (Rotation 0))
            tankLimits
            test1'scriptdef
        , Projectile
            (Dynamics (RotationRate 0) (Acceleration 100))
            (MovingPlatform (Position 100 100) (Rotation 180) (Speed 0))
            projectileLimits
        ]

startup :: IO RaylibState
startup = do
    window <- initWindow screenWidth screenHeight "robowars"
    refresh <- getMonitorRefreshRate 0
    setTargetFPS refresh
    pure (initGameState, window)

mainLoop :: RaylibState -> IO RaylibState
mainLoop (GameState entities, window) = do
    -- Run the sim
    --
    totalTime <- getTime
    deltaTime <- getFrameTime
    interactiveInputs <- getInteractiveInputs

    -- Execute scripts for all entities which have them
    --
    entities' :: [Entity] <-
        join
            <$> mapM
                ( \entity -> do
                    (newState, instructions) <- case entity of
                        Tank _ _ _ _ _ scriptRunner ->
                            pure $ runScript scriptRunner Nothing totalTime deltaTime interactiveInputs
                        _ -> pure (Nothing, [])

                    -- Tick the game state
                    --
                    pure $ tick totalTime deltaTime entity instructions
                )
                entities

    drawing
        ( do
            beginMode2D camera
            clearBackground rayWhite
            drawFPS 10 screenHeight
            drawText "robowars" 30 40 18 black
            drawText ("input: " <> show interactiveInputs) 0 (screenHeight + 18) 18 black
            drawText ("entities: " <> (show $ length entities')) 0 (screenHeight + 36) 18 black

            -- Draw everything
            --
            mapM_ drawEntity $ entities'

            drawBoundary
            endMode2D
        )
        >> pure (GameState entities', window)
  where
    camera =
        Camera2D
            { camera2D'offset = Vector2 (fromIntegral screenWidth / 2) (fromIntegral screenHeight / 2)
            , camera2D'target = Vector2 500 500
            , camera2D'rotation = 0
            , camera2D'zoom = 0.9
            }

drawBoundary :: IO ()
drawBoundary = do
    drawLine 0 0 0 1000 black
    drawLine 0 1000 1000 1000 black
    drawLine 1000 1000 1000 0 black
    drawLine 1000 0 0 0 black

drawBox :: Position -> Float -> Float -> Rotation -> Color -> IO ()
drawBox (Position posx posy) width height (Rotation rotation) color =
    drawRectanglePro (frame posx posy) origin rotation color
  where
    frame x y = Rectangle x y width height
    origin = Vector2 ((width / 2)) ((height / 2))

drawBoxOffset :: Position -> Float -> Float -> Rotation -> Color -> Position -> IO ()
drawBoxOffset (Position posx posy) width height (Rotation rotation) color (Position offx offy) =
    drawRectanglePro (frame posx posy) origin rotation color
  where
    frame x y = Rectangle x y width height
    origin = Vector2 (offx + (width / 2)) (offy + (height / 2))

drawTelemetry :: Entity -> IO ()
drawTelemetry (Tank dynamics (MovingPlatform platform'position platform'rotation platform'speed) turretDynamics (MountedPlatform turret'rotation) _ _) =
    let (Position x y) = platform'position
     in do
            drawText (show platform'speed <> "/" <> (show $ acceleration dynamics)) (round x + 12) (round y - 15) 18 black
            drawText (show platform'rotation <> "/" <> (show $ rotationRate dynamics)) (round x + 12) (round y - 0) 18 black
            drawText ("Turret " <> show turret'rotation <> "/" <> (show $ rotationRate turretDynamics)) (round x + 12) (round y + 15) 18 black
drawTelemetry _ = pure ()

drawEntity :: Entity -> IO ()
drawEntity (Projectile _ (MovingPlatform platform'position platform'rotation platform'speed) _) =
    drawBox platform'position 3 5 platform'rotation black
drawEntity (Mine _) = pure ()
drawEntity entity@(Tank _ (MovingPlatform platform'position platform'rotation platform'speed) _ (MountedPlatform turret'rotation) _ _) = do
    drawBox platform'position 20 30 platform'rotation darkGreen
    drawBoxOffset platform'position 5 25 (Rotation $ coerce platform'rotation + coerce turret'rotation) black (Position 0 10)
    drawTelemetry entity
drawEntity _ = pure ()

shouldClose :: RaylibState -> IO Bool
shouldClose _ = windowShouldClose

teardown :: RaylibState -> IO ()
teardown = closeWindow . Just . snd

raylibApplication 'startup 'mainLoop 'shouldClose 'teardown