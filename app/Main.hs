{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import Data.Coerce (coerce)
import Game
import Input (MovementInput (..), getMovementInputs)
import Raylib.Core (beginMode2D, clearBackground, closeWindow, endMode2D, getFrameTime, getKeyPressed, getMonitorRefreshRate, getTime, initWindow, isKeyDown, setTargetFPS, windowShouldClose)
import Raylib.Core.Shapes (drawLine, drawRectangleLinesEx, drawRectanglePro, drawRectangleV)
import Raylib.Core.Text (drawFPS, drawText)
import Raylib.Types (Camera2D (..), Color, KeyboardKey, Vector2, vector2'x, pattern Vector2)
import Raylib.Types.Core (Rectangle (..))
import Raylib.Util (WindowResources, drawing, raylibApplication)
import Raylib.Util.Colors (black, darkGreen, lightGray, rayWhite)

screenWidth, screenHeight, old_targetFPS :: Int
screenWidth = 1000
screenHeight = 1000
old_targetFPS = 60

data OldTank = OldTank
    { position :: Vector2
    , vehicleBearing :: Float
    , turretBearing :: Float -- Relative to vehicle
    }

data GameState = GameState [Entity]

type RaylibState = (GameState, WindowResources)

-- Doesn't handle more than 1 input at a time...
tankHumanInput :: Script
tankHumanInput totalTime deltaTime movementInput
    | MoveForward `elem` movementInput = Throttle $ throttleFactor * deltaTime
    | MoveBackward `elem` movementInput = Throttle $ -throttleFactor * deltaTime
    | MoveLeft `elem` movementInput = Steer $ -steerFactor * deltaTime
    | MoveRight `elem` movementInput = Steer $ steerFactor * deltaTime
    | otherwise = DoNothing
  where
    throttleFactor = 15000
    steerFactor = 3000

tankAutoScript :: Script
tankAutoScript totalTime deltaTime movementInput
    | totalTime > 1 && totalTime < 2 = Throttle 100
    | totalTime > 4 && totalTime < 6 = Steer 100
    | totalTime > 8 && totalTime < 10 = Throttle (-100)
    | totalTime > 12 && totalTime < 14 = Aim (100)
    | totalTime > 16 && totalTime < 30 = Fire
    | otherwise = DoNothing

initGameState =
    GameState
        [ Tank
            ((def :: MovingPlatform){position = Position 500 500, rotation = Rotation 180, acceleration = Acceleration 0, speed = Speed 0, rotationRate = RotationRate (-0)})
            ((def :: StandingPlatform){rotationRate = RotationRate (-0)})
            tankLimits
            tankHumanInput
        , Projectile (def{position = Position 100 100, rotation = Rotation 180, acceleration = Acceleration 100, speed = Speed 0}) def
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
    delta <- getFrameTime
    movementInputs <- getMovementInputs
    let entities' = tick totalTime delta movementInputs <$> entities
    drawing
        ( do
            beginMode2D camera
            clearBackground rayWhite
            drawFPS 10 screenHeight
            drawText ("input: " <> show movementInputs) 0 (screenHeight + 18) 18 black
            drawText "robowars" 30 40 18 black

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
    drawRectanglePro (frame posx posy) (origin offx offy) rotation color
  where
    frame x y = Rectangle x y width height
    origin offx offy = Vector2 (offx + (width / 2)) (offy + (height / 2))

drawTelemetry :: Entity -> IO ()
drawTelemetry (Tank platform _ _ _) =
    let (Position x y) = platform.position
     in do
            drawText ((take 100 $ show platform.speed) <> "/" <> (take 100 $ show platform.acceleration)) (round x + 12) (round y - 15) 18 black
            drawText (show platform.acceleration) (round x + 12) (round y - 0) 18 black
            drawText (show platform.rotationRate) (round x + 12) (round y + 15) 18 black
drawTelemetry _ = pure ()

drawEntity :: Entity -> IO ()
drawEntity (Projectile platform _) =
    drawBox platform.position 3 5 platform.rotation black
drawEntity (Mine _) = pure ()
drawEntity entity@(Tank platform turret _ _) = do
    drawBox platform.position 20 30 platform.rotation darkGreen
    drawBoxOffset platform.position 5 25 (Rotation $ coerce platform.rotation + coerce turret.rotation) black (Position 0 10)
    drawTelemetry entity

shouldClose :: RaylibState -> IO Bool
shouldClose _ = windowShouldClose

teardown :: RaylibState -> IO ()
teardown = closeWindow . Just . snd

raylibApplication 'startup 'mainLoop 'shouldClose 'teardown