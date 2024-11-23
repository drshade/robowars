{-# LANGUAGE DeriveGeneric #-}

module Script where

import Control.Monad.RWS (RWS, execRWS, get, modify, tell)
import Debug.Trace (trace)
import Input qualified (InteractiveInput (..))
import Types

stateScript :: ScriptExecutor
stateScript = ScriptExecutor $ (runScript script initState)
  where
    initState :: Int
    initState = 0
    runScript :: RWS ScriptInput ScriptOutput Int a -> Int -> ScriptInput -> (ScriptExecutor, ScriptOutput)
    runScript rws startingState input =
      let (state, output) = execRWS rws input startingState
       in (ScriptExecutor $ (runScript rws state), output)
    script :: RWS ScriptInput ScriptOutput Int ()
    script = do
      v <- get
      _ <- trace (show v) $ modify (+ 1)
      _ <- tell [Throttle 10]
      pure ()

-- Doesn't handle more than 1 input at a time...
interactiveScript :: ScriptExecutor
interactiveScript = ScriptExecutor $ script
  where
    script :: ScriptInput -> (ScriptExecutor, ScriptOutput)
    script (_totalTime, deltaTime, interactiveInput) =
      (interactiveScript, foldr go [] interactiveInput)
      where
        throttleFactor = 15000
        steerFactor = 5000
        aimFactor = 5000
        go Input.MoveForward = ((Throttle $ throttleFactor * deltaTime) :)
        go Input.MoveBackward = ((Throttle $ -throttleFactor * deltaTime) :)
        go Input.MoveLeft = ((Steer $ -steerFactor * deltaTime) :)
        go Input.MoveRight = ((Steer $ steerFactor * deltaTime) :)
        go Input.AimLeft = ((Aim $ -aimFactor * deltaTime) :)
        go Input.AimRight = ((Aim $ aimFactor * deltaTime) :)
        go Input.Fire = (Fire :)
        go Input.LayMine = (LayMine :)

-- Test script
dumbScript :: ScriptExecutor
dumbScript = ScriptExecutor $ script
  where
    script :: ScriptInput -> (ScriptExecutor, ScriptOutput)
    script _input = (dumbScript, [Throttle 10])
