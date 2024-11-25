module Scripts where

import Debug.Trace (trace)
import Input qualified as Input
import Script
import Types

-- Convert interactive input directly to instructions
interactiveScript :: Script
interactiveScript = script () $ do
  input <- getInteractiveInput
  mapM go input
  where
    go Input.MoveForward = instruct $ Throttle 15000
    go Input.MoveBackward = instruct $ Throttle (-15000)
    go Input.MoveLeft = instruct $ Steer (-5000)
    go Input.MoveRight = instruct $ Steer 5000
    go Input.AimLeft = instruct $ Aim (-5000)
    go Input.AimRight = instruct $ Aim 5000
    go Input.Fire = instruct Fire
    go Input.LayMine = instruct LayMine

-- Test script
dumbScript :: Script
dumbScript = script () $ do
  instruct $ Steer (-10)
  instruct $ Throttle 10
  instruct $ Aim (-2)
  instruct $ Fire

-- Test script showing use of state
sillyScript :: Script
sillyScript = script (0 :: Int, True) $ do
  (count, forward) <- getState

  case forward of
    True | count > 100 -> setState (0, False)
    True -> setState (count + 1, forward)
    False | count < (-100) -> setState (0, True)
    False -> setState (count - 1, forward)

  instruct (Steer (-100))
  instruct $ Throttle (fromIntegral count * 100)
  instruct (Aim $ fromIntegral count * 10)
  instruct Fire
  pure "hello"
