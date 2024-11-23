{-# LANGUAGE Arrows #-}

module ArrowsPlay where

import Control.Arrow
import Control.Arrow.Operations (fetch, store)
import Control.Arrow.Transformer.State (StateArrow, runState)

-- Define a simple State Arrow with an integer state
type StatefulArrow s a b = StateArrow s (->) a b

-- Our stateful arrow: increments the state by a given number
arrow1 :: StatefulArrow [Int] String Int
arrow1 = proc _x -> do
  -- Fetch the current state
  currentState <- fetch -< ()
  -- Update the state by adding increment
  store -< 1 : currentState
  -- Output the new state as the result of this arrow
  returnA -< 1

arrow2 :: StatefulArrow [Int] String Int
arrow2 = proc _x -> do
  -- Fetch the current state
  currentState <- fetch -< ()
  -- Update the state by adding increment
  store -< 1 : currentState
  -- Output the new state as the result of this arrow
  returnA -< 1

prog1 :: StatefulArrow [Int] String Int
prog1 = proc _x -> do
  (r1, (r2, r3)) <- (arrow1 *** arrow1 *** arrow2) -< ("hello1", ("hello2", "hello3"))
  returnA -< r1 + r2 + r3

-- Helper function to run the arrow with an initial state and input value
run :: StatefulArrow s a b -> s -> a -> (b, s)
run arrow state a =
  let x = runState arrow
   in x (a, state)

-- Example usage
test :: IO ()
test = do
  let result = run prog1 [] "hello"
   in putStrLn $ show result