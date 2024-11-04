module Script where

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, get, put, runStateT)
import Input (InteractiveInput)
import Input qualified (InteractiveInput (..))

data Instruction
    = Throttle Float
    | Steer Float
    | Aim Float
    | Fire
    | LayMine
    | DoNothing

type TotalTime = Double
type DeltaTime = Float

-- Monad stack for scripts
type Script a = StateT [Instruction] (ReaderT (TotalTime, DeltaTime, [InteractiveInput]) IO) a

runScript :: Script a -> TotalTime -> DeltaTime -> [InteractiveInput] -> IO [Instruction]
runScript script totalTime deltaTime interactiveInput = do
    let initialState = []
    let readerState = (totalTime, deltaTime, interactiveInput)
    snd <$> runReaderT (runStateT script initialState) readerState

-- Test script
tankTestScript :: Script ()
tankTestScript = do
    _x <- ask
    _y <- get
    pure ()

-- Doesn't handle more than 1 input at a time...
tankHumanInput :: Script ()
tankHumanInput = do
    (_, deltaTime, movementInput) <- ask
    put $ foldr (go deltaTime) [] movementInput
  where
    throttleFactor = 15000
    steerFactor = 5000
    aimFactor = 5000
    go deltaTime Input.MoveForward = ((Throttle $ throttleFactor * deltaTime) :)
    go deltaTime Input.MoveBackward = ((Throttle $ -throttleFactor * deltaTime) :)
    go deltaTime Input.MoveLeft = ((Steer $ -steerFactor * deltaTime) :)
    go deltaTime Input.MoveRight = ((Steer $ steerFactor * deltaTime) :)
    go deltaTime Input.AimLeft = ((Aim $ -aimFactor * deltaTime) :)
    go deltaTime Input.AimRight = ((Aim $ aimFactor * deltaTime) :)
    go _deltaTime Input.Fire = (Fire :)
    go _deltaTime Input.LayMine = (LayMine :)

tankAutoScript :: Script ()
tankAutoScript = do
    (totalTime, _, _) <- ask
    put $ [go totalTime]
  where
    go totalTime
        | totalTime > 1 && totalTime < 2 = Throttle 100
        | totalTime > 4 && totalTime < 6 = Steer 100
        | totalTime > 8 && totalTime < 10 = Throttle (-100)
        | totalTime > 12 && totalTime < 14 = Aim (100)
        | totalTime > 16 && totalTime < 30 = Fire
        | otherwise = DoNothing
