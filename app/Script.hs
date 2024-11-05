{-# LANGUAGE DeriveGeneric #-}

module Script where

import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, get, put, runStateT)
import Data.Binary (Binary, decode, encode)
import GHC.Generics (Generic)
import Input (InteractiveInput)
import Input qualified (InteractiveInput (..))

data Instruction
    = Throttle Float
    | Steer Float
    | Aim Float
    | Fire
    | LayMine
    | ScanRadar
    | DoNothing

type TotalTime = Double
type DeltaTime = Float

data UserState = forall s. (Binary s) => UserState s

-- Monad stack for scripts
type Script a = StateT (UserState, [Instruction]) (ReaderT (TotalTime, DeltaTime, [InteractiveInput]) Identity) a

runScript :: Script a -> UserState -> TotalTime -> DeltaTime -> [InteractiveInput] -> [Instruction]
runScript script initialState totalTime deltaTime interactiveInput = do
    let readerState = (totalTime, deltaTime, interactiveInput)
    snd $ snd <$> runIdentity $ runReaderT (runStateT script (initialState, [])) readerState

type ScriptDef = (UserState, Script ())

data Test1UserState = Test1UserState (Int, Int, Int) deriving (Generic)
instance Binary Test1UserState

test1'scriptdef :: ScriptDef
test1'scriptdef =
    (initialState, scriptLoop)
  where
    initialState = UserState @(Int, Int, Int) (1, 2, 3)
    scriptLoop = do
        -- UserState raw <- getMyState
        -- let x :: (Int, Int, Int)
        --     x = decode raw
        putInstructions [Fire]

test2'scriptdef :: ScriptDef
test2'scriptdef =
    (initialState, scriptLoop)
  where
    initialState = UserState "hello"
    scriptLoop = putInstructions [Steer 100]

putInstructions :: [Instruction] -> Script ()
putInstructions instructions = do
    (userState, _) <- get
    put (userState, instructions)

modifyMyState :: (UserState -> UserState) -> Script ()
modifyMyState f = do
    (userState, instructions) <- get
    put (f userState, instructions)

putMyState :: UserState -> Script ()
putMyState userState = do
    modifyMyState (const userState)

getMyState :: Script UserState
getMyState = do
    (userState, _) <- get
    pure userState

getTotalTime :: Script TotalTime
getTotalTime = do
    (totalTime, _, _) <- ask
    pure totalTime

getDeltaTime :: Script DeltaTime
getDeltaTime = do
    (_, deltaTime, _) <- ask
    pure deltaTime

getInteractiveInput :: Script [InteractiveInput]
getInteractiveInput = do
    (_, _, interactiveInput) <- ask
    pure interactiveInput

-- Test script
tankTestScript :: Script ()
tankTestScript = do
    _x <- ask
    _y <- get
    pure ()

-- Doesn't handle more than 1 input at a time...
tankHumanInput :: Script ()
tankHumanInput = do
    deltaTime <- getDeltaTime
    interactiveInput <- getInteractiveInput
    putInstructions $ foldr (go deltaTime) [] interactiveInput
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

toms'script :: Script ()
toms'script = do
    (totalTime, _, _) <- ask
    putInstructions $ [go totalTime]
  where
    go totalTime
        | totalTime > 1 && totalTime < 2 = Throttle 100
        | totalTime > 4 && totalTime < 6 = Steer 100
        | totalTime > 8 && totalTime < 10 = Throttle (-100)
        | totalTime > 12 && totalTime < 14 = Aim (100)
        | totalTime > 16 && totalTime < 30 = Fire
        | otherwise = DoNothing

bhaveshs'script :: Script ()
bhaveshs'script = do
    (totalTime, _, _) <- ask
    putInstructions $ [Fire, Throttle 100, Steer (-100), Aim (-50)]
