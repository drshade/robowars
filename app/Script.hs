{-# LANGUAGE DeriveGeneric #-}

module Script where

import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, get, put, runStateT)
import Data.Binary (Binary, decode, encode)
import Data.ByteString.Lazy qualified as BS
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
type Script a = StateT [Instruction] (ReaderT (TotalTime, DeltaTime, [InteractiveInput]) Identity) a

type ScriptStateBlob = BS.ByteString
type ScriptRunner = Maybe ScriptStateBlob -> Script (Maybe ScriptStateBlob)

runScript :: ScriptRunner -> Maybe ScriptStateBlob -> TotalTime -> DeltaTime -> [InteractiveInput] -> (Maybe ScriptStateBlob, [Instruction])
runScript script raw'state totalTime deltaTime interactiveInput = do
    let state = runStateT (script raw'state) []
    let readerState = (totalTime, deltaTime, interactiveInput)
    runIdentity $ runReaderT state readerState

data Test1UserState = Test1UserState (Int, Int, Int) deriving (Generic)
instance Binary Test1UserState

test1'scriptdef :: Maybe ScriptStateBlob -> Script (Maybe ScriptStateBlob)
test1'scriptdef Nothing = pure $ Just $ encode ((0, 0, 0) :: (Int, Int, Int))
test1'scriptdef (Just raw'state) = do
    let (_, b, c) = decode raw'state :: (Int, Int, Int)
    put [Fire]
    pure $ Just $ encode (b, c, c + 1)

test2'scriptdef :: Maybe ScriptStateBlob -> Script (Maybe ScriptStateBlob)
test2'scriptdef _ = do
    put [Steer 100]
    pure Nothing

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
tankHumanInput :: Maybe ScriptStateBlob -> Script (Maybe ScriptStateBlob)
tankHumanInput _ = do
    deltaTime <- getDeltaTime
    interactiveInput <- getInteractiveInput
    put $ foldr (go deltaTime) [] interactiveInput
    pure Nothing
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
    totalTime <- getTotalTime
    put $ [go totalTime]
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
    totalTime <- getTotalTime
    put $ [Fire, Throttle 100, Steer (-100), Aim (-50)]
