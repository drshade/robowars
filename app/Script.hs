{-# LANGUAGE DeriveGeneric #-}

module Script where

import Control.Monad.RWS (RWS, ask, execRWS, get, modify, put, tell)
import Debug.Trace (trace)
import Input qualified (InteractiveInput (..))
import Types

script :: s -> RWS ScriptInput ScriptOutput s a -> Script
script initState rws = Script $ runScript rws initState
  where
    runScript :: RWS ScriptInput ScriptOutput s a -> s -> ScriptInput -> (Script, ScriptOutput)
    runScript rws' startingState input =
      let (state, output) = execRWS rws' input startingState
       in (Script $ (runScript rws' state), output)

instruct :: Instruction -> RWS ScriptInput ScriptOutput s ()
instruct instruction = tell [instruction]

getState :: RWS ScriptInput ScriptOutput s s
getState = get

modifyState :: (s -> s) -> RWS ScriptInput ScriptOutput s ()
modifyState = modify

setState :: s -> RWS ScriptInput ScriptOutput s ()
setState = put

getTotalTime :: RWS ScriptInput ScriptOutput s TotalTime
getTotalTime = do
  (totalTime, _, _) <- ask
  pure totalTime

getDeltaTime :: RWS ScriptInput ScriptOutput s TotalTime
getDeltaTime = do
  (_, deltaTime, _) <- ask
  pure deltaTime

getInteractiveInput :: RWS ScriptInput ScriptOutput s [Input.InteractiveInput]
getInteractiveInput = do
  (_, _, interactiveInput) <- ask
  pure interactiveInput
