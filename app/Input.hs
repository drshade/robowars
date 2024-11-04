module Input where

import Data.Functor (($>))
import Raylib.Core (isKeyDown)
import Raylib.Types (KeyboardKey (..))

data MovementInput = MoveForward | MoveBackward | MoveLeft | MoveRight deriving (Eq, Show)
data ActionInput = Fire | LayMine deriving (Eq, Show)

movementMap :: [(KeyboardKey, MovementInput)]
movementMap = [(KeyW, MoveForward), (KeyS, MoveBackward), (KeyA, MoveLeft), (KeyD, MoveRight)]

getMovementInputs :: IO [MovementInput]
getMovementInputs = do
    inputs <- mapM (\(key, input) -> (,input) <$> isKeyDown key) movementMap
    pure $ snd <$> filter fst inputs