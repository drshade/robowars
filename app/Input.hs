module Input where

import Raylib.Core (isKeyDown)
import Raylib.Types (KeyboardKey (..))

data InteractiveInput
    = MoveForward
    | MoveBackward
    | MoveLeft
    | MoveRight
    | AimLeft
    | AimRight
    | Fire
    | LayMine
    deriving (Eq, Show)

interactiveMap :: [(KeyboardKey, InteractiveInput)]
interactiveMap =
    [ (KeyW, MoveForward)
    , (KeyS, MoveBackward)
    , (KeyA, MoveLeft)
    , (KeyD, MoveRight)
    , (KeyQ, AimLeft)
    , (KeyE, AimRight)
    , (KeySpace, Fire)
    , (KeyX, LayMine)
    ]

getInteractiveInputs :: IO [InteractiveInput]
getInteractiveInputs = do
    inputs <- mapM (\(key, input) -> (,input) <$> isKeyDown key) interactiveMap
    pure $ snd <$> filter fst inputs