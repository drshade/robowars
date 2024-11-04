module Types where

import Input (MovementInput)
import Text.Printf (printf)

data Position = Position Float Float deriving (Show)

newtype RotationRate = RotationRate Float deriving (Ord, Eq, Num)

instance Show RotationRate where
    show :: RotationRate -> String
    show (RotationRate x) = printf "RotationRate %0.2f" x

newtype Rotation = Rotation Float deriving (Ord, Eq, Num)

instance Show Rotation where
    show :: Rotation -> String
    show (Rotation x) = printf "Rotation %0.2f" x

newtype Acceleration = Acceleration Float deriving (Ord, Eq, Num)

instance Show Acceleration where
    show :: Acceleration -> String
    show (Acceleration x) = printf "Acceleration %0.2f" x

newtype Speed = Speed Float deriving (Ord, Eq, Num)

instance Show Speed where
    show :: Speed -> String
    show (Speed x) = printf "Speed %0.2f" x

data Liveliness = Alive | Dead deriving (Eq, Show)

newtype MaxAcceleration = MaxAcceleration Acceleration deriving (Ord, Eq, Num, Show)
newtype MaxSpeed = MaxSpeed Speed deriving (Ord, Eq, Num, Show)
newtype MaxRotationRate = MaxRotationRate RotationRate deriving (Ord, Eq, Num, Show)

data Platform
    = MovingPlatform
        { position :: Position
        , rotation :: Rotation
        , speed :: Speed
        }
    | MountedPlatform
        { rotation :: Rotation
        }
    deriving (Show)

data PlatformDynamics = PlatformDynamics
    { rotationRate :: RotationRate
    , acceleration :: Acceleration
    }
    deriving (Show)

data Limits = Limits
    { maxAcceleration :: MaxAcceleration
    , maxSpeed :: MaxSpeed
    , maxRotationRate :: MaxRotationRate
    }
    deriving (Show)

data Entity
    = Tank {platformDynamics :: PlatformDynamics, platform :: Platform, turretDynamics :: PlatformDynamics, turret :: Platform, limits :: Limits}
    | Projectile {dynamics :: PlatformDynamics, platform :: Platform, limits :: Limits}
    | Mine {platform :: Platform}

-- Allows user input temporarily
type Script = (TotalTime -> DeltaTime -> [MovementInput] -> Instruction)

data Instruction
    = Throttle Float
    | Steer Float
    | Aim Float
    | Fire
    | LayMine
    | DoNothing

type TotalTime = Double
type DeltaTime = Float