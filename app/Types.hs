{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Types where

import Control.Monad.RWS (RWS)
import Input (InteractiveInput)
import Text.Printf (printf)

-- In seconds
type TotalTime = Float

type DeltaTime = Float

-- In meters
data Position = Position Float Float deriving (Show)

-- In radians
newtype Rotation = Rotation Float deriving (Ord, Eq, Num)

-- In radians per second
newtype RotationRate = RotationRate Float deriving (Ord, Eq, Num)

-- In meters per second
newtype Speed = Speed Float deriving (Ord, Eq, Num)

-- In meters per second squared
newtype Acceleration = Acceleration Float deriving (Ord, Eq, Num)

instance Show RotationRate where
  show :: RotationRate -> String
  show (RotationRate x) = printf "RotationRate %0.2f" x

instance Show Rotation where
  show :: Rotation -> String
  show (Rotation x) = printf "Rotation %0.2f" x

instance Show Acceleration where
  show :: Acceleration -> String
  show (Acceleration x) = printf "Acceleration %0.2f" x

instance Show Speed where
  show :: Speed -> String
  show (Speed x) = printf "Speed %0.2f" x

data Liveliness = Alive | Dead deriving (Eq, Show)

newtype MaxAcceleration = MaxAcceleration Acceleration deriving (Ord, Eq, Num, Show)

newtype MaxSpeed = MaxSpeed Speed deriving (Ord, Eq, Num, Show)

newtype MaxRotationRate = MaxRotationRate RotationRate deriving (Ord, Eq, Num, Show)

data Platform
  = MovingPlatform Position Rotation Speed
  | MountedPlatform Rotation
  deriving (Show)

data Dynamics = Dynamics RotationRate Acceleration
  deriving (Show)

rotationRate :: Dynamics -> RotationRate
rotationRate (Dynamics rotationRate' _) = rotationRate'

acceleration :: Dynamics -> Acceleration
acceleration (Dynamics _ acceleration') = acceleration'

data Limits = Limits MaxAcceleration MaxSpeed MaxRotationRate
  deriving (Show)

data Instruction
  = Throttle Float
  | Steer Float
  | Aim Float
  | Fire
  | LayMine
  | ScanRadar
  | DoNothing

type ScriptInput = (TotalTime, DeltaTime, [InteractiveInput])

type ScriptOutput = [Instruction]

data Script
  = Script (ScriptInput -> (Script, ScriptOutput))

-- \| ScriptWithState (RWS ScriptInput ScriptOutput a ())

data Entity
  = Tank Dynamics Platform Dynamics Platform Limits Script
  | Projectile Dynamics Platform Limits
  | Mine Platform

--
-- The physics engine
--
toRadians :: Rotation -> Rotation
toRadians (Rotation rotation) = Rotation $ rotation * pi / 180

data PositionRotation = PositionRotation Position Rotation

data PositionRotationSpeed = PositionRotationSpeed Position Rotation Speed

data SpeedAcceleration = SpeedAcceleration Speed Acceleration

data RotationRotationRate = RotationRotationRate Rotation RotationRate

class Derive a b c | a b -> c where
  (-/<) :: a -> b -> c

instance Derive Position Rotation PositionRotation where
  position' -/< rotation' = PositionRotation position' rotation'

instance Derive PositionRotation Speed PositionRotationSpeed where
  (PositionRotation position' rotation') -/< speed' = PositionRotationSpeed position' rotation' speed'

instance Derive Speed Acceleration SpeedAcceleration where
  speed' -/< acceleration' = SpeedAcceleration speed' acceleration'

instance Derive Rotation RotationRate RotationRotationRate where
  rotation' -/< rotationRate' = RotationRotationRate rotation' rotationRate'

instance Derive PositionRotationSpeed DeltaTime Position where
  (PositionRotationSpeed (Position posx' posy') rotation' (Speed speed')) -/< delta' =
    let Rotation rotationRadians = toRadians rotation'
     in Position (posx' + speed' * sin rotationRadians * delta') (posy' + speed' * cos rotationRadians * delta' * (-1))

instance Derive SpeedAcceleration DeltaTime Speed where
  (SpeedAcceleration (Speed currentSpeed') (Acceleration acceleration')) -/< delta' =
    Speed $ currentSpeed' + acceleration' * delta'

instance Derive RotationRotationRate DeltaTime Rotation where
  (RotationRotationRate (Rotation rotation') (RotationRate rotationRate')) -/< delta' =
    Rotation $ rotation' + rotationRate' * delta'

infixl 8 -/<
