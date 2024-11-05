{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Types where

import Script (ScriptRunner)
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

data Entity
    = Tank Dynamics Platform Dynamics Platform Limits ScriptRunner
    | Projectile Dynamics Platform Limits
    | Mine Platform
