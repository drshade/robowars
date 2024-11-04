{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Game where

import Input (MovementInput)

data Position = Position Float Float deriving (Show)

newtype RotationRate = RotationRate Float deriving (Ord, Eq, Num, Show)
newtype Rotation = Rotation Float deriving (Ord, Eq, Num, Show)

newtype Acceleration = Acceleration Float deriving (Ord, Eq, Num, Show)
newtype Speed = Speed Float deriving (Ord, Eq, Num, Show)

data Liveliness = Alive | Dead deriving (Eq, Show)

newtype MaxAcceleration = MaxAcceleration Acceleration deriving (Ord, Eq, Num, Show)
newtype MaxSpeed = MaxSpeed Speed deriving (Ord, Eq, Num, Show)
newtype MaxRotationRate = MaxRotationRate RotationRate deriving (Ord, Eq, Num, Show)

data MovingPlatform = MovingPlatform
    { position :: Position
    , rotation :: Rotation
    , rotationRate :: RotationRate
    , acceleration :: Acceleration
    , speed :: Speed
    }
    deriving (Show)

data StandingPlatform = StandingPlatform
    { position :: Position
    , rotation :: Rotation
    , rotationRate :: RotationRate
    }
    deriving (Show)

data Limits = Limits
    { maxAcceleration :: MaxAcceleration
    , maxSpeed :: MaxSpeed
    , maxRotationRate :: MaxRotationRate
    }
    deriving (Show)

data Entity
    = Tank MovingPlatform StandingPlatform Limits Script
    | Projectile MovingPlatform Limits
    | Mine StandingPlatform

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

tick :: TotalTime -> DeltaTime -> [MovementInput] -> Entity -> Entity
tick totalTime deltaTime movementInput (Tank platform turret lims script) =
    let op = script totalTime deltaTime movementInput
        -- Perform the movement / rotation transformations requested
        (platform', turret') = case op of
            -- Use lenses here...
            Throttle amount ->
                ((platform{acceleration = platform.acceleration + Acceleration (amount * deltaTime)}), turret)
            Steer amount ->
                ((platform :: MovingPlatform){rotationRate = platform.rotationRate + RotationRate (amount * deltaTime)}, turret)
            Aim amount ->
                (platform, ((turret :: StandingPlatform){rotationRate = turret.rotationRate + RotationRate (amount * deltaTime)}))
            _ -> (platform, turret)

        -- Clamp the values to the limits
        platform'' = clampMovingPlatform platform' lims
        turret'' = clampStandingPlatform turret' lims

        -- Calculate the new values
        newRotation = calcRotationFromRotationRate deltaTime (platform''.rotation) (platform''.rotationRate)
        newTurretRotation = calcRotationFromRotationRate deltaTime (turret''.rotation) (turret''.rotationRate)
        newSpeed = calcSpeedFromAcceleration deltaTime (platform''.speed) (platform''.acceleration)
        newPos = calcPositionFromSpeed deltaTime (platform''.position) (platform''.rotation) newSpeed
     in Tank (platform''{speed = newSpeed, position = newPos, rotation = newRotation}) (turret''{rotation = newTurretRotation}) lims script
tick totalTime deltaTime movementInput mine@(Mine sp) = mine
tick totalTime deltaTime movementInput (Projectile mp lims) =
    proj
  where
    newSpeed = calcSpeedFromAcceleration deltaTime (mp.speed) (mp.acceleration)
    newPos = calcPositionFromSpeed deltaTime (mp.position) (mp.rotation) newSpeed
    proj = Projectile (mp{speed = newSpeed, position = newPos}) lims

clampMovingPlatform :: MovingPlatform -> Limits -> MovingPlatform
clampMovingPlatform (MovingPlatform position rotation rotationRate acceleration speed) (Limits (MaxAcceleration maxAcceleration) (MaxSpeed maxSpeed) (MaxRotationRate maxRotationRate)) =
    MovingPlatform
        position
        rotation
        (clamp rotationRate maxRotationRate)
        (clamp acceleration maxAcceleration)
        (clamp speed maxSpeed)

clampStandingPlatform :: StandingPlatform -> Limits -> StandingPlatform
clampStandingPlatform (StandingPlatform position rotation (RotationRate rotationRate)) (Limits _ _ (MaxRotationRate (RotationRate maxRotationRate))) =
    StandingPlatform position rotation (RotationRate (clamp rotationRate maxRotationRate))

clamp :: (Ord a) => (Num a) => a -> a -> a
clamp value max' =
    if abs value > max' then (if value < 0 then (max' * (-1)) else max') else (value)

toRadians :: Float -> Float
toRadians deg = deg * pi / 180

-- 0 degrees is up, 90 degrees is right, 180 degrees is down, 270 degrees is left
calcPositionFromSpeed :: DeltaTime -> Position -> Rotation -> Speed -> Position
calcPositionFromSpeed delta (Position x y) (Rotation rot) (Speed speed) =
    Position (x + speed * sin (toRadians rot) * delta) (y + speed * cos (toRadians rot) * delta * (-1))

calcSpeedFromAcceleration :: DeltaTime -> Speed -> Acceleration -> Speed
calcSpeedFromAcceleration delta (Speed currentSpeed) (Acceleration acceleration) =
    Speed $ currentSpeed + acceleration * delta

calcRotationFromRotationRate :: DeltaTime -> Rotation -> RotationRate -> Rotation
calcRotationFromRotationRate delta (Rotation currentRot) (RotationRate rate) =
    Rotation $ currentRot + rate * delta

class Default a where
    def :: a

instance Default MovingPlatform where
    def :: MovingPlatform
    def = MovingPlatform (Position 0 0) (Rotation 0) (RotationRate 0) (Acceleration 0) (Speed 0)

instance Default Limits where
    def :: Limits
    def = Limits (MaxAcceleration (Acceleration 0)) (MaxSpeed (Speed 0)) (MaxRotationRate (RotationRate 0))

tankLimits :: Limits
tankLimits = Limits (MaxAcceleration (Acceleration 150)) (MaxSpeed (Speed 65)) (MaxRotationRate (RotationRate 25))

instance Default StandingPlatform where
    def :: StandingPlatform
    def = StandingPlatform (Position 0 0) (Rotation 0) (RotationRate 0)