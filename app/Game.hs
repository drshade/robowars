{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Game where

import Input (MovementInput)
import Types

tick :: TotalTime -> DeltaTime -> [MovementInput] -> Script -> Entity -> Entity
tick totalTime deltaTime movementInput script (Tank platformDynamics platform turretDynamics turret limits) =
    let op = script totalTime deltaTime movementInput

        -- Perform the movement / rotation transformations requested and clamp them to limits
        new'platformDynamics :: PlatformDynamics
        new'platformDynamics = clampPlatformDynamics limits $ case op of
            Throttle amount ->
                platformDynamics{acceleration = platformDynamics.acceleration + Acceleration (amount * deltaTime)}
            Steer amount ->
                platformDynamics{rotationRate = platformDynamics.rotationRate + RotationRate (amount * deltaTime)}
            _ -> platformDynamics

        new'turretDynamics :: PlatformDynamics
        new'turretDynamics = clampPlatformDynamics limits $ case op of
            Aim amount ->
                turretDynamics{rotationRate = turretDynamics.rotationRate + RotationRate (amount * deltaTime)}
            _ -> turretDynamics

        -- Calculate the new values
        new'rotation = calcRotationFromRotationRate deltaTime (platform.rotation) (new'platformDynamics.rotationRate)
        new'speed = calcSpeedFromAcceleration deltaTime (platform.speed) (new'platformDynamics.acceleration)
        new'pos = calcPositionFromSpeed deltaTime (platform.position) (platform.rotation) new'speed
        new'turretRotation = calcRotationFromRotationRate deltaTime (turret.rotation) (new'turretDynamics.rotationRate)

        new'platform = clampPlatform limits (MovingPlatform new'pos new'rotation new'speed)
        new'turretPlatform = clampPlatform limits (MountedPlatform new'turretRotation)
     in Tank new'platformDynamics new'platform new'turretDynamics new'turretPlatform limits
tick totalTime deltaTime movementInput script mine@(Mine _) = mine
tick totalTime deltaTime movementInput script proj@(Projectile dynamics platform limits) =
    proj
  where
    new'dynamics = clampPlatformDynamics limits dynamics
    newSpeed = calcSpeedFromAcceleration deltaTime (platform.speed) (dynamics.acceleration)
    newPos = calcPositionFromSpeed deltaTime (platform.position) (platform.rotation) newSpeed

    new'platform = clampPlatform limits (MovingPlatform newPos (platform.rotation) newSpeed)

    proj = Projectile new'dynamics new'platform limits

clampPlatform :: Limits -> Platform -> Platform
clampPlatform (Limits _ (MaxSpeed maxSpeed) _) (MovingPlatform position rotation speed) =
    MovingPlatform
        position
        rotation
        (clamp speed maxSpeed)
clampPlatform (Limits _ _ _) (MountedPlatform rotation) =
    MountedPlatform rotation

clampPlatformDynamics :: Limits -> PlatformDynamics -> PlatformDynamics
clampPlatformDynamics (Limits (MaxAcceleration maxAcceleration) _ (MaxRotationRate maxRotationRate)) (PlatformDynamics rotationRate acceleration) =
    PlatformDynamics
        (clamp rotationRate maxRotationRate)
        (clamp acceleration maxAcceleration)

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

instance Default PlatformDynamics where
    def :: PlatformDynamics
    def = PlatformDynamics (RotationRate 0) (Acceleration 0)

instance Default Limits where
    def :: Limits
    def = Limits (MaxAcceleration (Acceleration 0)) (MaxSpeed (Speed 0)) (MaxRotationRate (RotationRate 0))

tankLimits :: Limits
tankLimits = Limits (MaxAcceleration (Acceleration 150)) (MaxSpeed (Speed 65)) (MaxRotationRate (RotationRate 25))

projectileLimits :: Limits
projectileLimits = Limits (MaxAcceleration (Acceleration 1000)) (MaxSpeed (Speed 1000)) (MaxRotationRate (RotationRate 0))