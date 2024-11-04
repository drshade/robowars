{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Game where

import Script
import Types

applyDynamicsInstructions :: DeltaTime -> (Dynamics, Dynamics) -> [Instruction] -> (Dynamics, Dynamics)
applyDynamicsInstructions deltaTime dynamics instructions = foldr go dynamics instructions
  where
    go (Throttle amount) (Dynamics platform'rotationRate platform'acceleration, Dynamics turret'rotationRate turret'acceleration) =
        (Dynamics platform'rotationRate (platform'acceleration + Acceleration (amount * deltaTime)), Dynamics turret'rotationRate turret'acceleration)
    go (Steer amount) (Dynamics platform'rotationRate platform'acceleration, Dynamics turret'rotationRate turret'acceleration) =
        (Dynamics (platform'rotationRate + RotationRate (amount * deltaTime)) platform'acceleration, Dynamics turret'rotationRate turret'acceleration)
    go (Aim amount) (Dynamics platform'rotationRate platform'acceleration, Dynamics turret'rotationRate turret'acceleration) =
        (Dynamics platform'rotationRate platform'acceleration, Dynamics (turret'rotationRate + RotationRate (amount * deltaTime)) turret'acceleration)
    go _ dynamics' = dynamics'

applyActionInstructions :: DeltaTime -> Entity -> [Instruction] -> [Entity]
applyActionInstructions _ (Tank _ (MovingPlatform platform'position platform'rotation _) _ (MountedPlatform turret'rotation) _ _) instructions = foldr go [] instructions
  where
    go Fire acc = (Projectile (Dynamics (RotationRate 0) (Acceleration (-250))) (MovingPlatform platform'position (turret'rotation + platform'rotation) (Speed 600)) projectileLimits) : acc
    go LayMine acc = acc
    go _ acc = acc
applyActionInstructions _ _ _ = []

tick :: TotalTime -> DeltaTime -> Entity -> [Instruction] -> [Entity]
tick _totalTime deltaTime (Tank platformDynamics (MovingPlatform platform'position platform'rotation platform'speed) turretDynamics (MountedPlatform turret'rotation) limits script) instructions =
    let
        -- Perform the movement / rotation transformations requested and clamp them to limits
        (new'platformDynamics', new'turretDynamics') = applyDynamicsInstructions deltaTime (platformDynamics, turretDynamics) instructions

        -- Clamp the new values to the limits
        (new'platformDynamics, new'turretDynamics) =
            ( clampPlatformDynamics limits new'platformDynamics'
            , clampPlatformDynamics limits new'turretDynamics'
            )

        -- Calculate the new values
        new'rotation = calcRotationFromRotationRate deltaTime (platform'rotation) (rotationRate new'platformDynamics)
        new'speed = calcSpeedFromAcceleration deltaTime (platform'speed) (acceleration new'platformDynamics)
        new'pos = calcPositionFromSpeed deltaTime (platform'position) (platform'rotation) new'speed
        new'turretRotation = calcRotationFromRotationRate deltaTime (turret'rotation) (rotationRate new'turretDynamics)

        -- And finally clamp those
        new'platform = clampPlatform limits (MovingPlatform new'pos new'rotation new'speed)
        new'turretPlatform = clampPlatform limits (MountedPlatform new'turretRotation)

        newTank = Tank new'platformDynamics new'platform new'turretDynamics new'turretPlatform limits script

        -- Apply any actions, which could create new entities (like projectiles or mines)
        newEntities = applyActionInstructions deltaTime newTank instructions
     in
        newTank : newEntities
tick _totalTime _deltaTime mine@(Mine _) _instruction = [mine]
tick _totalTime deltaTime (Projectile dynamics (MovingPlatform platform'position platform'rotation platform'speed) limits) _instruction =
    -- nuke the projectile if it's dropped to the ground (speed 0)
    if platform'speed > 0 then [new'projectile] else []
  where
    new'dynamics = clampPlatformDynamics limits dynamics
    newSpeed = calcSpeedFromAcceleration deltaTime (platform'speed) (acceleration dynamics)
    newPos = calcPositionFromSpeed deltaTime (platform'position) (platform'rotation) newSpeed

    new'platform = clampPlatform limits (MovingPlatform newPos (platform'rotation) newSpeed)

    new'projectile = Projectile new'dynamics new'platform limits
-- Should warn...
tick _ _ _ _ = []

clampPlatform :: Limits -> Platform -> Platform
clampPlatform (Limits _ (MaxSpeed maxSpeed) _) (MovingPlatform position rotation speed) =
    MovingPlatform
        position
        rotation
        (clamp speed maxSpeed)
clampPlatform (Limits _ _ _) (MountedPlatform rotation) =
    MountedPlatform rotation

clampPlatformDynamics :: Limits -> Dynamics -> Dynamics
clampPlatformDynamics (Limits (MaxAcceleration maxAcceleration) _ (MaxRotationRate maxRotationRate)) (Dynamics platform'rotationRate platform'acceleration) =
    Dynamics
        (clamp platform'rotationRate maxRotationRate)
        (clamp platform'acceleration maxAcceleration)

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

instance Default Dynamics where
    def :: Dynamics
    def = Dynamics (RotationRate 0) (Acceleration 0)

instance Default Limits where
    def :: Limits
    def = Limits (MaxAcceleration (Acceleration 0)) (MaxSpeed (Speed 0)) (MaxRotationRate (RotationRate 0))

tankLimits :: Limits
tankLimits = Limits (MaxAcceleration (Acceleration 150)) (MaxSpeed (Speed 65)) (MaxRotationRate (RotationRate 25))

projectileLimits :: Limits
projectileLimits = Limits (MaxAcceleration (Acceleration 1000)) (MaxSpeed (Speed 1000)) (MaxRotationRate (RotationRate 0))