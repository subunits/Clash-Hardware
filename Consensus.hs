{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}

module Consensus where

import Clash.Prelude
import GHC.Generics

-- | Unique name to avoid conflict with Clash.Prelude.Fixed
type Q15 = Signed 16

data Quaternion = Quaternion 
    { qW :: Q15, qX :: Q15, qY :: Q15, qZ :: Q15 }
    deriving (Generic, NFDataX, Show, Eq)

data DualQuaternion = DualQuaternion 
    { realPart :: Quaternion, dualPart :: Quaternion }
    deriving (Generic, NFDataX, Show, Eq)

-- | Hardware Dot Product (Q1.15)
qDot :: Quaternion -> Quaternion -> Q15
qDot (Quaternion w1 x1 y1 z1) (Quaternion w2 x2 y2 z2) = 
    let pW = w1 * w2
        pX = x1 * x2
        pY = y1 * y2
        pZ = z1 * z2
    in pack (slice d30 d15 (unpack (pW + pX + pY + pZ)))

-- | Hardware Scale: Multiply and Shift
qScale :: Q15 -> Q15 -> Q15
qScale s x = pack (slice d30 d15 (unpack (s * x)))

-- | The Stability Gate (Manifold Projection)
projectSE3 :: DualQuaternion -> DualQuaternion
projectSE3 (DualQuaternion r d) = 
    let dot = qDot r d
        dW' = qW d - qScale dot (qW r)
        dX' = qX d - qScale dot (qX r)
        dY' = qY d - qScale dot (qY r)
        dZ' = qZ d - qScale dot (qZ r)
    in DualQuaternion r (Quaternion dW' dX' dY' dZ')

-- | The Kernel (Top Entity)
topEntity 
    :: Clock System 
    -> Reset System 
    -> Enable System 
    -> Signal System DualQuaternion
    -> Signal System DualQuaternion
topEntity = exposeClockResetEnable $ \inputPose -> 
    let state = register initialPose (fmap projectSE3 inputPose)
    in state
  where
    initialPose = DualQuaternion (Quaternion 32767 0 0 0) (Quaternion 0 0 0 0)
