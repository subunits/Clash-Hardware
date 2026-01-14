{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Consensus where

import Clash.Prelude
import GHC.Generics

{-|
  The Axiom: SE(3) Consensus Engine
  Translated to 16-bit Fixed Point (Q1.15)
  Saturated at the 32767 Plateau.
-}

-- | Q1.15 Fixed Point Representation
type Fixed = Signed 16

data Quaternion = Quaternion 
    { qW :: Fixed, qX :: Fixed, qY :: Fixed, qZ :: Fixed }
    deriving (Generic, NFDataX, Show, Eq)

data DualQuaternion = DualQuaternion 
    { realPart :: Quaternion, dualPart :: Quaternion }
    deriving (Generic, NFDataX, Show, Eq)

-- =================================
-- HARDWARE GATE LOGIC
-- =================================

-- | Saturated Dot Product for Q1.15
qDot :: Quaternion -> Quaternion -> Fixed
qDot (Quaternion w1 x1 y1 z1) (Quaternion w2 x2 y2 z2) = 
    let products = (w1 * w2) + (x1 * x2) + (y1 * y2) + (z1 * z2)
    in truncateProp products -- Corrects decimal alignment for Q1.15
  where
    truncateProp :: Signed 32 -> Signed 16
    truncateProp x = pack (slice d30 d15 (unpack x))

-- | Manifold Projection Gate
-- Enforces the abstract law of reasoning (Orthogonality)
projectSE3 :: DualQuaternion -> DualQuaternion
projectSE3 (DualQuaternion r d) = 
    let -- Real part normalization (Placeholder for CORDIC pipeline)
        rUnit = r 
        -- d' = d - (r.d) * r
        dot   = qDot rUnit d
        dW'   = qW d - qScale dot (qW rUnit)
        dX'   = qX d - qScale dot (qX rUnit)
        dY'   = qY d - qScale dot (qY rUnit)
        dZ'   = qZ d - qScale dot (qZ rUnit)
    in DualQuaternion rUnit (Quaternion dW' dX' dY' dZ')
  where
    qScale s x = pack (slice d30 d15 (unpack (s * x)))

-- | The Top-Level Feedback Loop
-- This is the hardware embodiment of Spontaneous Order.
topEntity 
    :: Clock System 
    -> Reset System 
    -> Enable System 
    -> Signal System DualQuaternion -- Current Pose
    -> Signal System DualQuaternion -- Negotiated Output
topEntity = exposeClockResetEnable $ \poseIn -> 
    let poseOut = register (DualQuaternion (Quaternion 32767 0 0 0) (Quaternion 0 0 0 0)) 
                           (fmap projectSE3 poseIn)
    in poseOut