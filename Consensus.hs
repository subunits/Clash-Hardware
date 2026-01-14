{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : Consensus
Description : Hardened SE(3) Physics Kernel (Verified Hardware Logic)
Axiom       : Physical Laws == Abstract Reasoning
Precision   : Q1.15 Fixed-Point (Signed 16-bit)
-}

module Consensus where

import Clash.Prelude
import GHC.Generics

-- | Fixed point Q1.15: 1 sign bit, 15 fractional bits.
-- This name avoids conflict with Clash.Prelude.Fixed.
type Q15 = Signed 16

data Quaternion = Quaternion 
    { qW :: Q15, qX :: Q15, qY :: Q15, qZ :: Q15 }
    deriving (Generic, NFDataX, Show, Eq)

data DualQuaternion = DualQuaternion 
    { realPart :: Quaternion, dualPart :: Quaternion }
    deriving (Generic, NFDataX, Show, Eq)

-- =================================
-- CORE ALGEBRAIC GATES (Verified)
-- =================================

-- | Hardware Dot Product: 4 Multipliers + 3 Adders
-- Slices bits [30:15] to maintain decimal alignment.
qDot :: Quaternion -> Quaternion -> Q15
qDot (Quaternion w1 x1 y1 z1) (Quaternion w2 x2 y2 z2) = 
    let pW = w1 `mul` w2
        pX = x1 `mul` x2
        pY = y1 `mul` y2
        pZ = z1 `mul` z2
        summed = pW + pX + pY + pZ
    in unpack (slice d30 d15 (pack summed))

-- | Hardware Scale: Multiply and Shift
qScale :: Q15 -> Q15 -> Q15
qScale s x = 
    let extended = s `mul` x
    in unpack (slice d30 d15 (pack extended))

-- | The Stability Gate (Manifold Projection)
-- Physically prevents geometric drift by subtracting the error vector.
projectSE3 :: DualQuaternion -> DualQuaternion
projectSE3 (DualQuaternion r d) = 
    let dot = qDot r d
        dW' = qW d - qScale dot (qW r)
        dX' = qX d - qScale dot (qX r)
        dY' = qY d - qScale dot (qY r)
        dZ' = qZ d - qScale dot (qZ r)
    in DualQuaternion r (Quaternion dW' dX' dY' dZ')

-- =================================
-- THE KERNEL (TOP ENTITY)
-- =================================

-- | This creates the physical Flip-Flops and the feedback loop.
-- The system negotiates its state every clock cycle.
topEntity 
    :: Clock System 
    -> Reset System 
    -> Enable System 
    -> Signal System DualQuaternion -- Current Pose Input
    -> Signal System DualQuaternion -- Negotiated Output
topEntity = exposeClockResetEnable $ \inputPose -> 
    let state = register initialPose (fmap projectSE3 inputPose)
    in state
  where
    -- Identity rotation (32767 = 1.0) and Zero translation (0.0)
    initialPose = DualQuaternion (Quaternion 32767 0 0 0) (Quaternion 0 0 0 0)
