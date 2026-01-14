{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-|
Module      : Main (AKI Final)
Description : Refined SE(3) Physics Kernel & High-Energy Consensus Engine.
Architecture: Unit Dual Quaternions with Manifold Projection.
-}

module Main where

import System.Random
import Control.DeepSeq
import GHC.Generics
import Data.Aeson (encode, ToJSON)
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Monad (foldM)

-- =================================
-- ALGEBRAIC BASE: SE(3) KERNEL
-- =================================

data Quaternion = Quaternion 
  { qW :: Double, qX :: Double, qY :: Double, qZ :: Double }
  deriving (Show, Generic, ToJSON, NFData)

data DualQuaternion = DualQuaternion 
  { realPart :: !Quaternion, dualPart :: !Quaternion }
  deriving (Show, Generic, ToJSON, NFData)

-- Basic Quaternion Operations
qAdd :: Quaternion -> Quaternion -> Quaternion
qAdd (Quaternion w1 x1 y1 z1) (Quaternion w2 x2 y2 z2) = 
  Quaternion (w1+w2) (x1+x2) (y1+y2) (z1+z2)

qScale :: Double -> Quaternion -> Quaternion
qScale s (Quaternion w x y z) = Quaternion (s*w) (s*x) (s*y) (s*z)

qDot :: Quaternion -> Quaternion -> Double
qDot (Quaternion w1 x1 y1 z1) (Quaternion w2 x2 y2 z2) = 
  (w1*w2) + (x1*x2) + (y1*y2) + (z1*z2)

qNorm :: Quaternion -> Double
qNorm q = sqrt (qDot q q)

qNormalize :: Quaternion -> Quaternion
qNormalize q = 
  let n = qNorm q 
  in if n < 1e-12 then Quaternion 1 0 0 0 else qScale (1.0/n) q

qMul :: Quaternion -> Quaternion -> Quaternion
qMul (Quaternion w1 x1 y1 z1) (Quaternion w2 x2 y2 z2) =
  Quaternion (w1*w2 - x1*x2 - y1*y2 - z1*z2)
             (w1*x2 + x1*w2 + y1*z2 - z1*y2)
             (w1*y2 - x1*z2 + y1*w2 + z1*x2)
             (w1*z2 + x1*y2 - y1*x2 + z1*w2)

-- | Stability Protocol: SE(3) Manifold Projection
-- Enforces ||r|| = 1 and r · d = 0 (Orthogonality)
projectSE3 :: DualQuaternion -> DualQuaternion
projectSE3 (DualQuaternion r d) = 
  let rUnit = qNormalize r
      -- Project dual part to be orthogonal to real part
      dOrth = d `qAdd` qScale (-(qDot rUnit d)) rUnit
  in DualQuaternion rUnit dOrth

-- | Coupled Kinematic Evolution
-- Correctly maps spin (omega) and flow (v) to the dual quaternion derivative
stepPose :: DualQuaternion -> Quaternion -> Quaternion -> Double -> DualQuaternion
stepPose (DualQuaternion r d) omega v dt =
  let -- Derivative components
      dr = qScale 0.5 (omega `qMul` r)
      dv = qScale 0.5 (qAdd (omega `qMul` d) (v `qMul` r))
      -- Integration
      nextR = r `qAdd` qScale dt dr
      nextD = d `qAdd` qScale dt dv
  in projectSE3 $ DualQuaternion nextR nextD

-- =================================
-- DYNAMICS & NEGOTIATION
-- =================================

data AKIState = AKIState 
  { pose :: DualQuaternion, flow :: Quaternion, spin :: Quaternion, effort :: Double }
  deriving (Show, Generic, ToJSON, NFData)

data Attractor = Attractor { targetPose :: DualQuaternion, strength :: Double } 
  deriving (Show, Generic, ToJSON)

data Consensus = Consensus { attractors :: [Attractor], globalBias :: Double } 
  deriving (Show, Generic, ToJSON)

-- | Langevin Engine: Stochastic Jitter for Symmetry Breaking
applyLangevin :: Double -> Quaternion -> Quaternion -> Double -> IO Quaternion
applyLangevin noiseLevel currentSpin gradient dt = do
  nW <- randomRIO (-noiseLevel, noiseLevel)
  nX <- randomRIO (-noiseLevel, noiseLevel)
  nY <- randomRIO (-noiseLevel, noiseLevel)
  nZ <- randomRIO (-noiseLevel, noiseLevel)
  let noise = Quaternion nW nX nY nZ
  -- Damped spin + gradient + noise
  return $ qAdd (qScale 0.9 currentSpin) (qAdd (qScale dt gradient) noise)

-- | Core Negotiation Step
advanceState :: Double -> AKIState -> Consensus -> IO AKIState
advanceState noiseLevel AKIState{..} (Consensus attrs bias) = do
  let dt = 0.01
  -- Force calculation (Sum of attraction to all targets)
  let forceFor (Attractor t s) = qScale s (qAdd (realPart t) (qScale (-1.0) (realPart pose)))
  let gradient = qScale bias (foldl (\acc a -> qAdd acc (forceFor a)) (Quaternion 0 0 0 0) attrs)
  
  -- Update Dynamics
  newSpin <- applyLangevin noiseLevel spin gradient dt
  let newPose = stepPose pose newSpin flow dt
  
  -- Effort Tracking
  let currentEffort = qNorm gradient * dt
  return $ AKIState newPose flow newSpin (effort + currentEffort)

-- =================================
-- EXECUTION: THE JOURNEY TO ORDER
-- =================================

main :: IO ()
main = do
  -- Starting Identity Pose
  let initialPose = DualQuaternion (Quaternion 1 0 0 0) (Quaternion 0 0 0 0)
  -- Initial semantic flow (drift along X)
  let initialFlow = Quaternion 0 0.1 0 0 
  let initialState = AKIState initialPose initialFlow (Quaternion 0 0 0 0) 0.0
  
  -- Symmetric Attractors (Representing conflicting logical states)
  let attr1 = Attractor (DualQuaternion (Quaternion 0.707 0.707 0 0) (Quaternion 0 0 0 0)) 1.0
  let attr2 = Attractor (DualQuaternion (Quaternion 0.707 (-0.707) 0 0) (Quaternion 0 0 0 0)) 1.0
  let consensus = Consensus [attr1, attr2] 1.0

  putStrLn "=== AKI Final Simulation: High-Energy Consensus ==="
  
  -- Running simulation ticks
  finalState <- foldM (\s _ -> advanceState 0.1 s consensus) initialState [1..2000]
  
  putStrLn "Final Negotiated State (JSON):"
  B.putStrLn $ encode finalState
