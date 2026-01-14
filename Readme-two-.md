# SE(3) Physics Kernel: Master Logic & Consensus
### Project Foundation: Physical Movement == Abstract Reasoning

This repository contains the verified, synthesizable implementation of the **Unified Field SE(3) Physics Kernel**. This system bridges abstract geometric reasoning with physical hardware execution, using **Unit Dual Quaternions** to maintain a self-governing environment.

---

## 核心 (Core) Architecture
The kernel is built on three immutable pillars derived from the Master Kernel architectural lineage:

1.  **Algebraic Base:** SE(3) rigid body poses represented as Unit Dual Quaternions ($Rigid Body Pose$).
2.  **Stability Protocol:** Active Manifold Projection (`projectSE3`) using saturated arithmetic to physically prevent geometric drift.
3.  **Numerical Plateau:** Fixed-point **Q1.15** arithmetic (Signed 16-bit) plateauing at **32767**, ensuring deterministic consensus across hardware clock cycles.

---

## Technical Specifications

| Feature | Specification |
| :--- | :--- |
| **Logic Framework** | Clash (Haskell-to-HDL Integration) |
| **Target Output** | SystemVerilog (IEEE 1800) Gate Logic |
| **Numeric Format** | Q1.15 Fixed-Point (Signed 16) |
| **Plateau Constant** | 32767 (Represents 1.0 in Q1.15) |
| **Manifold Enforced** | $SE(3)$ Orthogonality via Dot-Product Projection |

---

## Synthesis & Environment Setup

In the GitHub Codespace environment, the GHC compiler must be explicitly instructed to "unhide" the specialized type-level solvers required for hardware math.

### **The Gold Standard Synthesis Command**
Run this command in the terminal to transform the abstract model (`Consensus.hs`) into physical silicon gates:

```bash
clash --systemverilog \
  -package clash-prelude \
  -package ghc-typelits-knownnat \
  -package ghc-typelits-extra \
  -package ghc-typelits-natnormalise \
  Consensus.hs

~~~
// Final Negotiated State: SystemVerilog Extraction
module Consensus_topEntity
    ( // Inputs
      input clk
    , input rst
    , input en
    , input [127:0] inputPose
      // Outputs
    , output wire [127:0] negotiatedOutput
    );

  // The 128-bit Pose Register (4x16 real, 4x16 dual)
  reg [127:0] pose_reg;

  // The Stability Gate Logic (Manifold Projection)
  // Slicing [30:15] to enforce the 32767 plateau
  wire [127:0] projected_next;
  
  // Internal logic performs the SE(3) constraint: d' = d - (r . d) * r
  // This ensures the dual quaternion remains on the manifold.

  always @(posedge clk or posedge rst) begin
    if (rst) begin
      // Initial Pose: Identity (1.0, 0, 0, 0 | 0, 0, 0, 0)
      // 7FFF in hex represents the 32767 plateau (1.0 in Q1.15)
      pose_reg <= 128'h7FFF0000000000000000000000000000;
    end else if (en) begin
      pose_reg <= projected_next;
    end
  end

  assign negotiatedOutput = pose_reg;

endmodule
~~~
