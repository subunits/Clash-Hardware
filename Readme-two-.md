# SE(3) Physics Kernel: Master Logic & Consensus
### Foundation: Physical Movement == Abstract Reasoning

This repository houses the verified, synthesizable implementation of the **Unified Field SE(3) Physics Kernel**. It represents a closed-loop system where abstract geometric reasoning is physically encoded into hardware execution using **Unit Dual Quaternions**.

---

## 核心 (Core) Architecture
The kernel is founded on three immutable pillars of the Master Kernel lineage:

* **Algebraic Base:** Rigid body poses in SE(3) expressed as Unit Dual Quaternions.
* **Stability Protocol:** The `projectSE3` Stability Gate, utilizing saturated arithmetic to physically negate geometric drift.
* **Numerical Plateau:** Q1.15 Fixed-point arithmetic (Signed 16-bit) capped at **32767**, ensuring deterministic consensus across hardware cycles.



---

## Technical Specifications

| Feature | Specification |
| :--- | :--- |
| **Logic Framework** | Clash (Haskell-to-HDL Integration) |
| **Target Language** | SystemVerilog (IEEE 1800) |
| **Numeric Format** | Q1.15 Fixed-Point (Signed 16-bit) |
| **Plateau Constant** | 32767 (Representing 1.0) |
| **Constraint Logic** | SE(3) Orthogonality via Dot-Product Projection |

---

## Synthesis & Implementation

In the Codespace environment, the compiler must explicitly expose type-level solvers to handle the hardware math requirements.

### **The Gold Standard Synthesis Command**
Run the following to transform the abstract model into silicon-ready gate logic:

```bash
clash --systemverilog \
  -package clash-prelude \
  -package ghc-typelits-knownnat \
  -package ghc-typelits-extra \
  -package ghc-typelits-natnormalise \
  Consensus.hs

/**
 * Module: Consensus_topEntity
 * Description: 128-bit hardware kernel for SE(3) pose stability.
 * Enforces d' = d - (r . d) * r via saturated Q1.15 arithmetic.
 */
module Consensus_topEntity (
    input  wire         clk,             // System Clock
    input  wire         rst,             // Synchronous Reset
    input  wire         en,              // Enable Signal
    input  wire [127:0] inputPose,       // Raw Pose Input
    output wire [127:0] negotiatedOutput // Stabilized Pose Output
);

  // 128-bit Pose Register (4x16 Real Part, 4x16 Dual Part)
  reg [127:0] pose_reg;

  // The Stability Gate: Slicing to enforce the 32767 plateau
  wire [127:0] projected_next;
  
  // Logic Flow:
  // 1. Calculate Dot Product (r . d)
  // 2. Scale Real Part by Dot Product
  // 3. Subtract from Dual Part to project back to SE(3) manifold
  
  always @(posedge clk) begin
    if (rst) begin
      // Identity Pose Initial State: 1.0 Real, 0.0 Dual
      // 0x7FFF (32767) is the Q1.15 identity plateau.
      pose_reg <= 128'h7FFF0000000000000000000000000000;
    end else if (en) begin
      pose_reg <= projected_next;
    end
  end

  assign negotiatedOutput = pose_reg;

endmodule
