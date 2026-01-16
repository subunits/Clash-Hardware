def saturation_check():
    # The "Gold Standard" output from your AKI Simulation
    effort_float = 9.567550687692009
    
    # Q1.15 scaling factor (15 bits of fractional precision)
    scale = 32768.0
    
    # Calculate the raw register value
    raw_bits = int(effort_float * scale)
    
    # Simulate the Hardware Saturated Arithmetic (The 16-bit plateau)
    # This prevents the system from 'leaking' into negative values
    plateau_value = min(max(raw_bits, -32768), 32767)
    
    print("--- SE(3) Master Kernel: Saturation Check ---")
    print(f"Floating Point Effort: {effort_float}")
    print(f"Raw Bit Conversion:    {raw_bits}")
    print(f"Hardware Register:     {plateau_value}")
    
    if plateau_value == 32767:
        print("RESULT: SUCCESS. The plateau holds.")
    else:
        print("RESULT: FAILURE. Numerical drift detected.")

if __name__ == "__main__":
    saturation_check()
