namespace NES_emulator.components
{
    public interface ICpuAddressingModes
    {
            // Addressing Modes =============================================
            // The 6502 has a variety of addressing modes to access data in 
            // memory, some of which are direct and some are indirect (like
            // pointers in C++). Each opcode contains information about which
            // addressing mode should be employed to facilitate the 
            // instruction, in regards to where it reads/writes the data it
            // uses.
            public byte IMP();	
            public byte IMM();	
            public byte ZP0();	
            public byte ZPX();	
            public byte ZPY();	
            public byte REL();
            public byte ABS();	
            public byte ABX();	
            public byte ABY();	
            public byte IND();	
            public byte IZX();	
            
            public byte IZY();
    }
}