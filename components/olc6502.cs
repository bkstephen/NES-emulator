using System.Threading.Tasks;
using System;
using System.Collections.Generic;
using NES_emulator.components;
using NES_emulator.components.common;
using System.Linq;
using System.Numerics;

public abstract class olc6502 : Registers, ICpuOpCodes, ICpuAddressingModes
{    
	public olc6502()
	{
		lookup = new List<INSTRUCTION>
		{
			new INSTRUCTION( "BRK", BRK, IMM, 7 ), new INSTRUCTION( "ORA", ORA, IZX, 6 ), new INSTRUCTION( "???", XXX, IMP, 2 ), new INSTRUCTION( "???", XXX, IMP, 8 ), new INSTRUCTION( "???", NOP, IMP, 3 ), new INSTRUCTION( "ORA", ORA, ZP0, 3 ), new INSTRUCTION( "ASL", ASL, ZP0, 5 ), new INSTRUCTION( "???", XXX, IMP, 5 ), new INSTRUCTION( "PHP", PHP, IMP, 3 ), new INSTRUCTION( "ORA", ORA, IMM, 2 ), new INSTRUCTION( "ASL", ASL, IMP, 2 ), new INSTRUCTION( "???", XXX, IMP, 2 ), new INSTRUCTION( "???", NOP, IMP, 4 ), new INSTRUCTION( "ORA", ORA, ABS, 4 ), new INSTRUCTION( "ASL", ASL, ABS, 6 ), new INSTRUCTION( "???", XXX, IMP, 6 ),
			new INSTRUCTION( "BPL", BPL, REL, 2 ), new INSTRUCTION( "ORA", ORA, IZY, 5 ), new INSTRUCTION( "???", XXX, IMP, 2 ), new INSTRUCTION( "???", XXX, IMP, 8 ), new INSTRUCTION( "???", NOP, IMP, 4 ), new INSTRUCTION( "ORA", ORA, ZPX, 4 ), new INSTRUCTION( "ASL", ASL, ZPX, 6 ), new INSTRUCTION( "???", XXX, IMP, 6 ), new INSTRUCTION( "CLC", CLC, IMP, 2 ), new INSTRUCTION( "ORA", ORA, ABY, 4 ), new INSTRUCTION( "???", NOP, IMP, 2 ), new INSTRUCTION( "???", XXX, IMP, 7 ), new INSTRUCTION( "???", NOP, IMP, 4 ), new INSTRUCTION( "ORA", ORA, ABX, 4 ), new INSTRUCTION( "ASL", ASL, ABX, 7 ), new INSTRUCTION( "???", XXX, IMP, 7 ),
			new INSTRUCTION( "JSR", JSR, ABS, 6 ), new INSTRUCTION( "AND", AND, IZX, 6 ), new INSTRUCTION( "???", XXX, IMP, 2 ), new INSTRUCTION( "???", XXX, IMP, 8 ), new INSTRUCTION( "BIT", BIT, ZP0, 3 ), new INSTRUCTION( "AND", AND, ZP0, 3 ), new INSTRUCTION( "ROL", ROL, ZP0, 5 ), new INSTRUCTION( "???", XXX, IMP, 5 ), new INSTRUCTION( "PLP", PLP, IMP, 4 ), new INSTRUCTION( "AND", AND, IMM, 2 ), new INSTRUCTION( "ROL", ROL, IMP, 2 ), new INSTRUCTION( "???", XXX, IMP, 2 ), new INSTRUCTION( "BIT", BIT, ABS, 4 ), new INSTRUCTION( "AND", AND, ABS, 4 ), new INSTRUCTION( "ROL", ROL, ABS, 6 ), new INSTRUCTION( "???", XXX, IMP, 6 ),
			new INSTRUCTION( "BMI", BMI, REL, 2 ), new INSTRUCTION( "AND", AND, IZY, 5 ), new INSTRUCTION( "???", XXX, IMP, 2 ), new INSTRUCTION( "???", XXX, IMP, 8 ), new INSTRUCTION( "???", NOP, IMP, 4 ), new INSTRUCTION( "AND", AND, ZPX, 4 ), new INSTRUCTION( "ROL", ROL, ZPX, 6 ), new INSTRUCTION( "???", XXX, IMP, 6 ), new INSTRUCTION( "SEC", SEC, IMP, 2 ), new INSTRUCTION( "AND", AND, ABY, 4 ), new INSTRUCTION( "???", NOP, IMP, 2 ), new INSTRUCTION( "???", XXX, IMP, 7 ), new INSTRUCTION( "???", NOP, IMP, 4 ), new INSTRUCTION( "AND", AND, ABX, 4 ), new INSTRUCTION( "ROL", ROL, ABX, 7 ), new INSTRUCTION( "???", XXX, IMP, 7 ),
			new INSTRUCTION( "RTI", RTI, IMP, 6 ), new INSTRUCTION( "EOR", EOR, IZX, 6 ), new INSTRUCTION( "???", XXX, IMP, 2 ), new INSTRUCTION( "???", XXX, IMP, 8 ), new INSTRUCTION( "???", NOP, IMP, 3 ), new INSTRUCTION( "EOR", EOR, ZP0, 3 ), new INSTRUCTION( "LSR", LSR, ZP0, 5 ), new INSTRUCTION( "???", XXX, IMP, 5 ), new INSTRUCTION( "PHA", PHA, IMP, 3 ), new INSTRUCTION( "EOR", EOR, IMM, 2 ), new INSTRUCTION( "LSR", LSR, IMP, 2 ), new INSTRUCTION( "???", XXX, IMP, 2 ), new INSTRUCTION( "JMP", JMP, ABS, 3 ), new INSTRUCTION( "EOR", EOR, ABS, 4 ), new INSTRUCTION( "LSR", LSR, ABS, 6 ), new INSTRUCTION( "???", XXX, IMP, 6 ),
			new INSTRUCTION( "BVC", BVC, REL, 2 ), new INSTRUCTION( "EOR", EOR, IZY, 5 ), new INSTRUCTION( "???", XXX, IMP, 2 ), new INSTRUCTION( "???", XXX, IMP, 8 ), new INSTRUCTION( "???", NOP, IMP, 4 ), new INSTRUCTION( "EOR", EOR, ZPX, 4 ), new INSTRUCTION( "LSR", LSR, ZPX, 6 ), new INSTRUCTION( "???", XXX, IMP, 6 ), new INSTRUCTION( "CLI", CLI, IMP, 2 ), new INSTRUCTION( "EOR", EOR, ABY, 4 ), new INSTRUCTION( "???", NOP, IMP, 2 ), new INSTRUCTION( "???", XXX, IMP, 7 ), new INSTRUCTION( "???", NOP, IMP, 4 ), new INSTRUCTION( "EOR", EOR, ABX, 4 ), new INSTRUCTION( "LSR", LSR, ABX, 7 ), new INSTRUCTION( "???", XXX, IMP, 7 ),
			new INSTRUCTION( "RTS", RTS, IMP, 6 ), new INSTRUCTION( "ADC", ADC, IZX, 6 ), new INSTRUCTION( "???", XXX, IMP, 2 ), new INSTRUCTION( "???", XXX, IMP, 8 ), new INSTRUCTION( "???", NOP, IMP, 3 ), new INSTRUCTION( "ADC", ADC, ZP0, 3 ), new INSTRUCTION( "ROR", ROR, ZP0, 5 ), new INSTRUCTION( "???", XXX, IMP, 5 ), new INSTRUCTION( "PLA", PLA, IMP, 4 ), new INSTRUCTION( "ADC", ADC, IMM, 2 ), new INSTRUCTION( "ROR", ROR, IMP, 2 ), new INSTRUCTION( "???", XXX, IMP, 2 ), new INSTRUCTION( "JMP", JMP, IND, 5 ), new INSTRUCTION( "ADC", ADC, ABS, 4 ), new INSTRUCTION( "ROR", ROR, ABS, 6 ), new INSTRUCTION( "???", XXX, IMP, 6 ),
			new INSTRUCTION( "BVS", BVS, REL, 2 ), new INSTRUCTION( "ADC", ADC, IZY, 5 ), new INSTRUCTION( "???", XXX, IMP, 2 ), new INSTRUCTION( "???", XXX, IMP, 8 ), new INSTRUCTION( "???", NOP, IMP, 4 ), new INSTRUCTION( "ADC", ADC, ZPX, 4 ), new INSTRUCTION( "ROR", ROR, ZPX, 6 ), new INSTRUCTION( "???", XXX, IMP, 6 ), new INSTRUCTION( "SEI", SEI, IMP, 2 ), new INSTRUCTION( "ADC", ADC, ABY, 4 ), new INSTRUCTION( "???", NOP, IMP, 2 ), new INSTRUCTION( "???", XXX, IMP, 7 ), new INSTRUCTION( "???", NOP, IMP, 4 ), new INSTRUCTION( "ADC", ADC, ABX, 4 ), new INSTRUCTION( "ROR", ROR, ABX, 7 ), new INSTRUCTION( "???", XXX, IMP, 7 ),
			new INSTRUCTION( "???", NOP, IMP, 2 ), new INSTRUCTION( "STA", STA, IZX, 6 ), new INSTRUCTION( "???", NOP, IMP, 2 ), new INSTRUCTION( "???", XXX, IMP, 6 ), new INSTRUCTION( "STY", STY, ZP0, 3 ), new INSTRUCTION( "STA", STA, ZP0, 3 ), new INSTRUCTION( "STX", STX, ZP0, 3 ), new INSTRUCTION( "???", XXX, IMP, 3 ), new INSTRUCTION( "DEY", DEY, IMP, 2 ), new INSTRUCTION( "???", NOP, IMP, 2 ), new INSTRUCTION( "TXA", TXA, IMP, 2 ), new INSTRUCTION( "???", XXX, IMP, 2 ), new INSTRUCTION( "STY", STY, ABS, 4 ), new INSTRUCTION( "STA", STA, ABS, 4 ), new INSTRUCTION( "STX", STX, ABS, 4 ), new INSTRUCTION( "???", XXX, IMP, 4 ),
			new INSTRUCTION( "BCC", BCC, REL, 2 ), new INSTRUCTION( "STA", STA, IZY, 6 ), new INSTRUCTION( "???", XXX, IMP, 2 ), new INSTRUCTION( "???", XXX, IMP, 6 ), new INSTRUCTION( "STY", STY, ZPX, 4 ), new INSTRUCTION( "STA", STA, ZPX, 4 ), new INSTRUCTION( "STX", STX, ZPY, 4 ), new INSTRUCTION( "???", XXX, IMP, 4 ), new INSTRUCTION( "TYA", TYA, IMP, 2 ), new INSTRUCTION( "STA", STA, ABY, 5 ), new INSTRUCTION( "TXS", TXS, IMP, 2 ), new INSTRUCTION( "???", XXX, IMP, 5 ), new INSTRUCTION( "???", NOP, IMP, 5 ), new INSTRUCTION( "STA", STA, ABX, 5 ), new INSTRUCTION( "???", XXX, IMP, 5 ), new INSTRUCTION( "???", XXX, IMP, 5 ),
			new INSTRUCTION( "LDY", LDY, IMM, 2 ), new INSTRUCTION( "LDA", LDA, IZX, 6 ), new INSTRUCTION( "LDX", LDX, IMM, 2 ), new INSTRUCTION( "???", XXX, IMP, 6 ), new INSTRUCTION( "LDY", LDY, ZP0, 3 ), new INSTRUCTION( "LDA", LDA, ZP0, 3 ), new INSTRUCTION( "LDX", LDX, ZP0, 3 ), new INSTRUCTION( "???", XXX, IMP, 3 ), new INSTRUCTION( "TAY", TAY, IMP, 2 ), new INSTRUCTION( "LDA", LDA, IMM, 2 ), new INSTRUCTION( "TAX", TAX, IMP, 2 ), new INSTRUCTION( "???", XXX, IMP, 2 ), new INSTRUCTION( "LDY", LDY, ABS, 4 ), new INSTRUCTION( "LDA", LDA, ABS, 4 ), new INSTRUCTION( "LDX", LDX, ABS, 4 ), new INSTRUCTION( "???", XXX, IMP, 4 ),
			new INSTRUCTION( "BCS", BCS, REL, 2 ), new INSTRUCTION( "LDA", LDA, IZY, 5 ), new INSTRUCTION( "???", XXX, IMP, 2 ), new INSTRUCTION( "???", XXX, IMP, 5 ), new INSTRUCTION( "LDY", LDY, ZPX, 4 ), new INSTRUCTION( "LDA", LDA, ZPX, 4 ), new INSTRUCTION( "LDX", LDX, ZPY, 4 ), new INSTRUCTION( "???", XXX, IMP, 4 ), new INSTRUCTION( "CLV", CLV, IMP, 2 ), new INSTRUCTION( "LDA", LDA, ABY, 4 ), new INSTRUCTION( "TSX", TSX, IMP, 2 ), new INSTRUCTION( "???", XXX, IMP, 4 ), new INSTRUCTION( "LDY", LDY, ABX, 4 ), new INSTRUCTION( "LDA", LDA, ABX, 4 ), new INSTRUCTION( "LDX", LDX, ABY, 4 ), new INSTRUCTION( "???", XXX, IMP, 4 ),
			new INSTRUCTION( "CPY", CPY, IMM, 2 ), new INSTRUCTION( "CMP", CMP, IZX, 6 ), new INSTRUCTION( "???", NOP, IMP, 2 ), new INSTRUCTION( "???", XXX, IMP, 8 ), new INSTRUCTION( "CPY", CPY, ZP0, 3 ), new INSTRUCTION( "CMP", CMP, ZP0, 3 ), new INSTRUCTION( "DEC", DEC, ZP0, 5 ), new INSTRUCTION( "???", XXX, IMP, 5 ), new INSTRUCTION( "INY", INY, IMP, 2 ), new INSTRUCTION( "CMP", CMP, IMM, 2 ), new INSTRUCTION( "DEX", DEX, IMP, 2 ), new INSTRUCTION( "???", XXX, IMP, 2 ), new INSTRUCTION( "CPY", CPY, ABS, 4 ), new INSTRUCTION( "CMP", CMP, ABS, 4 ), new INSTRUCTION( "DEC", DEC, ABS, 6 ), new INSTRUCTION( "???", XXX, IMP, 6 ),
			new INSTRUCTION( "BNE", BNE, REL, 2 ), new INSTRUCTION( "CMP", CMP, IZY, 5 ), new INSTRUCTION( "???", XXX, IMP, 2 ), new INSTRUCTION( "???", XXX, IMP, 8 ), new INSTRUCTION( "???", NOP, IMP, 4 ), new INSTRUCTION( "CMP", CMP, ZPX, 4 ), new INSTRUCTION( "DEC", DEC, ZPX, 6 ), new INSTRUCTION( "???", XXX, IMP, 6 ), new INSTRUCTION( "CLD", CLD, IMP, 2 ), new INSTRUCTION( "CMP", CMP, ABY, 4 ), new INSTRUCTION( "NOP", NOP, IMP, 2 ), new INSTRUCTION( "???", XXX, IMP, 7 ), new INSTRUCTION( "???", NOP, IMP, 4 ), new INSTRUCTION( "CMP", CMP, ABX, 4 ), new INSTRUCTION( "DEC", DEC, ABX, 7 ), new INSTRUCTION( "???", XXX, IMP, 7 ),
			new INSTRUCTION( "CPX", CPX, IMM, 2 ), new INSTRUCTION( "SBC", SBC, IZX, 6 ), new INSTRUCTION( "???", NOP, IMP, 2 ), new INSTRUCTION( "???", XXX, IMP, 8 ), new INSTRUCTION( "CPX", CPX, ZP0, 3 ), new INSTRUCTION( "SBC", SBC, ZP0, 3 ), new INSTRUCTION( "INC", INC, ZP0, 5 ), new INSTRUCTION( "???", XXX, IMP, 5 ), new INSTRUCTION( "INX", INX, IMP, 2 ), new INSTRUCTION( "SBC", SBC, IMM, 2 ), new INSTRUCTION( "NOP", NOP, IMP, 2 ), new INSTRUCTION( "???", SBC, IMP, 2 ), new INSTRUCTION( "CPX", CPX, ABS, 4 ), new INSTRUCTION( "SBC", SBC, ABS, 4 ), new INSTRUCTION( "INC", INC, ABS, 6 ), new INSTRUCTION( "???", XXX, IMP, 6 ),
			new INSTRUCTION( "BEQ", BEQ, REL, 2 ), new INSTRUCTION( "SBC", SBC, IZY, 5 ), new INSTRUCTION( "???", XXX, IMP, 2 ), new INSTRUCTION( "???", XXX, IMP, 8 ), new INSTRUCTION( "???", NOP, IMP, 4 ), new INSTRUCTION( "SBC", SBC, ZPX, 4 ), new INSTRUCTION( "INC", INC, ZPX, 6 ), new INSTRUCTION( "???", XXX, IMP, 6 ), new INSTRUCTION( "SED", SED, IMP, 2 ), new INSTRUCTION( "SBC", SBC, ABY, 4 ), new INSTRUCTION( "NOP", NOP, IMP, 2 ), new INSTRUCTION( "???", XXX, IMP, 7 ), new INSTRUCTION( "???", NOP, IMP, 4 ), new INSTRUCTION( "SBC", SBC, ABX, 4 ), new INSTRUCTION( "INC", INC, ABX, 7 ), new INSTRUCTION( "???", XXX, IMP, 7 ),
		};
	}

    public byte Fetched { get; set; } = 0x00;
	public UInt16 addrs_abs { get; set; } = 0x00;
	public UInt16 addrs_rel { get; set; } = 0x00;
	public byte OprogramCounterode { get; set; } = 0x00;
	public byte Cycles { get; set; } = 0x00;
	
    public virtual void ConnectBus(Bus bus) => BUS = bus;

	public virtual Task Clock() {
		if (Cycles == 0){
			OprogramCounterode = Read(programCounter);
			programCounter++;

			// WARNING: This might not work as intended.
			Cycles = lookup[OprogramCounterode].Cycles;

			var additionalCycle1 = lookup[OprogramCounterode].AddressMode();
			var additionalCycle2 = lookup[OprogramCounterode].Operate();

			Cycles += (byte)(additionalCycle1 & additionalCycle1);
		}

		Cycles--;
		return Task.CompletedTask;
	}

	public virtual Task Reset()
	{
		a = 0;
		x = 0;
		y = 0;
		stackPointer = 0xFD;
		status = 0x00 | (byte) FLAGS6502.U;

		addrs_abs = 0xFFFC;
		var lo = Read((ushort)(addrs_abs + 0));
		var hi = Read((ushort)(addrs_abs + 1));

		programCounter = (byte)((hi << 8) | lo);

		addrs_rel = 0x0000;
		addrs_abs = 0x0000;
		Fetched = 0x00;

		Cycles = 8;

		return Task.CompletedTask;
	}

	public virtual Task InteruptRequestSignal()
	{
		if (GetFlag(FLAGS6502.I) == 0)
		{
			Write((byte)(0x0100 + stackPointer), (byte)((programCounter >> 8) & 0x00FF));
			stackPointer--;
			Write((byte)(0x0100 + stackPointer), (byte)(programCounter & 0x00FF));
			stackPointer--;
			
			SetFlag(FLAGS6502.B, false);
			SetFlag(FLAGS6502.U, true);
			SetFlag(FLAGS6502.I, true);
			Write((byte)(0x0100 + stackPointer), status);
			stackPointer--;

			addrs_abs = 0xFFFE;
			var lo = Read((ushort)(addrs_abs + 0));
			var hi = Read((ushort)(addrs_abs + 1));
			programCounter = (byte)((hi << 8) | lo);

			Cycles = 7;
		}	
		
		return Task.CompletedTask;
	}
	
	public virtual Task NonMaskableInteruptRequestSignal()	
	{
		Write((byte)(0x0100 + stackPointer), (byte)((programCounter >> 8) & 0x00FF));
		stackPointer--;
		Write((byte)(0x0100 + stackPointer), (byte)(programCounter & 0x00FF));
		stackPointer--;
			
		SetFlag(FLAGS6502.B, false);
		SetFlag(FLAGS6502.U, true);
		SetFlag(FLAGS6502.I, true);
		Write((byte)(0x0100 + stackPointer), status);
		stackPointer--;

		addrs_abs = 0xFFFA;
		var lo = Read((ushort)(addrs_abs + 0));
		var hi = Read((ushort)(addrs_abs + 1));
		programCounter = (byte)((hi << 8) | lo);

		Cycles = 7;

		return Task.CompletedTask;
	}
	public virtual byte Fetch()
    {
        if (!(lookup[OprogramCounterode].AddressMode == IMP)){
            Fetched = Read(addrs_abs);
        }
        return Fetched;
    }

    public byte AND()
    {
        Fetch();
        a = (byte)(a & Fetched);
        SetFlag(FLAGS6502.Z, a == 0x00);
        SetFlag(FLAGS6502.N, a == 0x80);
        return 1;
    }

	private Bus BUS { get; set; }

    private byte Read(UInt16 address) => BUS.Read(address);    

    private void Write(UInt16 address, byte data) => BUS.Write(address, data);

    private byte GetFlag(FLAGS6502 flag) => status;

    private void SetFlag(FLAGS6502 flag, bool v) {
		if (v)
        {
            status = (byte)(status | (byte)flag);	
        }
        else
        {
            status = (byte)(status & (byte)~flag);
        }
	} 

	public struct INSTRUCTION
	{
		public INSTRUCTION(string name, Func<byte> am, Func<byte> op, byte cy)
		{
			Name = name;
			AddressMode = am;
			Operate = op;
			Cycles = cy;
		}
		public string Name;
		public Func<byte> AddressMode;
		public Func<byte> Operate;
		public byte Cycles;
	}

	public List<INSTRUCTION> lookup;

	[Flags]
    private enum FLAGS6502 : byte
	{
		C = (1 << 0),	// Carry Bit
		Z = (1 << 1),	// Zero
		I = (1 << 2),	// Disable Interrupts
		D = (1 << 3),	// Decimal Mode (unused in this implementation)
		B = (1 << 4),	// Break
		U = (1 << 5),	// Unused
		V = (1 << 6),	// Overflow
		N = (1 << 7),	// Negative
		All = C & Z & I & D & B & U & V & N
	}

    public byte IMP()
    {
        Fetched = a;
        return 0;
    }

    public byte IMM()
    {
        addrs_abs = programCounter++;
        return 0;
    }

    public byte ZP0()
    {
        addrs_abs = Read(programCounter);
        programCounter++;
        addrs_abs &= 0x00FF;
        return 0;
    }

    public byte ZPX()
    {
        addrs_abs = (UInt16)(Read(programCounter) + x);
        programCounter++;
        addrs_abs &= 0x00FF;
        return 0;
    }

    public byte ZPY()
    {
        addrs_abs = (UInt16)(Read(programCounter) + y);
        programCounter++;
        addrs_abs &= 0x00FF;
        return 0;
    }

    public byte REL()
    {
        addrs_rel = Read(programCounter);
        programCounter++;

        if((addrs_rel & 0x80) == 1){
            addrs_rel |= 0xFF00;            
        }

        return 0;
    }

    public byte ABS()
    {
        var lo = Read(programCounter);
        programCounter++;
        var hi = Read(programCounter);
        programCounter++;

        addrs_abs = (UInt16)((hi << 8) | lo);
        return 0;
    }

    public byte ABX()
    {
        var lo = Read(programCounter);
        programCounter++;
        var hi = Read(programCounter);
        programCounter++;

        addrs_abs = (UInt16)((hi << 8) | lo);
        addrs_abs += x;

        if((addrs_abs & 0x00FF) != (hi << 8))
        {
            return 1;
        }

        return 0;
    }

    public byte ABY()
    {
        var lo = Read(programCounter);
        programCounter++;
        var hi = Read(programCounter);
        programCounter++;

        addrs_abs = (UInt16)((hi << 8) | lo);
        addrs_abs += y;

        if((addrs_abs & 0x00FF) != (hi << 8))
        {
            return 1;
        }
        
        return 0;
    }

    public byte IND()
    {
        var ptr_lo = Read(programCounter);
        programCounter++;
        var ptr_hi = Read(programCounter);
        programCounter++;

        var ptr = (ptr_hi << 8) | ptr_lo;

        addrs_abs = (UInt16)((Read((UInt16)(ptr + 1)) << 8) | Read((UInt16)(ptr + 0)));

        if (ptr_lo == 0x00FF){
            addrs_abs = (UInt16)((Read((UInt16)(ptr & 0xFF00)) << 8) | Read((UInt16)(ptr + 0)));
        }
        else{
            addrs_abs = (UInt16)((Read((UInt16)(ptr + 1)) << 8) | Read((UInt16)(ptr + 0)));
        }

        return 0;
    }

    public byte IZX()
    {
        var t = Read(programCounter);
        programCounter++;

        var lo = Read((UInt16)(t + x)) & 0x00FF;
        var hi = Read((UInt16)(t + x + 1)) & 0x00FF;

        addrs_abs = (UInt16)((hi << 8) | lo);

        return 0;
    }

    public byte IZY()
    {
        var t = Read(programCounter);
        programCounter++;

        var lo = Read((UInt16)(t + 0x00FF));
        var hi = Read((UInt16)(t + 1 + 0x00FF));

        addrs_abs = (UInt16)((hi << 8) | lo);
        addrs_abs += y;

        if ((addrs_abs & 0xFF00) != (hi << 8)){
            return 1;
        }

        return 0;
    }

public byte ADC()
{
	// Grab the data that we are adding to the accumulator
	Fetch();
	
	// Add is performed in 16-bit domain for emulation to capture any
	// carry bit, which will exist in bit 8 of the 16-bit word
	var temp = a + Fetched + GetFlag(FLAGS6502.C);
	
	// The carry flag out exists in the high byte bit 0
	SetFlag(FLAGS6502.C, temp > 255);
	
	// The Zero flag is set if the result is 0
	SetFlag(FLAGS6502.Z, (temp & 0x00FF) == 0);
	
	// The signed Overflow flag is set based on all that up there! :D
	SetFlag(FLAGS6502.V, ((~(a ^ Fetched) & (a ^ (UInt16)temp)) & 0x0080) == 1);
	
	// The negative flag is set to the most significant bit of the result
	SetFlag(FLAGS6502.N, (temp & 0x80) == 1);
	
	// Load the result into the accumulator (it's 8-bit dont forget!)
	a = (byte)(temp & 0x00FF);
	
	// This instruction has the potential to require an additional clock cycle
	return 1;
}

public byte SBC()
{
	Fetch();
	
	// Operating in 16-bit domain to capture carry out
	
	// We can invert the bottom 8 bits with bitwise xor
	UInt16 value = (UInt16)(((UInt16)Fetched) ^ 0x00FF);
	
	// Notice this is exactly the same as addition from here!
	var temp = (UInt16)a + value + (UInt16)GetFlag(FLAGS6502.C);
	SetFlag(FLAGS6502.C, (temp & 0xFF00) == 1);
	SetFlag(FLAGS6502.Z, ((temp & 0x00FF) == 0));
	SetFlag(FLAGS6502.V, ((temp ^ (UInt16)a) & (temp ^ value) & 0x0080) == 1);
	SetFlag(FLAGS6502.N, (temp & 0x0080) == 1);
	a = (byte)(temp & 0x00FF);
	return 1;
}

public byte ASL()
{
	Fetch();
	var temp = (UInt16)Fetched << 1;
	SetFlag(FLAGS6502.C, (temp & 0xFF00) > 0);
	SetFlag(FLAGS6502.Z, (temp & 0x00FF) == 0x00);
	SetFlag(FLAGS6502.N, (temp & 0x80) == 1);
	if (lookup[OprogramCounterode].AddressMode == IMP)
		a = (byte)(temp & 0x00FF);
	else
		Write(addrs_abs, (byte)(temp & 0x00FF));
	return 0;
}

public byte BCC()
{
	if (GetFlag(FLAGS6502.C) == 0)
	{
		Cycles++;
		addrs_abs = (ushort)(programCounter + addrs_rel);
		
		if((addrs_abs & 0xFF00) != (programCounter & 0xFF00))
			Cycles++;
		
		programCounter = (byte)addrs_abs;
	}
	return 0;
}

public byte BCS()
{
	if (GetFlag(FLAGS6502.C) == 1)
	{
		Cycles++;
		addrs_abs = (ushort)(programCounter + addrs_rel);

		if ((addrs_abs & 0xFF00) != (programCounter & 0xFF00))
			Cycles++;

		programCounter = (byte)addrs_abs;
	}
	return 0;
}

public byte BEQ()
{
	if (GetFlag(FLAGS6502.Z) == 1)
	{
		Cycles++;
		addrs_abs =(ushort)(programCounter + addrs_rel);

		if ((addrs_abs & 0xFF00) != (programCounter & 0xFF00))
			Cycles++;

		programCounter = (byte)addrs_abs;
	}
	return 0;
}

public byte BIT()
{
	Fetch();
	var temp = a & Fetched;
	SetFlag(FLAGS6502.Z, (temp & 0x00FF) == 0x00);
	SetFlag(FLAGS6502.N, (Fetched & (1 << 7)) == 1);
	SetFlag(FLAGS6502.V, (Fetched & (1 << 6)) == 1);
	return 0;
}

public byte BMI()
{
	if (GetFlag(FLAGS6502.N) == 1)
	{
		Cycles++;
		addrs_abs = (ushort)(programCounter + addrs_rel);

		if ((addrs_abs & 0xFF00) != (programCounter & 0xFF00))
			Cycles++;

		programCounter = (byte)addrs_abs;
	}
	return 0;
}

public byte BNE()
{
	if (GetFlag(FLAGS6502.Z) == 0)
	{
		Cycles++;
		addrs_abs = (ushort)(programCounter + addrs_rel);

		if ((addrs_abs & 0xFF00) != (programCounter & 0xFF00))
			Cycles++;

		programCounter = (byte)addrs_abs;
	}
	return 0;
}

public byte BPL()
{
	if (GetFlag(FLAGS6502.N) == 0)
	{
		Cycles++;
		addrs_abs = (ushort)(programCounter + addrs_rel);

		if ((addrs_abs & 0xFF00) != (programCounter & 0xFF00))
			Cycles++;

		programCounter = (byte)addrs_abs;
	}
	return 0;
}

public byte BRK()
{
	programCounter++;
	
	SetFlag(FLAGS6502.I, true);
	Write((byte)(0x0100 + stackPointer), (byte)((programCounter >> 8) & 0x00FF));
	stackPointer--;
	Write((byte)(0x0100 + stackPointer), (byte)(programCounter & 0x00FF));
	stackPointer--;

	SetFlag(FLAGS6502.B, true);
	Write((byte)(0x0100 + stackPointer), status);
	stackPointer--;
	SetFlag(FLAGS6502.B, false);

	programCounter = (byte)((UInt16)Read(0xFFFE) | ((UInt16)Read(0xFFFF) << 8));
	return 0;
}

public byte BVC()
{
	if (GetFlag(FLAGS6502.V) == 0)
	{
		Cycles++;
		addrs_abs = (ushort)(programCounter + addrs_rel);

		if ((addrs_abs & 0xFF00) != (programCounter & 0xFF00))
			Cycles++;

		programCounter = (byte)addrs_abs;
	}
	return 0;
}

public byte BVS()
{
	if (GetFlag(FLAGS6502.V) == 1)
	{
		Cycles++;
		addrs_abs = (ushort)(programCounter + addrs_rel);

		if ((addrs_abs & 0xFF00) != (programCounter & 0xFF00))
			Cycles++;

		programCounter = (byte)addrs_abs;
	}
	return 0;
}


public byte CLC()
{
	SetFlag(FLAGS6502.C, false);
	return 0;
}


public byte CLD()
{
	SetFlag(FLAGS6502.D, false);
	return 0;
}


// Instruction: Disable Interrupts / Clear Interrupt Flag
// Function:    I = 0
public byte CLI()
{
	SetFlag(FLAGS6502.I, false);
	return 0;
}


// Instruction: Clear Overflow Flag
// Function:    V = 0
public byte CLV()
{
	SetFlag(FLAGS6502.V, false);
	return 0;
}

// Instruction: Compare Accumulator
// Function:    C <- A >= M      Z <- (A - M) == 0
// Flags Out:   N, C, Z
public byte CMP()
{
	Fetch();
	var temp = (UInt16)a - (UInt16)Fetched;
	SetFlag(FLAGS6502.C, a >= Fetched);
	SetFlag(FLAGS6502.Z, (temp & 0x00FF) == 0x0000);
	SetFlag(FLAGS6502.N, (temp & 0x0080) == 1);
	return 1;
}


// Instruction: Compare X Register
// Function:    C <- X >= M      Z <- (X - M) == 0
// Flags Out:   N, C, Z
public byte CPX()
{
	Fetch();
	var temp = (UInt16)x - (UInt16)Fetched;
	SetFlag(FLAGS6502.C, x >= Fetched);
	SetFlag(FLAGS6502.Z, (temp & 0x00FF) == 0x0000);
	SetFlag(FLAGS6502.N, (temp & 0x0080) == 1);
	return 0;
}


// Instruction: Compare Y Register
// Function:    C <- Y >= M      Z <- (Y - M) == 0
// Flags Out:   N, C, Z
public byte CPY()
{
	Fetch();
	var temp = (UInt16)y - (UInt16)Fetched;
	SetFlag(FLAGS6502.C, y >= Fetched);
	SetFlag(FLAGS6502.Z, (temp & 0x00FF) == 0x0000);
	SetFlag(FLAGS6502.N, (temp & 0x0080) == 1);
	return 0;
}


// Instruction: Decrement Value at Memory Location
// Function:    M = M - 1
// Flags Out:   N, Z
public byte DEC()
{
	Fetch();
	var temp = Fetched - 1;
	Write(addrs_abs, (byte)(temp & 0x00FF));
	SetFlag(FLAGS6502.Z, (temp & 0x00FF) == 0x0000);
	SetFlag(FLAGS6502.N, (temp & 0x0080) == 1);
	return 0;
}


// Instruction: Decrement X Register
// Function:    X = X - 1
// Flags Out:   N, Z
public byte DEX()
{
	x--;
	SetFlag(FLAGS6502.Z, x == 0x00);
	SetFlag(FLAGS6502.N, (x & 0x80) == 1);
	return 0;
}


// Instruction: Decrement Y Register
// Function:    Y = Y - 1
// Flags Out:   N, Z
public byte DEY()
{
	y--;
	SetFlag(FLAGS6502.Z, y == 0x00);
	SetFlag(FLAGS6502.N, (y & 0x80) == 1);
	return 0;
}


// Instruction: Bitwise Logic XOR
// Function:    A = A xor M
// Flags Out:   N, Z
public byte EOR()
{
	Fetch();
	a = (byte)(a ^ Fetched);	
	SetFlag(FLAGS6502.Z, a == 0x00);
	SetFlag(FLAGS6502.N, (a & 0x80) == 1);
	return 1;
}


// Instruction: Increment Value at Memory Location
// Function:    M = M + 1
// Flags Out:   N, Z
public byte INC()
{
	Fetch();
	var temp = Fetched + 1;
	Write(addrs_abs, (byte)(temp & 0x00FF));
	SetFlag(FLAGS6502.Z, (temp & 0x00FF) == 0x0000);
	SetFlag(FLAGS6502.N, (temp & 0x0080) == 1);
	return 0;
}


// Instruction: Increment X Register
// Function:    X = X + 1
// Flags Out:   N, Z
public byte INX()
{
	x++;
	SetFlag(FLAGS6502.Z, x == 0x00);
	SetFlag(FLAGS6502.N, (x & 0x80) == 1);
	return 0;
}


// Instruction: Increment Y Register
// Function:    Y = Y + 1
// Flags Out:   N, Z
public byte INY()
{
	y++;
	SetFlag(FLAGS6502.Z, y == 0x00);
	SetFlag(FLAGS6502.N, (y & 0x80) == 1);
	return 0;
}


// Instruction: Jump To Location
// Function:    programCounter = address
public byte JMP()
{
	programCounter = (byte)addrs_abs;
	return 0;
}


// Instruction: Jump To Sub-Routine
// Function:    Push current programCounter to stack, programCounter = address
public byte JSR()
{
	programCounter--;

	Write((ushort)(0x0100 + stackPointer), (byte)((programCounter >> 8) & 0x00FF));
	stackPointer--;
	Write((ushort)(0x0100 + stackPointer), (byte)(programCounter & 0x00FF));
	stackPointer--;

	programCounter = (byte)addrs_abs;
	return 0;
}


// Instruction: Load The Accumulator
// Function:    A = M
// Flags Out:   N, Z
public byte LDA()
{
	Fetch();
	a = Fetched;
	SetFlag(FLAGS6502.Z, a == 0x00);
	SetFlag(FLAGS6502.N, (a & 0x80) == 1);
	return 1;
}


// Instruction: Load The X Register
// Function:    X = M
// Flags Out:   N, Z
public byte LDX()
{
	Fetch();
	x = Fetched;
	SetFlag(FLAGS6502.Z, x == 0x00);
	SetFlag(FLAGS6502.N, (x & 0x80) == 1);
	return 1;
}


// Instruction: Load The Y Register
// Function:    Y = M
// Flags Out:   N, Z
public byte LDY()
{
	Fetch();
	y = Fetched;
	SetFlag(FLAGS6502.Z, y == 0x00);
	SetFlag(FLAGS6502.N, (y & 0x80) == 1);
	return 1;
}

public byte LSR()
{
	Fetch();
	SetFlag(FLAGS6502.C, (Fetched & 0x0001) == 1);
	var temp = Fetched >> 1;	
	SetFlag(FLAGS6502.Z, (temp & 0x00FF) == 0x0000);
	SetFlag(FLAGS6502.N, (temp & 0x0080) == 1);
	if (lookup[OprogramCounterode].AddressMode == IMP)
		a = (byte)(temp & 0x00FF);
	else
		Write(addrs_abs, (byte)(temp & 0x00FF));
	return 0;
}

public byte NOP()
{
	switch (OprogramCounterode) {
	case 0x1C:
	case 0x3C:
	case 0x5C:
	case 0x7C:
	case 0xDC:
	case 0xFC:
		return 1;
		break;
	}
	return 0;
}


// Instruction: Bitwise Logic OR
// Function:    A = A | M
// Flags Out:   N, Z
public byte ORA()
{
	Fetch();
	a = (byte)(a | Fetched);
	SetFlag(FLAGS6502.Z, a == 0x00);
	SetFlag(FLAGS6502.N, (a & 0x80) == 1);
	return 1;
}


// Instruction: Push Accumulator to Stack
// Function:    A -> stack
public byte PHA()
{
	Write((ushort)(0x0100 + stackPointer), a);
	stackPointer--;
	return 0;
}


// Instruction: Push Status Register to Stack
// Function:    status -> stack
// Note:        Break flag is set to 1 before push
public byte PHP()
{
	Write((ushort)(0x0100 + stackPointer), (byte)(status | (byte)FLAGS6502.B | (byte)FLAGS6502.U));
	SetFlag(FLAGS6502.B, false);
	SetFlag(FLAGS6502.U, false);
	stackPointer--;
	return 0;
}


// Instruction: Pop Accumulator off Stack
// Function:    A <- stack
// Flags Out:   N, Z
public byte PLA()
{
	stackPointer++;
	a = Read((ushort)(0x0100 + stackPointer));
	SetFlag(FLAGS6502.Z, a == 0x00);
	SetFlag(FLAGS6502.N, (a & 0x80) == 1);
	return 0;
}


// Instruction: Pop Status Register off Stack
// Function:    Status <- stack
public byte PLP()
{
	stackPointer++;
	status = Read((ushort)(0x0100 + stackPointer));
	SetFlag(FLAGS6502.U, true);
	return 0;
}

public byte ROL()
{
	Fetch();
	var temp = (UInt16)(Fetched << 1) | GetFlag(FLAGS6502.C);
	SetFlag(FLAGS6502.C, (temp & 0xFF00) == 1);
	SetFlag(FLAGS6502.Z, (temp & 0x00FF) == 0x0000);
	SetFlag(FLAGS6502.N, (temp & 0x0080) == 1);
	if (lookup[OprogramCounterode].AddressMode == IMP)
		a = (byte)(temp & 0x00FF);
	else
		Write(addrs_abs, (byte)(temp & 0x00FF));
	return 0;
}

public byte ROR()
{
	Fetch();
	var temp = (UInt16)(GetFlag(FLAGS6502.C) << 7) | (Fetched >> 1);
	SetFlag(FLAGS6502.C, (Fetched & 0x01) == 1);
	SetFlag(FLAGS6502.Z, (temp & 0x00FF) == 0x00);
	SetFlag(FLAGS6502.N, (temp & 0x0080) == 1);
	if (lookup[OprogramCounterode].AddressMode == IMP)
		a = (byte)(temp & 0x00FF);
	else
		Write(addrs_abs, (byte)(temp & 0x00FF));
	return 0;
}

public byte RTI()
{
	stackPointer++;
	status = Read((ushort)(0x0100 + stackPointer));
	status &= (byte)~FLAGS6502.B;
	status &= (byte)~FLAGS6502.U;

	stackPointer++;
	programCounter = (byte)Read((ushort)(0x0100 + stackPointer));
	stackPointer++;
	programCounter |= (byte)(Read((ushort)(0x0100 + stackPointer)) << 8);
	return 0;
}

public byte RTS()
{
	stackPointer++;
	programCounter = (byte)Read((ushort)(0x0100 + stackPointer));
	stackPointer++;
	programCounter |= (byte)(Read((ushort)(0x0100 + stackPointer)) << 8);
	
	programCounter++;
	return 0;
}

// Instruction: Set Carry Flag
// Function:    C = 1
public byte SEC()
{
	SetFlag(FLAGS6502.C, true);
	return 0;
}


// Instruction: Set Decimal Flag
// Function:    D = 1
public byte SED()
{
	SetFlag(FLAGS6502.D, true);
	return 0;
}


// Instruction: Set Interrupt Flag / Enable Interrupts
// Function:    I = 1
public byte SEI()
{
	SetFlag(FLAGS6502.I, true);
	return 0;
}


// Instruction: Store Accumulator at Address
// Function:    M = A
public byte STA()
{
	Write(addrs_abs, a);
	return 0;
}


// Instruction: Store X Register at Address
// Function:    M = X
public byte STX()
{
	Write(addrs_abs, x);
	return 0;
}


// Instruction: Store Y Register at Address
// Function:    M = Y
public byte STY()
{
	Write(addrs_abs, y);
	return 0;
}


// Instruction: Transfer Accumulator to X Register
// Function:    X = A
// Flags Out:   N, Z
public byte TAX()
{
	x = a;
	SetFlag(FLAGS6502.Z, x == 0x00);
	SetFlag(FLAGS6502.N, (x & 0x80)==1);
	return 0;
}


// Instruction: Transfer Accumulator to Y Register
// Function:    Y = A
// Flags Out:   N, Z
public byte TAY()
{
	y = a;
	SetFlag(FLAGS6502.Z, y == 0x00);
	SetFlag(FLAGS6502.N, (y & 0x80)==1);
	return 0;
}


// Instruction: Transfer Stack Pointer to X Register
// Function:    X = stack pointer
// Flags Out:   N, Z
public byte TSX()
{
	x = stackPointer;
	SetFlag(FLAGS6502.Z, x == 0x00);
	SetFlag(FLAGS6502.N, (x & 0x80) == 1);
	return 0;
}


// Instruction: Transfer X Register to Accumulator
// Function:    A = X
// Flags Out:   N, Z
public byte TXA()
{
	a = x;
	SetFlag(FLAGS6502.Z, a == 0x00);
	SetFlag(FLAGS6502.N, (a & 0x80) == 1);
	return 0;
}


// Instruction: Transfer X Register to Stack Pointer
// Function:    stack pointer = X
public byte TXS()
{
	stackPointer = x;
	return 0;
}


// Instruction: Transfer Y Register to Accumulator
// Function:    A = Y
// Flags Out:   N, Z
public byte TYA()
{
	a = y;
	SetFlag(FLAGS6502.Z, a == 0x00);
	SetFlag(FLAGS6502.N, (a & 0x80) == 1);
	return 0;
}

    public byte XXX()
    {
        return 0x00;
    }
}