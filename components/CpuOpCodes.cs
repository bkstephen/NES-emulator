namespace NES_emulator.components
{
    public interface ICpuOpCodes
    {
        // op codes
        public byte  ADC();	public byte  AND();	public byte  ASL();	public byte  BCC();
        public byte  BCS();	public byte  BEQ();	public byte  BIT();	public byte  BMI();
        public byte  BNE();	public byte  BPL();	public byte  BRK();	public byte  BVC();
        public byte  BVS();	public byte  CLC();	public byte  CLD();	public byte  CLI();
        public byte  CLV();	public byte  CMP();	public byte  CPX();	public byte  CPY();
        public byte  DEC();	public byte  DEX();	public byte  DEY();	public byte  EOR();
        public byte  INC();	public byte  INX();	public byte  INY();	public byte  JMP();
        public byte  JSR();	public byte  LDA();	public byte  LDX();	public byte  LDY();
        public byte  LSR();	public byte  NOP();	public byte  ORA();	public byte  PHA();
        public byte  PHP();	public byte  PLA();	public byte  PLP();	public byte  ROL();
        public byte  ROR();	public byte  RTI();	public byte  RTS();	public byte  SBC();
        public byte  SEC();	public byte  SED();	public byte  SEI();	public byte  STA();
        public byte  STX();	public byte  STY();	public byte  TAX();	public byte  TAY();
        public byte  TSX();	public byte  TXA();	public byte  TXS();	public byte  TYA();

        public byte  XXX();
    }
}