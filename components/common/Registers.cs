namespace NES_emulator.components.common
{
    public class Registers
    {
        public byte a { get; set; } = 0x00;
        public byte x { get; set; } = 0x00;
        public byte y { get; set; } = 0x00;
        public byte stackPointer { get; set; } = 0x00;
        public byte programCounter { get; set; } = 0x00;
        public byte status { get; set; } = 0x00;
    }
}