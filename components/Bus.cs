using System.Globalization;
using System;
using System.Threading.Tasks;
using System.Linq;

public class Bus
{
    private static int RAM_length_rows = 64;
    private static  int RAM_length_columns = 1024;

    public olc6502 CPU { get; set; } = new olc6502();
    public byte[] RAM { get; set; } = new byte[RAM_length_rows * RAM_length_columns];
    

    public Bus()
    {       
        CleanRAM();
    }
    
    public void Connect()
    {
        CPU.ConnectBus(this);
    }

    public void Write(UInt16 address, byte data)
    {
        if (address >= 0x0000 && address <= 0xFFFF)
        {
            RAM[address] = data;
        }
    }

    public byte Read(UInt16 address, bool bReadOnly = false)
    {
        if (address >= 0x0000 && address <= 0xFFFF)
        {
            return RAM[address];
        }
        else
        {
            return 0x00;
        }
    }

    private void CleanRAM()
    {
        for (var i=0; i < RAM_length_rows * RAM_length_columns; i++)
        {
            RAM[i] = 0x00;            
        }
    }
}