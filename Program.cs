using System;
using System.Diagnostics.CodeAnalysis;
using System.Threading.Tasks;

namespace NES_emulator
{
    class Program
    {
        private static Bus nes = new Bus();
        [SuppressMessage("ReSharper.DPA", "DPA0002: Excessive memory allocations in SOH", MessageId = "type: System.String")]
        static Task Main(string[] args)
        {
            nes.Connect();
            while (true)
            {
                nes.CPU.ZP0();
                nes.CPU.ZPX();
                nes.CPU.ZPY();
                nes.CPU.programCounter++;
                Task.Delay(1000);
                
                var test = nes.CPU.Disassemble(0x0000, 0xFFFF);
                foreach (var item in test)
                {
                    Console.WriteLine($"{item.Key}, {item.Value}");
                }
            }
        }
    }
}
