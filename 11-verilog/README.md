# Verilog

> Verilog, standardized as IEEE 1364, is a hardware description language (HDL) used to model electronic systems. It is most commonly used in the design and verification of digital circuits at the register-transfer level of abstraction.

I'm using [Icarus Verilog](http://iverilog.icarus.com), [Logisim](http://www.cburch.com/logisim) and [GTKWave](http://gtkwave.sourceforge.net/) to write & compile Verilog code and design & visualize digital logic circuits.

- Compile Verilog code: `iverilog -o <filename> <filename>.v`
- Execute the compiled code: `vvp <filename> -fst`
- Visualize using GTKWave (if you've dumped a `.vcd` file in your code): `gtkwave <output>.vcd`

