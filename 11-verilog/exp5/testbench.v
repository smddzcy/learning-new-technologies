`timescale 1ns / 1ns
module testbench();

reg [4:0] X;
reg [4:0] Y;
reg [1:0] S;

wire [4:0] F;
wire Cout;
wire Overflow;

source sc(
	.X(X),
	.Y(Y),
	.S(S),
  .F(F),
  .Cout(Cout),
	.Overflow(Overflow)
);

initial begin
    $dumpfile("TimingDiagram.vcd");
    $dumpvars(0, X, Y, S, F, Cout, Overflow);

    // Check the multiplier functionality
    S = 2'b00;
    X = 5'b01010;
    Y = 5'b00110;
    #40;

    S = 2'b00;
    X = 5'b11110;
    Y = 5'b11111;
    #40;

    // Check the comparator functionality
    S = 2'b01;
    X = 5'b01010;
    Y = 5'b11010;
    #40;

    S = 2'b01;
    X = 5'b11010;
    Y = 5'b01010;
    #40;

    // Check the adder functionality
    S = 2'b10;
    X = 5'b00010;
    Y = 5'b11010;
    #40;

    S = 2'b10;
    X = 5'b01111;
    Y = 5'b01111;
    #40;

    // Check the fourth function (adder and shifter) functionality
    S = 2'b11;
    X = 5'b11010;
    Y = 5'b00010;
    #40;

    S = 2'b11;
    X = 5'b01010;
    Y = 5'b11010;
    #40;

    $finish;
end

endmodule
