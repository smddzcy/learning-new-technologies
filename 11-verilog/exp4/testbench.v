`timescale 1ns / 1ns
module testbench();

reg x;
reg rst;
reg clk;

wire [1:0] y;

source sc(
	.y(y),
	.x(x),
	.rst(rst),
	.clk(clk)
);

always begin
	clk = 1; #20;
	clk = 0; #20;
end

initial begin
    $dumpfile("TimingDiagram.vcd");
    $dumpvars(0, y, x, rst, clk);

    x = 0; rst = 1; #55;
	rst = 0; #32;

    // 010 input
  	x = 0; #40;
  	x = 1; #50;
  	x = 0; #20;

    x = 0; #60;

    // 0110 input
    x = 0; #20;
  	x = 1; #60;
  	x = 0; #20;

    x = 0; #20;

    // 01110 input
    x = 0; #20;
  	x = 1; #100;
  	x = 0; #20;

    x = 0; #20;

    // 01010 input
    x = 0; #10;
    x = 1; #30;
    x = 0; #50;
    x = 1; #30;
    x = 0; #50;

    x = 0; #20;

    // 0110 input with reset
  	x = 0; #20;
  	x = 1; rst = 1; #55;
  	x = 0; #35;
    rst = 0; #30;

    $finish;
end

endmodule
