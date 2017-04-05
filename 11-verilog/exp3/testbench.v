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

    x = 0; rst = 0; #80;
  	x = 0; rst = 0; #30;

    // 101 input
  	x = 1; #20;
  	x = 0; #60;
  	x = 1; #20;

    x = 0; #20;

    // 1001 input
    x = 1; #20;
  	x = 0; #100;
  	x = 1; #20;

    x = 0; #20;

    // 10001 input
    x = 1; #20;
  	x = 0; #140;
  	x = 1; #20;

    x = 0; #20;

    // 1111 input
    x = 1; #140;

    x = 0; #20;

    // 100001 input
    x = 1; #20;
    x = 0; #180;
    x = 1; #20;

    x = 0; #20;

    // 10101 input
    x = 1; #20;
    x = 0; #60;
    x = 1; #20;
    x = 0; #60;
    x = 1; #20;

    x = 0; #20;

    // 101 input with reset
  	x = 1; #20;
  	x = 0; rst = 1; #60;
  	x = 1; #20;
    rst = 0; #20;

    $finish;
end

endmodule
