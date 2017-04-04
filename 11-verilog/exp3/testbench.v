`timescale 1ns/1ns
module testbench();

reg [0:0] x;
reg [2:0] s;
wire [1:0] y;
wire [2:0] n;

source sc(y, n, x, s);

always begin
    #20 x = x + 1;
    #20 s = s + 1; x = x + 1;
end

initial begin
    $dumpfile("TimingDiagram.vcd");
    $dumpvars(0, y, n, x, s);

    x = 1'b0;
    s = 3'b000;
    #320;

    $finish;
end

endmodule
