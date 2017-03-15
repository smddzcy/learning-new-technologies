`timescale 1ns/1ns
module testbench();

reg [4:0] x;
wire y;

source s(y, x);

always begin
    #20 x = x + 1;
end

initial begin
    $dumpfile("TimingDiagram.vcd");
    $dumpvars(0, y, x);

    x = 5'b00000;
    #640;

    $finish;
end

endmodule
