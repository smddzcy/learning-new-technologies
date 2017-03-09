`include "PriorityEncoder.v"
module testbench();

reg [7:0] x;
wire [2:0] y;

PriorityEncoder s(y, x);

always begin
  #20 x = x + 1;
end

initial begin
  $dumpfile("TimingDiagram.vcd");
  $dumpvars(0, y, x);

  x = 8'b00000000;
  #5120;

  $finish;
end

endmodule
