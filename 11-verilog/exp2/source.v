`timescale 1ns/1ns
module source(y, x);

input wire [4:0] x;
output wire [0:0] y;

wire nx1, nx0, x1_or_x0, x1_and_x0, nx1_and_nx0;

not(nx1, x[1]);
not(nx0, x[0]);

and(x1_and_x0, x[1], x[0]);
and(nx1_and_nx0, nx1, nx0);

not(x1_or_x0, nx1_and_nx0);

mymux mux(x[4], x[3], x[2], x1_and_x0, nx1_and_nx0, x1_or_x0, x1_and_x0, nx0, nx0, x[0], x[1], y);

endmodule
