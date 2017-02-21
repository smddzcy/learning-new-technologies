`timescale 1ns / 1ns
module source(y, x);

input wire [2:0] x;
input wire [0:0] y;

wire w1, w2, w3, w4, nx2, nx1, nx0;

not (nx2, x[2]);
and P1(w1, nx2, x[1], x[0]);

not (nx1, x[1]);
and P2(w2, x[2], nx1, x[0]);

not (nx0, x[0]);
and P3(w3, x[2], x[1], nx0);

and P4(w4, x[2], x[1], x[0]);

or(y, x4, x3, x2, x1);

endmodule
