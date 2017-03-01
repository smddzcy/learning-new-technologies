module source(y, x);

input wire [2:0] x;
output wire [0:0] y;

wire w1, nx2;

not (nx2, x[2]);

and P1(w1, nx2, x[1]);
or (y, w1, x[0]);

endmodule
