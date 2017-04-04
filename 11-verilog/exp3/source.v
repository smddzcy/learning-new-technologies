`timescale 1ns/1ns
module source(y, n, x, s);

input wire [0:0] x;
input wire [2:0] s;
output wire [1:0] y;
output wire [2:0] n;

wire s1_and_not_s0, s2_and_not_s0, s2_or_s0, s2_xor_s1, w1, w2, w3, ns0, nx;

not(ns0, s[0]);
not(nx, x);

and(s1_and_not_s0, s[1], ns0);
and(s2_and_not_s0, s[2], ns0);
and(w1, s1_and_not_s0, nx);
or(n[2], s2_and_not_s0, w1);

or(s2_or_s0, s[0], s[2]);
and(w2, nx, s2_or_s0);
and(w3, s1_and_not_s0, x);
or(n[1], w2, w3);

not(n[0], nx);

xor(s2_xor_s1, s[2], s[1]);
and(y[1], s[0], s2_xor_s1);

and(y[0], s[1], s[0]);

endmodule
