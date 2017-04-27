`timescale 1ns / 1ns
module source (
	output reg [4:0] F,
	output reg Cout,
  output reg Overflow,
	input [4:0] X,
  input [4:0] Y,
  input [1:0] S
);

reg [5:0] result;
reg [4:0] temp;

always @(S, X, Y) begin
	if(S == 2'b00) begin
		Overflow = 0;
    result = X[3:1] * Y[2:0];
    F = result[4:0];
    Cout = result[5];
	end
	else if(S == 2'b01) begin
		Overflow = 0;
    F = 5'b00000;
    if (X > Y) begin
      Cout = 1;
    end
    else begin
      Cout = 0;
    end
	end
	else if(S == 2'b10) begin
		result = X + Y;
    F = result[4:0];
    Cout = result[5];
    Overflow = X[4] != Y[4] ? 0 : X[4] == F[4] ? 0 : 1;
	end
	else begin
    temp[2:0] = 2'b00;
    temp[4:2] = Y[2:0];
    temp = ~temp + 5'b00001;
    result = X + temp;
    F = result[4:0];
    Cout = result[5];
    Overflow = X[4] != Y[4] ? 0 : X[4] == F[4] ? 0 : 1;
	end
end

endmodule
