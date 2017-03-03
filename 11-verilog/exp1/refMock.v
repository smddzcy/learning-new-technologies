
// This is the reference mock generated for the student

`timescale 1ns / 1ps
module refMock(outputs, inputs);

output reg [0:0] outputs;
input wire [2:0] inputs;
reg [15:0] inpchange;

initial begin
	inpchange = 0;
	outputs = 0;
end

always @(inputs) begin
	inpchange <= inpchange + 1;
end

always @(inpchange) begin
	if (inpchange == 1) outputs = 0;
	else if (inpchange == 2) outputs = 1;
	else if (inpchange == 3) outputs = 1;
	else if (inpchange == 4) outputs = 1;
	else if (inpchange == 5) outputs = 0;
	else if (inpchange == 6) outputs = 1;
	else if (inpchange == 7) outputs = 0;
	else outputs = 1;
end

endmodule
