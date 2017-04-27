`timescale 1ns / 1ns
module source (
	output reg [1:0] y,
	input x,
	input rst,
	input clk
);

parameter A = 3'b000, B = 3'b001, C = 3'b010, D = 3'b011, E = 3'b100,
          F = 3'b101, G = 3'b110, H = 3'b111;

reg [2:0] s;

initial begin
  s = A;
  y = 2'b00;
end

always @(s, x) begin
	case(s)
		C: if(x == 1'b0) begin
         y <= 2'b01;
       end
    D: if(x == 1'b0) begin
         y <= 2'b10;
       end
    E: if(x == 1'b0) begin
         y <= 2'b11;
       end
		default: y <= 2'b00;
	endcase
end

always @(negedge clk) begin
  if(rst == 1'b1) begin
    s = A;
    y = 2'b00;
  end
end

always @(posedge clk) begin
	if(s == A) begin
		if(x == 1'b0) begin
			s <= B;
		end
		else begin
			s <= A;
		end
	end
	else if(s == B) begin
		if(x == 1'b0) begin
			s <= B;
		end
		else begin
			s <= C;
		end
	end
	else if(s == C) begin
		if(x == 1'b0) begin
			s <= B;
		end
		else begin
			s <= D;
		end
	end
	else if(s == D) begin
		if(x == 1'b0) begin
			s <= B;
		end
		else begin
			s <= E;
		end
	end
	else if(s == E) begin
		if(x == 1'b0) begin
			s <= B;
		end
		else begin
			s <= E;
		end
	end
  else begin
    s <= A;
  end
end

endmodule
