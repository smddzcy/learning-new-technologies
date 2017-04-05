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
end

always @(s) begin
	case(s)
		D: y <= 2'b11;
    F: y <= 2'b10;
    H: y <= 2'b01;
		default: y <= 2'b00;
	endcase
end

always @(posedge clk) begin
	if(rst == 1'b1) begin
		s <= A;
	end
	else begin
		if(s == A) begin
			if(x == 1'b0) begin
				s <= A;
			end
			else begin
				s <= B;
			end
		end
		else if(s == B) begin
			if(x == 1'b0) begin
				s <= C;
			end
			else begin
				s <= B;
			end
		end
		else if(s == C) begin
			if(x == 1'b0) begin
				s <= E;
			end
			else begin
				s <= D;
			end
		end
		else if(s == D) begin
			if(x == 1'b0) begin
				s <= C;
			end
			else begin
				s <= B;
			end
		end
		else if(s == E) begin
			if(x == 1'b0) begin
				s <= G;
			end
			else begin
				s <= F;
			end
		end
		else if(s == F) begin
			if(x == 1'b0) begin
				s <= C;
			end
			else begin
				s <= B;
			end
		end
		else if(s == G) begin
			if(x == 1'b0) begin
				s <= G;
			end
			else begin
				s <= H;
			end
		end
    else begin
      if(x == 1'b0) begin
        s <= C;
      end
      else begin
        s <= B;
      end
    end
	end
end

endmodule
