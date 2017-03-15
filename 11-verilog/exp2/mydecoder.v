`timescale 1ns/1ns
module mydecoder(in1, in0, out3, out2, out1, out0);

input in1, in0;
output out3, out2, out1, out0;
reg out3, out2, out1, out0;

always @(in1, in0)
begin
  if (in1 == 0 && in0 == 0)
  begin
    out3 <= 0; out2 <= 0; out1 <= 0; out0 <= 1;
  end
  else if (in1 == 0 && in0 == 1)
  begin
    out3 <= 0; out2 <= 0; out1 <= 1; out0 <= 0;
  end
  else if (in1 == 1 && in0 == 0)
  begin
    out3 <= 0; out2 <= 1; out1 <= 0; out0 <= 0;
  end
  else
  begin
    out3 <= 1; out2 <= 0; out1 <= 0; out0 <= 0;
  end
end
endmodule
