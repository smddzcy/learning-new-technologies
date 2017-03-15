`timescale 1ns/1ns
module mymux(sel2, sel1, sel0, in7, in6, in5, in4, in3, in2, in1, in0, out0);

input in7, in6, in5, in4, in3, in2, in1, in0;
input sel2, sel1, sel0;
output out0;
reg out0;

always @(in7, in6, in5, in4, in3, in2, in1, in0, sel2, sel1, sel0)
begin
  if ( sel2 == 0 && sel1 == 0 && sel0 == 0 )
  begin
    out0 <= in0;
  end
  else if ( sel2 == 0 && sel1 == 0 && sel0 == 1 )
  begin
    out0 <= in1;
  end
  else if ( sel2 == 0 && sel1 == 1 && sel0 == 0 )
  begin
    out0 <= in2;
  end
  else if ( sel2 == 0 && sel1 == 1 && sel0 == 1 )
  begin
    out0 <= in3;
  end
  else if ( sel2 == 1 && sel1 == 0 && sel0 == 0 )
  begin
    out0 <= in4;
  end
  else if ( sel2 == 1 && sel1 == 0 && sel0 == 1 )
  begin
    out0 <= in5;
  end
  else if ( sel2 == 1 && sel1 == 1 && sel0 == 0 )
  begin
    out0 <= in6;
  end
  else
  begin
    out0 <= in7;
  end
end
endmodule
