module testbench();

reg [2:0] x;
wire y;

source s(y, x);

initial begin
    $dumpfile("TimingDiagram.vcd");
    $dumpvars(0, y, x);
    
    x = 3'b000;
    #20 x = 3'b001;
    #20 x = 3'b010;
    #20 x = 3'b011;
    #20 x = 3'b100;
    #20 x = 3'b101;
    #20 x = 3'b110;
    #20 x = 3'b111;
    #20;

    $finish;
end


// This part is automatically generated.
reg [15:0] _time;
wire [0:0] _r;
refMock ref(_r, x);

initial begin
    _time = 0;
    $dumpvars(0, _r);
    $monitor("time = %dns, x = 3'b%b, source = 1'b%b, REF = 1'b%b", _time, x, y, _r);
end

always #1 _time = _time + 1;
endmodule
