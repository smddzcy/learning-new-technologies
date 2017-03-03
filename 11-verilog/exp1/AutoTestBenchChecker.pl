#!/usr/bin/perl

sub main() {
	my $result = "";
	if ($ARGC < 7) {
		$result = help();
	} else {
		my $testbench = $ARGV[0];
        my $mut = $ARGV[1];
		my $ref = $ARGV[2];
        my $iName = $ARGV[3];
        my $oName = $ARGV[4];
		my $nInputs = $ARGV[5];
		my $nOutputs = $ARGV[6];
		my $inputfile = $ARGV[7];
		
		if ($nInputs !~ /^[0-9]+$/) {
			$result = error('<#inputs> should be a positive integer or zero');
		} elsif ($nOutputs !~ /^[0-9]+$/) {
			$result = error('<#outputs> should be a positive integer');
		} elsif (!(-e "$testbench.v")) {
			$result = error("$testbench.v NOT FOUND!");		
		} elsif (!(-e "$mut.v")) {
			$result = error("$mut.v NOT FOUND!");		
		} elsif (!(-e "$ref.v")) {
			$result = error("$ref.v NOT FOUND!");
        } elsif (!(-e "$inputfile")) {
			$result = error("$inputfile NOT FOUND!");
        } elsif ($nOutputs == 0) {
			$result = error("NO OUTPUTS TO MONITOR!");
		} else {
			$result = check($testbench, $mut, $ref, $iName, $nInputs, $oName, $nOutputs, $inputfile);
		}
	}
	print("$result\n");
}

sub help() {
	return '
Verilog AutoTestBenchChecker, Yavuz Koroglu, Feb 2017

Usage: AutoTestBenchChecker.pl <testbench> <ref> <input-name> <output-name>  <#input-ports> <#output-ports> <input-file>
Example: AutoTestBenchChecker.pl StudentBench ReferenceComponent x y 1

List of Parameters:
-----------------
  <testbench> 
    Test Bench without .v extension.
    
  <mut>
    Module Under Test without .v extension.
	
  <ref>
    REFerence module without .v extension.
    
  <input-name>
    Name of the input variable in <testbench>.v file.

  <output-name>
    Name of the output variable in <testbench>.v file.  

  <#input-ports>
    Number of output ports.

  <#output-ports>
    Number of output ports.
  
  <input-file>
    An inputs.txt file which provides the inputs to the program.
		     
NOTE: All modules must define outputs, inputs and clock in respective order.';
}

sub error() {
	return "ERROR: @_";
}

sub check() {
	my ($testbench, $mut, $ref, $iName, $nInputs, $oName, $nOutputs, $inputfile) = @_;
	my $autoTestBenchName = "$testbench\_$ref\_checkher";
    my $nOutputsMinus1 = $nOutputs - 1;
	
    open(TESTBENCH, "<$testbench.v");
    open(CHECKER, ">$autoTestBenchName.v");
    while ($line = <TESTBENCH>) {
        if ($line =~ /endmodule/) {
            print CHECKER '
// This part is automatically generated.
reg [15:0] _time;
wire ['. $nOutputsMinus1 .':0] _r;
'.$ref.' ref(_r, ' . $iName . ');

initial begin
    _time = 0;
    $dumpvars(0, _r);
    $monitor("time = %dns, ' . $iName . ' = ' . $nInputs . '\'b%b, source = ' . $nOutputs . '\'b%b, REF = ' . $nOutputs . '\'b%b", _time, ' . $iName . ', ' . $oName . ', _r);
end

always #1 _time = _time + 1;
';
        }
        print CHECKER $line;
    }
    close(CHECKER);
    close(TESTBENCH);
	
	my $err = `iverilog -o $autoTestBenchName.vvp $autoTestBenchName.v $ref.v $mut.v`;
	return($err) if ($err ne "");
	my @lines = `vvp $autoTestBenchName.vvp -fst`;
	
	my $nTotal = 0;
    my $lineNo = 0;
    my $nErrors = 0;
	my $report = "";
    open(INPUTS, "<$inputfile");
    my $correctInputs = "?";
	foreach $line (@lines) {
		#$line =~ s/^\s+|\s+$//g;
		
		if($lineNo == 0) {
			return join("", @lines) if ($line !~ /^FST info: dumpfile .+\.vcd opened for output\./);
		} else {
			return join("", @lines) if ($line !~ /time = (.*)ns, $iName = (.*), source = (.*), REF = (.*)/);
			
            if ($1 > 0) {            
                my $currentInputs = $2;
                my $o1 = $3;
                my $o2 = $4;
                if ($currentInputs ne $correctInputs) {
        			$report .= $line;
                    $correctInputs = <INPUTS>;
                    $correctInputs =~ s/\s+//g;
                    $correctInputs = $nInputs . '\'b' . $correctInputs;
                    if ($currentInputs ne $correctInputs) {
                        return "$testbench.v DOES NOT CONFIRM WITH $inputfile\nExpected inputs $correctInputs, got $currentInputs\n$report";
                    }
                }
            
                if ($o1 ne $o2) {
                    $nErrors++;
                }
                $nTotal++;
            }
		}
		
		$lineNo++;
	}
    if (!(eof INPUTS)) {
        $correctInputs = <INPUTS>;
        $correctInputs =~ s/\s+//g;
        $correctInputs = $nInputs . '\'b' . $correctInputs;
        return "$testbench.v DOES NOT CONFIRM WITH $inputfile\nExpected inputs $correctInputs are not there!\n$report";        
    }
    close(INPUTS);
    
    print "ERRORS = $nErrors, TOTAL = $nTotal\n";
	
	return "GRADE = " . ((100 * ($nTotal - $nErrors)) / $nTotal)."\n\n$report";
}

$ARGC = @ARGV; 
main();