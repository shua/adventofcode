#!/usr/bin/perl
print "circuit(A,D,E,F,G,H,I,X,Y) :-
    D is 72,
    E is 507,
    F is 492,
    G is 114,
    H is 65412,
    I is 65079,
    X is 123,
    Y is 456";
my $arrow = "->";
my $first = 0;
while(!eof(STDIN)) {
	$line = <STDIN>;
	@words = split ' ', $line;
	if(!$first) {
		print ",\n";
	} else {
		$first = 0;
	}
	print "    ";
	if($words[1] eq $arrow) {
		if($words[0] =~ m/[0-9]+/) {
			print uc($words[2]) . " is " . uc($words[0]) . "";
		} else {
			print uc($words[2]) . " = " . uc($words[0]) . "";
		}
	} elsif ($words[2] eq $arrow) {
		if($words[1] =~ m/[0-9]+/) {
			print uc($words[3]) . " is " . uc($words[1]) . "";
		} else {
			print uc($words[3]) . " = " . uc($words[1]) . "";
		}
	} else {
		if($words[1] eq "AND") {
			print uc($words[4]) . " = ";
			print uc($words[0]) . " /\\ " . uc($words[2]) . "";
		} elsif ($words[1] eq "OR") {
			print uc($words[4]) . " = ";
			print uc($words[0]) . " \\/ " . uc($words[2]) . "";
		} elsif ($words[1] eq "XOR") {
			print uc($words[4]) . " = ";
			print "(" . uc($words[0]) . " \\/ " . uc($words[2]) . ") /\\ \\";
			print "(" . uc($words[0]) . " /\\ " . uc($words[2]) . ") ";
		} elsif ($words[1] eq "RSHIFT") {
			print uc($words[4]) . " = ";
			print uc($words[0]) . " >> " . uc($words[2]) . "";
		} elsif ($words[1] eq "LSHIFT") {
			print uc($words[4]) . " = ";
			print "(" . uc($words[0]) . " << " . uc($words[2]) . ")";
			print " /\\ 65535";
		} else {
			print "UNRECOGNIZED OP $words[1]";
		}
	}
}
print ".\n"
