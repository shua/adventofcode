#!/usr/bin/perl

print ":- include(\"circuit.pl\").\n";

sub sop {
	"$_[0]($_[1], N, B)";
}

sub dop {
	"$_[0]($_[1], $_[2], N, B)";
}

sub pred {
	"$_[0] :- $_[1].\n"
}

my $arrow = "->";
while(!eof(STDIN)) {
	$line = <STDIN>;
	@words = split ' ', $line;
	if($words[1] eq $arrow) {
		print pred(sop("bit", $words[2]), sop("eql", $words[0]));
	} elsif ($words[2] eq $arrow) {
		print pred(sop("bit", $words[3]), sop("not", $words[1]));
	} else {
		if($words[1] eq "AND") {
			print pred(sop("bit", $words[4]), dop("and", $words[0], $words[2]));
		} elsif ($words[1] eq "OR") {
			print pred(sop("bit", $words[4]), dop("or", $words[0], $words[2]));
		} elsif ($words[1] eq "XOR") {
			print pred(sop("bit", $words[4]), dop("xor", $words[0], $words[2]));
		} elsif ($words[1] eq "RSHIFT") {
			print pred(sop("bit", $words[4]), dop("shft", $words[0], $words[2]));
		} elsif ($words[1] eq "LSHIFT") {
			print pred(sop("bit", $words[4]), dop("shft", $words[0], "-".$words[2]));
		} else {
			print "UNRECOGNIZED OP $words[1]";
		}
	}
}

sub init {
	pred(sop("bit", $_[0]), sop("eql", $_[1]));
}

print init("d", "72");
print init("e", "507");
print init("f", "492");
print init("g", "114");
print init("h", "65412");
print init("i", "65079");
print init("x", "123");
print init("y", "456");

