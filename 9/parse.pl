#!/usr/bin/perl

while(!eof(STDIN)) {
	$line = <STDIN>;
	@words = split ' ', $line;
	print "road(", lc($words[0]), ",", lc($words[2]), ",", $words[4], ").\n";
}
