#!/usr/bin/perl
open(LIST, "<list.txt") or die "Couldn't read list.txt";

my $num = 0;
while(!eof(LIST)) {
	$line = <LIST>;
	$bad = m/ab|cd|pq|xy/;
	next if($line !~ m/[aeiou].*[aeiou].*[aeiou]/);
	next if($line !~ m/(.)\1{1,}/);
	next if($line =~ m/ab|cd|pq|xy/);
	$num++;
}
print $num, "\n";

seek LIST, 0, 0;
$num = 0;
while(!eof(LIST)) {
	$line = <LIST>;
	next if($line !~ m/(.).\1/);
	next if($line !~ m/(..).*\1/);
	$num++;
}
print $num, "\n";

