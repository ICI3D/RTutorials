use strict;
use 5.10.0;

## Read answers file and split into paragraphs
## Put first paragraph at top of the batch script
undef $/;
my $ansf = pop @ARGV;
open (ANS, "<$ansf");
my @ans = split /\n{2,}/, <ANS>;

say shift @ans;

undef $/;
my $sf = pop @ARGV;
open (S, "<$sf");
my @s = split /\n{1,}/, <S>;

foreach my $ln (@s){
	next if $ln =~ /#.*CONSOLE/;
	next if $ln =~ /#.*BADCODE/;
	if ($ln =~ /#.*FIXME/ || $ln =~ /#.*FIND/ || $ln =~ /#.*ADDCODE/){
		say "## $ln";
		say shift @ans;
	}
	else {say $ln;}
}
