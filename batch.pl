use strict;
use 5.10.0;

undef $/;
my $ansf = pop @ARGV;
open (ANS, "<$ansf");
my @ans = split /\n{2,}/, <ANS>;

undef $/;
my $sf = pop @ARGV;
open (S, "<$sf");
my @s = split /\n{1,}/, <S>;

foreach my $ln (@s){
	next if $ln =~ /#.*CONSOLE/;
	next if $ln =~ /#.*BADCODE/;
	if ($ln =~ /#.*FIXME/){
		say "## $ln";
		say shift @ans;
	}
	else {say $ln;}
}
