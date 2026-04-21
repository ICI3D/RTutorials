use strict;
use 5.10.0;

while(<>){
	chomp;
	my ($old, $new) = split /\t/;
	say "git mv $old $new";
}
