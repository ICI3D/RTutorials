use strict;
use 5.10.0;

## If you do this again, strip the .R somewhere upstream of here.
while(<>){
	chomp;
	my ($old, $new) = split /\t/;
	say "s/$old/$new/g;";
}
