use strict;
use 5.10.0;

while(<>){
	chomp;
	my ($old, $new) = split /\t/;
	$old =~ s/.R$//;
	$new =~ s/.R$//;
	say "git mv $old.answers.R $new.answers.R";
}
