The batch paradigm is the idea that we should be able to check changes and compatibility to the R exercises with a pipeline.

The problem is that we don't always want to spoon-feed the students: the students should work for us in batch, but not for the students out of the box.

This is done with magic words in comments.

The project started June 2021 and there are only a few scripts online; these are the ones that have companion .answers.R scripts (possibly empty ones).

The project is implemented through make rules and the perl script [batch.pl](batch.pl)

The currently supported magic words are:

* BADCODE: suppress this line (it has an error that doesn't need to be fixed)
* CONSOLE: suppress this line (it's more of a console than a script line)
* FIXME: fix this line by taking the next *blank-line delimited ¶* from the .answers.R file and replacing it
* [Beginning of script]: gets the first blank-line delimited ¶

In theory, it would be slightly better if the .answers.R files were in a private repo, but I don't see it mattering.
