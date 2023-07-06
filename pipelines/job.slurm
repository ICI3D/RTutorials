# ...
# -t 1:60
WHICH_PARS = head -n $ARRAY_ID grid.csv | tail -n 1
WHICH_FILE = head -n $ARRAY_ID filenames.csv | tail -n 1

Rscript multistudy.R $WHICH_PARS $WHICH_FILE