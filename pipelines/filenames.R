
library(data.table)

.args <- commandArgs(trailingOnly = TRUE)

gridfile <- .args[1]

src <- fread(gridfile)
src[, filename := sprintf("%i.rds", 1:.N) ]

fwrite(src[, .(filename)], tail(.args, 1), col.names = FALSE)