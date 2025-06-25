
.args <- if (interactive()) {
  c("07", "target_07.csv")
} else commandArgs(trailingOnly = TRUE)

tarcsv <- tail(.args, 1)
tarindex <- .args[1] |> as.integer()

df <- data.frame(
  x = 1:10, y = (1:10) * tarindex
)
write.csv(x = df, file = tarcsv, row.names = FALSE)