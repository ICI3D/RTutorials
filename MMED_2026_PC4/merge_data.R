# merge_data.R
# Merge all CSV simulation outputs in a directory into a single RDS file

# Parse arguments
.args <- if (interactive()) {
  c(".", "merged_data.rds")
} else {
  commandArgs(trailingOnly = TRUE)
}

input_dir <- .args[1]
rds_file <- .args[2]

# Scan the directory for all simulation output CSV files matching target pattern
csv_files <- list.files(
  path = input_dir,
  pattern = "^sim_.*\\.csv$",
  full.names = TRUE
)

# Safety check
if (length(csv_files) == 0) {
  stop(
    "No CSV files matching pattern '^sim_.*\\.csv$' found in directory: ",
    input_dir
  )
}

# Read and combine all found CSV files into one dataframe
df_list <- lapply(csv_files, read.csv)
df <- do.call(rbind, df_list)

# Save consolidated data frame as RDS
saveRDS(df, file = rds_file)
