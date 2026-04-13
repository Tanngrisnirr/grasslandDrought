# grasslandDrought - Auto-load on project open
# =============================================

message("\n=== grasslandDrought ===\n")

# Load required packages
suppressPackageStartupMessages({
  library(nlme)
  library(emmeans)
  library(readxl)
  library(jsonlite)
  library(ggplot2)
})

# Source package functions
invisible(lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source))

message("Functions loaded: import_urep(), analyze_treatment(), export_json()\n")

# Auto-detect and load data from data/ folder
data_files <- list.files("data", pattern = "\\.xlsx$", full.names = TRUE)

if (length(data_files) == 0) {
  message("No Excel file found in data/ folder.")
  message("Place your .xlsx file there and restart, or use:")
  message("  data <- import_urep('path/to/your/file.xlsx')\n")
} else if (length(data_files) > 1) {
  message("Multiple Excel files found in data/ folder:")
  message(paste(" -", basename(data_files), collapse = "\n"))
  message("\nKeep only one file, or load manually:")
  message("  data <- import_urep('data/yourfile.xlsx')\n")
} else {
  message("Loading: ", basename(data_files[1]), "\n")
  data <- import_urep(data_files[1])
  message("\nData loaded into 'data' variable. Ready to analyze:")
  message("  results <- analyze_treatment(data, 'RootGrowth')")
  message("  results <- analyze_treatment(data, 'RootGrowth', export_json = 'results.json')\n")
}
