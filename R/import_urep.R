#' Import UREP Excel Dataset
#'
#' Reads Excel files in the format used by UREP (Unite de Recherche sur
#' l'Ecosysteme Prairial) grassland experiments. Auto-detects common column
#' names for treatment, block, date, and ID.
#'
#' @param path Character. Path to the Excel file.
#' @param sheet Character or integer. Sheet name or index. Default 1.
#'
#' @return A data frame with standardized column names and factor types.
#'
#' @examples
#' \dontrun{
#' data <- import_urep("Dataset_Picon-Cochard et al. 2020_PCI_Ecology.xlsx")
#' }
#'
#' @export
import_urep <- function(path, sheet = 1) {

  if (!file.exists(path)) {
    stop("File not found: ", path)
  }

  # Read Excel
  data <- readxl::read_excel(path, sheet = sheet)
  data <- as.data.frame(data)

  # Auto-detect and standardize column names
  col_names <- tolower(names(data))

  # Treatment column
  treat_patterns <- c("trait", "treatment", "trt", "traitement")
  treat_idx <- which(col_names %in% treat_patterns)[1]
  if (!is.na(treat_idx)) {
    names(data)[treat_idx] <- "treatment"
    data$treatment <- as.factor(data$treatment)
  }

  # Block column
  block_patterns <- c("bloc", "block", "blk", "rep")
  block_idx <- which(col_names %in% block_patterns)[1]
  if (!is.na(block_idx)) {
    names(data)[block_idx] <- "block"
    data$block <- as.factor(data$block)
  }

  # Date column
  date_patterns <- c("date", "time", "sampling", "day")
  date_idx <- which(col_names %in% date_patterns)[1]
  if (!is.na(date_idx)) {
    names(data)[date_idx] <- "date"
    data$date <- as.factor(data$date)
  }

  # ID column (keep as-is if found)
  id_patterns <- c("id", "plot", "subplot", "sample")
  id_idx <- which(col_names %in% id_patterns)[1]
  if (!is.na(id_idx)) {
    names(data)[id_idx] <- "ID"
    data$ID <- as.factor(data$ID)
  }

  # Add class for method dispatch
  class(data) <- c("urep_data", class(data))

  message("Imported ", nrow(data), " rows. Detected columns: ",
          paste(intersect(names(data), c("treatment", "block", "date", "ID")), collapse = ", "))

  return(data)
}


#' Validate UREP Data Structure
#'
#' Checks that a data frame has the required columns and structure
#' for grassland drought analysis.
#'
#' @param data A data frame.
#' @param required_cols Character vector. Required column names.
#' @param warn Logical. Warn about missing optional columns? Default TRUE.
#'
#' @return Logical. TRUE if valid, FALSE otherwise. Issues warnings for problems.
#'
#' @export
validate_urep <- function(data,
                          required_cols = c("trait", "bloc", "ID"),
                          warn = TRUE) {

  valid <- TRUE

  # Check required columns
  missing <- setdiff(required_cols, names(data))
  if (length(missing) > 0) {
    warning("Missing required columns: ", paste(missing, collapse = ", "))
    valid <- FALSE
  }

  # Check for common issues
  if ("trait" %in% names(data)) {
    n_treatments <- length(unique(data$trait))
    if (n_treatments < 2 && warn) {
      warning("Only ", n_treatments, " treatment level(s) found")
    }
  }

  if ("bloc" %in% names(data)) {
    n_blocks <- length(unique(data$bloc))
    if (n_blocks < 2 && warn) {
      warning("Only ", n_blocks, " block(s) found - random effects may be singular")
    }
  }

  return(valid)
}


#' Print Method for urep_data
#'
#' @param x A urep_data object
#' @param ... Additional arguments (ignored)
#' @export
print.urep_data <- function(x, ...) {
  cat("UREP Grassland Dataset\n")
  cat("======================\n")
  cat("Dimensions:", nrow(x), "rows x", ncol(x), "columns\n")

  if ("treatment" %in% names(x)) {
    cat("Treatments:", paste(unique(x$treatment), collapse = ", "), "\n")
  }
  if ("block" %in% names(x)) {
    cat("Blocks:", paste(unique(x$block), collapse = ", "), "\n")
  }
  if ("date" %in% names(x)) {
    cat("Dates:", length(unique(x$date)), "time points\n")
  }

  cat("\nVariables:", paste(names(x), collapse = ", "), "\n")
  invisible(x)
}
