#' Export Analysis Results to JSON
#'
#' Exports grassland_analysis results to JSON format, compatible with
#' the thermal-vegetation-monitoring web tool for integrated visualization.
#'
#' @param x A grassland_analysis object from analyze_treatment().
#' @param path Character. Output file path. If NULL, returns JSON string.
#' @param pretty Logical. Pretty-print JSON? Default TRUE.
#' @param include_model Logical. Include full model object? Default FALSE
#'   (model objects are large and not JSON-serializable).
#'
#' @return If path is NULL, returns JSON string. Otherwise writes file
#'   and returns path invisibly.
#'
#' @examples
#' \dontrun{
#' result <- analyze_treatment(data, "RootGrowth")
#' export_json(result, "analysis_results.json")
#' }
#'
#' @export
export_json <- function(x, path = NULL, pretty = TRUE, include_model = FALSE) {

  if (!inherits(x, "grassland_analysis")) {
    stop("x must be a grassland_analysis object")
  }


  # Get version (handle case when package not formally installed)
  pkg_version <- tryCatch(
    as.character(utils::packageVersion("grasslandDrought")),
    error = function(e) "0.1.0"
  )

  # Build export structure
  export_list <- list(
    metadata = list(
      package = "grasslandDrought",
      version = pkg_version,
      timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
      call = deparse(x$call)
    ),
    anova = list(
      table = as.data.frame(x$anova),
      terms = rownames(x$anova)
    ),
    emmeans = list(
      means = as.data.frame(x$emmeans$emmeans),
      contrasts = as.data.frame(x$emmeans$contrasts)
    )
  )

  # Model summary (not full object)
  if (!is.null(x$model)) {
    model_sum <- summary(x$model)
    export_list$model_summary <- list(
      method = x$model$method,
      n_obs = model_sum$dims$N,
      n_groups = model_sum$dims$ngrps,
      logLik = as.numeric(stats::logLik(x$model)),
      AIC = stats::AIC(x$model),
      BIC = stats::BIC(x$model)
    )
  }

  # Convert to JSON
  json_str <- jsonlite::toJSON(export_list, pretty = pretty, auto_unbox = TRUE)

  # Write or return
  if (is.null(path)) {
    return(json_str)
  } else {
    writeLines(json_str, path)
    message("Results exported to: ", path)
    return(invisible(path))
  }
}


#' Export Data Frame to JSON
#'
#' Simple export of a data frame to JSON, useful for passing data
#' to the thermal-vegetation-monitoring web interface.
#'
#' @param data A data frame.
#' @param path Character. Output file path.
#' @param pretty Logical. Pretty-print? Default TRUE.
#'
#' @return Path to written file (invisibly).
#' @export
export_data_json <- function(data, path, pretty = TRUE) {
  json_str <- jsonlite::toJSON(data, pretty = pretty, auto_unbox = TRUE)
  writeLines(json_str, path)
  message("Data exported to: ", path)
  return(invisible(path))
}
