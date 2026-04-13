#' Analyze Treatment Effects with Mixed Models
#'
#' Wraps the nlme::lme() workflow for grassland experiments with nested random
#' effects (bloc/ID). Runs the model, ANOVA, diagnostic plots, and post-hoc
#' comparisons in a single function call.
#'
#' @param data A data frame containing the variables.
#' @param response Character. Name of the response variable column.
#' @param treatment Character. Name of the treatment factor column. Default "treatment".
#' @param time Character. Name of the time/date factor column. Default "date".
#'   Set to NULL for models without time factor.
#' @param block Character. Name of the block factor column. Default "block".
#' @param id Character. Name of the plot/replicate ID column. Default "ID".
#' @param method Character. Estimation method for lme. Default "ML".
#' @param diagnostics Logical. Generate diagnostic plots? Default TRUE.
#' @param export_json Character. Path to export results as JSON. Default NULL (no export).
#'
#' @return A list with components:
#' \describe{
#'   \item{model}{The fitted lme model object}
#'   \item{anova}{ANOVA table for fixed effects}
#'   \item{emmeans}{Estimated marginal means and pairwise comparisons}
#'   \item{diagnostics}{Diagnostic plot (if requested)}
#' }
#'
#' @examples
#' \dontrun{
#' # Simple usage with auto-detected columns
#' data <- import_urep("data.xlsx")
#' result <- analyze_treatment(data, "RootGrowth")
#'
#' # With JSON export
#' result <- analyze_treatment(data, "RootGrowth", export_json = "results.json")
#' }
#'
#' @export
analyze_treatment <- function(data,
                              response,
                              treatment = "treatment",
                              time = "date",
                              block = "block",
                              id = "ID",
                              method = "ML",
                              diagnostics = TRUE,
                              export_json = NULL) {


  # Build formula

if (!is.null(time)) {
    fixed_formula <- stats::as.formula(
      paste(response, "~", treatment, "*", time)
    )
  } else {
    fixed_formula <- stats::as.formula(
      paste(response, "~", treatment)
    )
  }

  # Random effects formula: ~ 1 | bloc/ID
random_formula <- stats::as.formula(
    paste("~ 1 |", block, "/", id)
  )

  # Fit model
  model <- nlme::lme(
    fixed = fixed_formula,
    random = random_formula,
    data = data,
    method = method
  )

  # ANOVA
  anova_result <- stats::anova(model)

  # Estimated marginal means and pairwise comparisons
  if (!is.null(time)) {
    emm_formula <- stats::as.formula(
      paste("pairwise ~", treatment, "|", time)
    )
  } else {
    emm_formula <- stats::as.formula(
      paste("pairwise ~", treatment)
    )
  }
  emm_result <- emmeans::emmeans(model, emm_formula)

  # Diagnostic plots
  diag_plot <- NULL
  if (diagnostics) {
    diag_plot <- create_diagnostics(model)
  }

  # Build results object
  result <- structure(
    list(
      model = model,
      anova = anova_result,
      emmeans = emm_result,
      diagnostics = diag_plot,
      call = match.call()
    ),
    class = "grassland_analysis"
  )

  # Auto-export to JSON if path provided
  if (!is.null(export_json)) {
    export_json_internal(result, export_json)
    message("Results exported to: ", export_json)
  }

  return(result)
}


#' Internal JSON export (avoids circular dependency)
#' @keywords internal
export_json_internal <- function(x, path) {
  pkg_version <- tryCatch(
    as.character(utils::packageVersion("grasslandDrought")),
    error = function(e) "0.1.0"
  )

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

  json_str <- jsonlite::toJSON(export_list, pretty = TRUE, auto_unbox = TRUE)
  writeLines(json_str, path)
}


#' Create Diagnostic Plots for LME Model
#'
#' @param model An lme model object
#' @return A ggplot object with diagnostic panels
#' @keywords internal
create_diagnostics <- function(model) {

  resid_df <- data.frame(
    fitted = stats::fitted(model),
    residuals = stats::residuals(model)
  )

  p1 <- ggplot2::ggplot(resid_df, ggplot2::aes(x = fitted, y = residuals)) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    ggplot2::labs(x = "Fitted values", y = "Residuals",
                  title = "Residuals vs Fitted") +
    ggplot2::theme_minimal()

  p2 <- ggplot2::ggplot(resid_df, ggplot2::aes(sample = residuals)) +
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line(color = "red") +
    ggplot2::labs(title = "Q-Q Plot") +
    ggplot2::theme_minimal()

  p3 <- ggplot2::ggplot(resid_df, ggplot2::aes(x = residuals)) +
    ggplot2::geom_histogram(bins = 20, fill = "steelblue", color = "white") +
    ggplot2::labs(x = "Residuals", y = "Count", title = "Histogram") +
    ggplot2::theme_minimal()

  # Combine if patchwork available, otherwise return list
  if (requireNamespace("patchwork", quietly = TRUE)) {
    return(p1 + p2 + p3 + patchwork::plot_layout(ncol = 3))
  } else {
    return(list(residuals_vs_fitted = p1, qq_plot = p2, histogram = p3))
  }
}


#' Print Method for grassland_analysis
#'
#' @param x A grassland_analysis object
#' @param ... Additional arguments (ignored)
#' @export
print.grassland_analysis <- function(x, ...) {
  cat("Grassland Drought Analysis\n")
  cat("===========================\n\n")
  cat("Call:\n")
  print(x$call)
  cat("\n--- ANOVA ---\n")
  print(x$anova)
  cat("\n--- Estimated Marginal Means ---\n")
  print(x$emmeans)
  invisible(x)
}
