#' Plot effect estimates and change-in-estimate values (forestplot type)
#'
#' \code{'chest_forest'} plots effect estimates and change-in-estimate values
#' with \code{forestplot} package.
#' @export
#' @import forestplot
#' @param data \emph{Object} from \code{chest_cox}, \code{chest_glm},
#' \code{chest_speedglm}, \code{chest_lm}, \code{chest_clogit}, or \code{chest_nb},
#' including effect estimate values and change-in-estimate values.
#' @param var_lab \emph{Character} string for the column name of variables in the graph.
#' @param est_lab \emph{Character} string for the column name of effect estimates.
#' @param change_lab \emph{Character} string for the column name of "Changes".
#' @param digits Set the display format for number in the graph other than the \emph{"Change"} column. Default: "\%.2f"
#' @param digits_change Set the format for the \emph{"Change"} column. Default: "\%.1f"
#' @param hrzl_lines A \emph{logic} to include or remove horizontal line.
#' @param plus Change the \code{+} sign before variable names.
#' @param ... Further optional arguments for forestplot.
#' @return A table with effect estimates and their changes at all steps.
#' @seealso \pkg{'forestplot'}
#' @examples
#' vlist <- c("Age", "Sex", "Married", "Cancer", "CVD", "Education", "Income")
#' results <- chest_cox(crude = "Surv(t0, t1, Endpoint) ~ Diabetes", xlist = vlist, data = diab_df)
#' chest_forest(results)
#' @name chest_forest
chest_forest <- function(
  data,
  var_lab = "Variables",
  est_lab = "Estimate (95% CI)",
  change_lab = "Change, %",
  digits = "%.2f",
  digits_change = "%.1f",
  hrzl_lines = gpar(col="#444444"),
  plus = "  + ",
  ...) {
  df <- data$data
  out_1 <- tibble::add_row(df, .before=1)
  if (is.null(xlab)) {
    if (data$fun == "chest_cox") {
      xlab <- "Hazard ratio"
    } else if (data$fun == "chest_lm") {
      xlab <- "Coefficient"
    } else if (data$family == "poisson" |
               data$family == "nb") {
      xlab <- "Rate ratio"
    } else if (data$family == "binomial") {
      xlab <- "Odds ratio"
    } else {
      xlab <- "Effect estimates"
    }
  }
  is.summary <- c(TRUE, rep(FALSE, nrow(df)))
  out_2 <- df %>%
    dplyr::transmute(
      variables,
      est_values = paste0(sprintf(digits, est), " (",
                          sprintf(digits, lb), ", ",
                          sprintf(digits, ub), "),  "),
      change = paste0(sprintf(digits_change, Change), "%"))
  var_lab = "Variables"
  est_lab = "OR (95% CI)"
  change_lab = "Change, %"
  out_2 <- tibble::add_row(
    out_2,
    variables = var_lab,
    est_values = est_lab,
    change = change_lab,
    .before = 1)
  out_2$change[2] = ""
  forestplot::forestplot(
    out_2,
    mean = out_1$est,
    lower = out_1$lb,
    upper = out_1$ub,
    is.summary = is.summary,
    hrzl_lines = hrzl_lines)
}

