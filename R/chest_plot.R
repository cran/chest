#' Plot effect estimate and change-in-estimate values (ggplot type)
#'
#' @export
#' @param data \emph{Object} from \code{chest_cox}, \code{chest_glm},
#' \code{chest_speedglm}, \code{chest_lm}, \code{chest_clogit}, or \code{chest_nb},
#' including effect estimate values and change-in-estimate values.
#' @param no_values Suppress effect estimate values in plot, default is FALSE.
#' @param ylab Add \code{y} label.
#' @param xlab Add \code{x} label.
#' @param change_lab \emph{Character} string for the column name of \emph{"Change"} in the graph
#' @param digits Set the display format for number in the graph other than the \emph{"Change"} column. Default: "\%.2f"
#' @param digits_change Set the format for the \emph{"Change"} column. Default: "\%.1f"
#' @param plus Change leading " +" text.
#' @param nudge_y Adjust vertical distance between values and point marker.
#' @param nudge_x Adjust horizontal distance between values and point marker.
#' @param hjust Adjust horizontal alignment.
#' @param height Change the height of error bars.
#' @param point_size Change point marker size.
#' @param point_shape Change point marker shape.
#' @param vline_type Change vertical line type.
#' @param vline_color Change vertical line color.
#' @param ebar_color Change error bar color.
#' @param plus Change the \code{+} sign before variable names.
#' @param zero x-axis coordinate for vertical non-effect line, see \pkg{forestplot}.
#' @param value_position Change the position of value labels.
#' @param ... Further optional arguments for forestplot.
#' @return a ggplot object.
#' @seealso \pkg{'ggplot2'}
#' @examples
#' vlist<-c("Age", "Sex", "Married", "Cancer", "CVD", "Education", "Income")
#' results <- chest_speedglm(crude = "Endpoint ~ Diabetes",
#'               xlist = vlist, na_omit=TRUE, data = diab_df)
#' chest_plot(results)
#' @name chest_plot
chest_plot <- function(
  data,
  no_values = FALSE,
  ylab = NULL,
  xlab = NULL,
  change_lab = "Change, %",
  digits = "%.2f",
  digits_change = "%.1f",
  plus = "  + ",
  nudge_y = 0.4,
  nudge_x = NULL,
  hjust = 0.5,
  height = 0.06,
  point_size = 3,
  point_shape = 15,
  vline_type = "dashed",
  vline_color = "grey50",
  ebar_color = "grey50",
  zero = 1,
  value_position = NULL,
  ...) {
  df <- data.frame(data$data)
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
  df$variables <- stats::reorder(df$variables, -as.numeric(rownames(df)))
  df <- df %>%
    dplyr::mutate(
      est_values = paste0(sprintf(digits, est), " (",
                  sprintf(digits, lb), ", ",
                  sprintf(digits, ub), "),  ",
                  sprintf(digits_change, Change), "%"))
  if (no_values) {
    df$est_values = " "
  } else {
    df$est_values[1] = paste0(sprintf(digits, df$est[1]), " (",
                      sprintf(digits, df$lb[1]), ", ",
                      sprintf(digits, df$ub[1]), "),  ", change_lab)
  }
  if (is.null(value_position)) {
    df$x_value = df$est
  } else {
    df$x_value = value_position
    hjust = 0
  }
  df %>%
    ggplot(aes(y = variables, x = est)) +
      geom_errorbarh(aes(xmin = lb, xmax = ub),
                  height = height,
                  color = ebar_color) +
      geom_point(size = point_size, shape = point_shape) +
      geom_vline(xintercept = zero,
               linetype=vline_type,
               color = vline_color) +
      theme_classic() +
      geom_text(aes(x = x_value, label = est_values),
                   nudge_y = nudge_y,
                   nudge_x = nudge_x,
                   hjust = hjust) +
      labs(x = xlab, y = ylab)
  }
