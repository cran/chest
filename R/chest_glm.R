#' Assessing confounding effects using Generalized Linear regression models
#'
#' Please note: There is a faster option: \code{'chest_speedglm'}. \code{'chest_glm'}
#' is used to assess confounding effects using Generalized Linear Models, such as
#' logistic regression and Poisson regression with \code{'glm'}.
#' It presents \emph{odds ratios} or \emph{rate ratios} for the association
#' between exposure and outcome variables by adding other variables (potential
#' confounders) to the model sequentially. The order of variables to be added is
#' based on the magnitudes of the changes in effect estimates.
#'
#' @export
#' @param crude An object of \emph{formula} for initial model, generally crude model.
#' However, any other variables can also be included here as the initial model.
#' @param xlist A \emph{vector} of characters with all variable names of potential confounders.
#' @param data \emph{Data frame}.
#' @param var_lab \emph{Character} string for the column name of variables in the graph.
#' @param est_lab \emph{Character} string for the column name of effect estimates.
#' @param change_lab \emph{Character} string for the column name of "Changes".
#' @param digits Set the display format for number in the graph other than the \emph{"Change"} column. Default: "\%.2f"
#' @param digits_change Set the format for the \emph{"Change"} column. Default: "\%.1f"
#' @param family Description of the error distribution. Default is "binomial".
#' @param method Method to detect for singularity.
#' @param maxit See \code{'glm'}.
#' @param epsilon See \code{'glm'}.
#' @param na_omit Remove all missing values.
#' @param hrzl_lines A \emph{logic} to include or remove horizontal line.
#' @param text_size Set text size.
#' @param xlab_size Set \code{x} label size.
#' @param xtick_size Set \code{x} tick label size.
#' @param xlab Add \code{x} label.
#' @param plus Change the \code{+} sign before variable names.
#' @param indicate indicate progress
#' @param ... Further optional arguments for forestplot.
#' @return A table with effect estimates and their changes at all steps.
#' @seealso \pkg{'forestplot'}
#' @seealso \code{'glm'} \pkg{'stats'}
#'
#' for effects estimates from all possible models.
#' @examples
#'
#' chest_glm(crude = "Endpoint ~ Diabetes", xlist = c("Age", "Sex", "Married"),
#' na_omit=TRUE, data = diab_df)
#'
#' @name chest_glm
#'
chest_glm <- function(
  crude, xlist, data,
  var_lab = "Variables",
  est_lab = "OR (95% CI)",
  change_lab = "Change, %",
  digits = "%.2f",
  digits_change = "%.1f",
  family = "binomial",
  method = "glm.fit",
  maxit = 25,
  epsilon = 1e-8,
  na_omit = FALSE,
  hrzl_lines = gpar(col="#444444"),
  text_size = 1,
  xlab_size = 1,
  xtick_size = 1,
  xlab = NULL,
  indicate = FALSE,
  plus = "  + ",
  ...) {
  pick <- variables <- OR <- se <- Change <- p <- lb <- ub <- n<- c()
  n_xlist <- length(xlist)
  data <- data.frame(c(data[all.vars(as.formula(crude))], data[xlist]))
  if (na_omit) {data <- na.omit(data)}
  mod_crude <- glm(
    as.formula(crude),
    family = family,
    method = method,
    control = list(maxit = maxit, epsilon = epsilon),
    data = data)
  mod0 <- broom::tidy(
    mod_crude,
    exponentiate = TRUE,
    conf.int = TRUE)
  n <- broom::glance(mod_crude)$df.null
  OR[1] = mod0$estimate[2]
  p[1] = mod0$p.value[2]
  lb[1] = mod0$conf.low[2]
  ub[1] = mod0$conf.high[2]
  variables[1] <- c("Crude")
  initial_model <- crude
  for (i in 2:(n_xlist+1)) {
    mod <- glm(as.formula(crude),
               family = family, method = method,
               control = list(
                 maxit = maxit,
                 epsilon = epsilon),
               data = data)
    hr_0 <- broom::tidy(mod, exponentiate = TRUE)$estimate[2]
    models <- lapply(xlist, function(x)
                        update(mod, as.formula(paste0(". ~ . +", x))))
    hr_1 <- unlist(lapply(models, function(x)
      broom::tidy(x, exponentiate = TRUE)$estimate[2]))
    p_1 <- unlist(lapply(models, function(x)
      broom::tidy(x)$p.value[2]))
    chg <- (hr_1-hr_0)*100/hr_0
    lb_1 <- unlist(lapply(models, function(x)
      broom::tidy(x, exponentiate = TRUE,
                  conf.int = T)$conf.low[2]))
    ub_1  <- unlist(lapply(models, function(x)
      broom::tidy(x, exponentiate = TRUE,
                  conf.int = T)$conf.high[2]))
    n_1 <- unlist(lapply(models, function(x)
      broom::glance(x, exponentiate = TRUE)$df.null))
    pick[i] <- xlist[which.max(abs(chg))]
    xlist <- xlist[-which(xlist %in% paste0(pick[i]))]
    crude <- paste0(crude, "+", paste0(pick[i]), collapse = " + ")
    variables[i] = paste0(plus, pick[i])
    OR[i] = hr_1[which.max(abs(chg))]
    lb[i] = lb_1[which.max(abs(chg))]
    ub[i] = ub_1[which.max(abs(chg))]
    Change[i] = chg[which.max(abs(chg))]
    p[i] = p_1[which.max(abs(chg))]
    n[i] = n_1[which.max(abs(chg))]
    cat("\r",i, "out of", n_xlist + 1)
  }
  est_1 <- paste0(sprintf(digits, OR), " (",
                  sprintf(digits, lb), ", ",
                  sprintf(digits, ub), ") ")
  change1 <- paste0(sprintf(digits_change, Change))
  change1 <- sub("NA", " ", change1)
  out   <- data.frame(variables, OR, lb, ub, Change)
  out_1 <- tibble::add_row(out, .before=1)
  out_2 <- data.frame(variables, est_1, change1)
  out_2 <- tibble::add_row(
    out_2,
    variables = var_lab,
    est_1 = est_lab,
    change1 = change_lab,
    .before = 1)
  tab_out <- data.frame(out, p, n)
  is.summary = c(T, rep(F, 1 + n_xlist))
  graph <-  forestplot::forestplot(
    out_2,
    mean = out_1$OR,
    lower = out_1$lb,
    upper =out_1$ub,
    is.summary = is.summary,
    hrzl_lines = hrzl_lines,
    txt_gp = fpTxtGp(label = gpar(cex = text_size),
                     ticks = gpar(cex = xtick_size),
                     xlab = gpar(cex = xlab_size)),
    xlab = xlab,
    ...)

  row.names(tab_out) <- NULL
  tab_out
}
