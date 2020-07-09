#' Assessing confounding effects using Cox Proportional Hazards regression models
#'
#' \code{'chest_cox'} is used to assess confounding effects
#' using Proportional Hazards Regression Model (\code{'coxph'} from \pkg{'survival'} package).
#' It presents the effect estimates (such as hazard ratios) for the association between exposure and outcome variables by adding other variables (potential confounders) to the model sequentially.
#' The order of variables to be added is based on the magnitudes of the changes in effect estimates.
#'
#' @export
#' @import survival
#' @param crude An object of \emph{formula} for initial model, generally crude model.
#' However, any other variables can also be included here as the initial model.
#' @param xlist A \emph{vector} of characters with variable names of potential confounders.
#' @param data \emph{Data frame}.
#' @param na_omit Remove all missing values, default: 'na_omit = TRUE'.
#' @param plus Change the \code{+} sign before variable names.
#' @param indicate indicate the progress.
#' @param ... Further optional arguments for forestplot.
#' @return A table with effect estimates and their changes at all steps.
#' @seealso \pkg{'survival'}
#'
#' @examples
#'
#' vlist <- c("Age", "Sex", "Married", "Cancer", "CVD", "Education", "Income")
#'
#' chest_cox(crude = "Surv(t0, t1, Endpoint) ~ Diabetes", xlist = vlist, data = diab_df)
#'
#' @name chest_cox
#'
chest_cox <- function(
  crude, xlist, data,
  na_omit = TRUE,
  plus = "  + ",
  indicate = FALSE,
  ...) {
  pick <- variables <- est <- Change <- p <- lb <- ub <- n<- c()
  n_xlist <- length(xlist)
  data <- data.frame(c(data[all.vars(as.formula(crude))], data[xlist]))
  if (na_omit) {data <- na.omit(data)}
  mod_crude <- survival::coxph(as.formula(crude),
                               data = data,
                               ...)
  n[1] <- broom::glance(mod_crude)$n
  mod0    <- broom::tidy(mod_crude, exponentiate = TRUE, conf.int = TRUE)
  est[1] = mod0$estimate[1]
  lb[1] = mod0$conf.low[1]
  ub[1] = mod0$conf.high[1]
  p[1] = mod0$p.value[1]
  variables[1] <- c("Crude")
  initial_model <- crude
  for (i in 2:(length(xlist)+1)) {
    mod    <- survival::coxph(as.formula(crude),
                              data = data,
                              ...)
    hr_0   <- broom::tidy(mod, exponentiate = TRUE)$estimate[1]
    models  <- lapply(xlist, function(x)
      update(mod, as.formula(paste0(". ~ . +", x))))
    hr_1    <- unlist(lapply(models, function(x)
      broom::tidy(x, exponentiate = TRUE)$estimate[1]))
    p_1       <- unlist(lapply(models, function(x)
      broom::tidy(x)$p.value[1]))
    chg     <- (hr_1-hr_0)*100/hr_0
    lb_1    <- unlist(lapply(models, function(x)
      broom::tidy(x, exponentiate = TRUE)$conf.low[1]))
    ub_1    <- unlist(lapply(models, function(x)
      broom::tidy(x, exponentiate = TRUE)$conf.high[1]))
    n_1    <- unlist(lapply(models, function(x)
      broom::glance(x)$n))
    pick[i] <- xlist[which.max(abs(chg))]
    xlist <- xlist[-which(xlist %in% paste0(pick[i]))]
    crude <- paste0(crude, "+", paste0(pick[i]), collapse = " + ")
    variables[i] = paste0(plus, pick[i])
    est[i] = hr_1[which.max(abs(chg))]
    Change[i] = chg[which.max(abs(chg))]
    lb[i] = lb_1[which.max(abs(chg))]
    ub[i] = ub_1[which.max(abs(chg))]
    p[i] = p_1[which.max(abs(chg))]
    n[i] = n_1[which.max(abs(chg))]
    if (indicate) {
      cat("\r",i, "out of", n_xlist + 1)
    }
  }
  out <- data.frame(variables, est,lb, ub, Change)
  tab_out <- data.frame(out, p, n)
  row.names(tab_out) <- NULL
  fun <- "chest_cox"
  family <- "coxph"
  lst_ret <- list(tab_out, fun, family)
  names(lst_ret) <- c("data", "fun", "family")
  lst_ret
}
