#' Assessing confounding effects using conditional logistic regression models
#'
#' \code{'chest_clogit'} is used to fit many \emph{Conditional Logistic Regression}
#' models to assess confounding effects.
#'
#' @export
#' @import survival
#' @param crude An object of \emph{formula} for the initial model, generally crude model.
#' However, any other variables can also be included here as the initial model.
#' @param xlist A \emph{vector} of characters with all variable names of potential confounders.
#' @param data \emph{Data frame}.
#' @param method See 'clogit', default is the \emph{"exact"} method.
#' @param na_omit Remove all missing values, default: 'na_omit = TRUE'.
#' @param plus Change the \code{+} sign before variable names.
#' @param indicate indicate the calculation progress.
#' @param ... Further optional arguments.
#' @return A table with effect estimates and their changes at all steps.
#' @seealso chest
#' @seealso 'clogit' in \pkg{'survival'}
#' @examples
#' vlist <- c("Age", "Sex", "Married", "Cancer", "CVD", "Education", "Income")
#' chest_clogit(
#'   crude = "Endpoint ~ Diabetes + strata(mid)",
#'   xlist = vlist, data = diab_df
#' )
#' @name chest_clogit
chest_clogit <- function(
                         crude, xlist, data,
                         method = "exact",
                         na_omit = TRUE,
                         plus = "  + ",
                         indicate = FALSE,
                         ...) {
  pick <- variables <- est <- se <- Change <- p <- lb <- ub <- n <- c()
  n_xlist <- length(xlist)
  data <- data.frame(c(data[all.vars(as.formula(crude))], data[xlist]))
  if (na_omit) {
    data <- na.omit(data)
  }
  mod_crude <- survival::clogit(
    as.formula(crude),
    method = method,
    data = data,
    ...
  )
  mod0 <- broom::tidy(
    mod_crude,
    exponentiate = TRUE,
    conf.int = TRUE
  )
  n[1] <- broom::glance(mod_crude)$n
  est[1] <- mod0$estimate[1]
  p[1] <- mod0$p.value[1]
  lb[1] <- mod0$conf.low[1]
  ub[1] <- mod0$conf.high[1]
  variables[1] <- c("Crude")
  initial_model <- crude
  for (i in 2:(n_xlist + 1)) {
    mod <- clogit(as.formula(crude),
      method = method,
      data = data,
      ...
    )
    hr_0 <- broom::tidy(mod, exponentiate = TRUE)$estimate[1]
    models <- lapply(xlist, function(x) {
      stats::update(mod, as.formula(paste0(". ~ . +", x)))
    })
    hr_1 <- unlist(lapply(models, function(x) {
      broom::tidy(x, exponentiate = TRUE)$estimate[1]
    }))
    p_1 <- unlist(lapply(models, function(x) {
      broom::tidy(x)$p.value[1]
    }))
    chg <- (hr_1 - hr_0) * 100 / hr_0
    lb_1 <- unlist(lapply(models, function(x) {
      broom::tidy(x,
        exponentiate = TRUE,
        conf.int = TRUE
      )$conf.low[1]
    }))
    ub_1 <- unlist(lapply(models, function(x) {
      broom::tidy(x,
        exponentiate = TRUE,
        conf.int = TRUE
      )$conf.high[1]
    }))
    n_1 <- unlist(lapply(models, function(x) {
      broom::glance(x)$n
    }))
    pick[i] <- xlist[which.max(abs(chg))]
    xlist <- xlist[-which(xlist %in% paste0(pick[i]))]
    crude <- paste0(crude, "+", paste0(pick[i]), collapse = " + ")
    variables[i] <- paste0(plus, pick[i])
    est[i] <- hr_1[which.max(abs(chg))]
    lb[i] <- lb_1[which.max(abs(chg))]
    ub[i] <- ub_1[which.max(abs(chg))]
    Change[i] <- chg[which.max(abs(chg))]
    p[i] <- p_1[which.max(abs(chg))]
    n[i] <- n_1[which.max(abs(chg))]
    if (indicate) {
      cat("\r", i, "out of", n_xlist + 1)
    }
  }
  cat("\n")
  out <- data.frame(variables, est, lb, ub, Change)
  tab_out <- data.frame(out, p, n)
  row.names(tab_out) <- NULL
  fun <- "chest_clogit"
  family <- "clogit"
  lst_ret <- list(tab_out, fun, family)
  names(lst_ret) <- c("data", "fun", "family")
  lst_ret
}

