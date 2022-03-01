#' Assessing confounding effects using Generalized Linear regression models
#'
#' This is a faster option to 'chest_glm'.
#' It presents the effect estimates (such as \emph{odds ratios}) for the association between exposure and outcome variables by adding other variables (potential confounders) to the model sequentially.
#' The order of variables to be added is based on the magnitudes of the changes in effect estimates. It returns a graph and a table with effect estimates, 95\% confidence intervals and changes (\%) at different steps.
#'
#' @export
#' @import speedglm
#' @param crude An object of \emph{formula} for initial model, generally crude model.
#' However, any other variables can also be included here as the initial model.
#' @param xlist A \emph{vector} of characters with all variable names of potential confounders.
#' @param data \emph{Data frame}.
#' @param family Description of the error distribution. Default is \emph{"binomial"}.
#' @param method Method to detect for singularity.
#' @param na_omit Remove all missing values, default: 'na_omit = TRUE'.
#' @param plus Change the \code{+} sign before variable names.
#' @param indicate indicate progress
#' @param ... Further optional arguments for speedglm.
#' @return A table with effect estimates and their changes at all steps.
#' @seealso \pkg{'speedglm'}
#' @seealso \code{'glm'} of \pkg{'stats'}
#' @examples
#' vlist <- c("Age", "Sex", "Married", "Cancer", "CVD", "Education", "Income")
#' results <- chest_speedglm(
#'   crude = "Endpoint ~ Diabetes",
#'   xlist = vlist, na_omit = TRUE, data = diab_df
#' )
#' results
#' chest_plot(results)
#' @name chest_speedglm
chest_speedglm <- function(
                           crude, xlist, data,
                           family = binomial(),
                           method = c("eigen", "Cholesky", "qr"),
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
  mod_crude <- speedglm::speedglm(
    as.formula(crude),
    family = family,
    method = method,
    data = data,
    ...
  )

  #  temp fix until broom update
  my_tidy <- function(x) {
    estimate <- conf.low <- conf.high <- NULL
    a_1 <- summary(x)$coefficients %>%
      tibble::as_tibble(rownames = "term")
    colnames(a_1) <- c("term", "estimate", "std.error", "statistic", "p.value")
    a_ci <- confint(x) %>%
      tibble::as_tibble(rownames = "term")
    colnames(a_ci) <- c("term", "conf.low", "conf.high")
    result <- dplyr::left_join(a_1, a_ci, by = "term") %>%
      mutate(
        estimate = exp(estimate),
        conf.low = exp(conf.low),
        conf.high = exp(conf.high)
      )
    result
  }
  mod0 <- my_tidy(mod_crude) # mod0 <- broom::tidy(mod_crude, exponentiate = TRUE, conf.int = TRUE)
  n[1] <- stats::nobs(mod_crude)
  est[1] <- mod0$estimate[2]
  p[1] <- mod0$p.value[2]
  lb[1] <- mod0$conf.low[2]
  ub[1] <- mod0$conf.high[2]
  variables[1] <- c("Crude")
  initial_model <- crude
  for (i in 2:(n_xlist + 1)) {
    mod <- speedglm::speedglm(
      as.formula(crude),
      family = family,
      method = method,
      data = data,
      ...
    )

    hr_0 <- my_tidy(mod)$estimate[2] # hr_0 <- broom::tidy(mod, exponentiate = TRUE)$estimate[2]
    models <- lapply(xlist, function(x) {
      speedglm::speedglm(
        as.formula(paste(crude, "+", x)),
        data = data,
        family = family,
        method = method,
        ...
      )
    })
    hr_1 <- unlist(lapply(models, function(x) {
      my_tidy(x)$estimate[2] # broom::tidy(x, exponentiate = TRUE)$estimate[2]
    }))
    p_1 <- unlist(lapply(models, function(x) {
      my_tidy(x)$p.value[2] # broom::tidy(x)$p.value[2]
    }))
    chg <- (hr_1 - hr_0) * 100 / hr_0
    lb_1 <- unlist(lapply(models, function(x) {
      my_tidy(x)$conf.low[2] # broom::tidy(x, exponentiate = TRUE, conf.int = TRUE)$conf.low[2]
    }))
    ub_1 <- unlist(lapply(models, function(x) {
      my_tidy(x)$conf.high[2] # broom::tidy(x, exponentiate = TRUE, conf.int = TRUE)$conf.high[2]
    }))
    n_1 <- unlist(lapply(models, function(x) {
      stats::nobs(x)
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
  cat(" ", "\n")
  out <- data.frame(variables, est, lb, ub, Change)
  tab_out <- data.frame(out, p, n)
  row.names(tab_out) <- NULL
  fun <- "chest_speedglm"
  family <-  mod_crude$family$family
  lst_ret <- list(tab_out, fun, family)
  names(lst_ret) <- c("data", "fun", "family")
  lst_ret
}

