#'
#' Change-in-Estimate Approach to Assess Confounding Effects
#'
#'  In clinical trials and epidemiological studies, the association
#'  between an exposure and the outcome of interest in a study can be estimated by
#'  \emph{regression coefficients}, \emph{odds ratios} or \emph{hazard ratios} depending
#'  on the nature of study designs and outcome measurements. We use a general term
#'  \emph{effect estimate} here for any of those measurements in this document.
#'  Based on those measurements,
#'  we determine if a treatment is effective (or detrimental) or a factor is a risk factor.
#'  Imbalanced distributions of other factors could bias the effect estimates, called
#'  \emph{confounding}. One way to assess the
#'  confounding effect of a factor is to examine the difference in effect
#'  estimates between models with and without a specific factor. \code{'chest'} allows
#'  users quickly calculate the changes when potential confounding factors
#'  are sequentially added to the model in a stepwise fashion. At each step, one
#'  variable which creates the largest change (\%) of the effect estimate among the remaining
#'  variables is added to the model. \code{'chest'} returns a graph and a data frame (table) with
#'  effect estimates (95\% CI) and change (\%) values. The package currently has the following main
#'  functions: \code{'chest_lm'} for linear regression, \code{'chest_glm'} for logistic
#'  regression and Poisson regression, \code{'chest_speedglm'} using \code{'speedlm'} as
#'  a faster alternative of \code{'chest_glm'}, \code{'chest_clogit'} for matched logistic
#'  regression, \code{'chest_nb'} for negative binomial regression and \code{'chest_cox'} for
#'  Cox proportional hazards models.
#'
#' @docType package
#' @name chest
#' @importFrom broom tidy glance
#' @import ggplot2
#' @importFrom tibble add_row
#' @importFrom MASS glm.nb
#' @import speedglm
#' @import forestplot
#' @import stats
#' @import grid
#' @importFrom dplyr mutate %>%
#'
#' @examples
#'
#' ? chest_speedglm
#' ? chest_glm
#' ? chest_cox
#' ? chest_clogit
#' ? chest_lm
#' ? chest_nb
#' ? chest_plot
#' ? chest_forest
#' @references {
#' Zhiqiang Wang (2007) <https://doi.org/10.1177/1536867X0700700203> }
#'
#'
utils::globalVariables(c(
  "est", "lb", "ub", "Change", "variables", "est_values", "x_value"
))
NULL
