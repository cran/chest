% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chest_cox.R
\name{chest_cox}
\alias{chest_cox}
\title{Assessing confounding effects using Cox Proportional Hazards regression models}
\usage{
chest_cox(
  crude,
  xlist,
  data,
  na_omit = TRUE,
  plus = "  + ",
  indicate = FALSE,
  ...
)
}
\arguments{
\item{crude}{An object of \emph{formula} for initial model, generally crude model.
However, any other variables can also be included here as the initial model.}

\item{xlist}{A \emph{vector} of characters with variable names of potential confounders.}

\item{data}{\emph{Data frame}.}

\item{na_omit}{Remove all missing values, default: 'na_omit = TRUE'.}

\item{plus}{Change the \code{+} sign before variable names.}

\item{indicate}{indicate the progress.}

\item{...}{Further optional arguments for forestplot.}
}
\value{
A table with effect estimates and their changes at all steps.
}
\description{
\code{'chest_cox'} is used to assess confounding effects
using Proportional Hazards Regression Model (\code{'coxph'} from \pkg{'survival'} package).
It presents the effect estimates (such as hazard ratios) for the association between exposure and outcome variables by adding other variables (potential confounders) to the model sequentially.
The order of variables to be added is based on the magnitudes of the changes in effect estimates.
}
\examples{

vlist <- c("Age", "Sex", "Married", "Cancer", "CVD", "Education", "Income")

chest_cox(crude = "Surv(t0, t1, Endpoint) ~ Diabetes", xlist = vlist, data = diab_df)
}
\seealso{
\pkg{'survival'}
}
