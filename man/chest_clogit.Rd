% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chest_clogit.R
\name{chest_clogit}
\alias{chest_clogit}
\title{Assessing confounding effects using conditional logistic regression models}
\usage{
chest_clogit(
  crude,
  xlist,
  data,
  method = "exact",
  na_omit = TRUE,
  plus = "  + ",
  indicate = FALSE,
  ...
)
}
\arguments{
\item{crude}{An object of \emph{formula} for the initial model, generally crude model.
However, any other variables can also be included here as the initial model.}

\item{xlist}{A \emph{vector} of characters with all variable names of potential confounders.}

\item{data}{\emph{Data frame}.}

\item{method}{See 'clogit', default is the \emph{"exact"} method.}

\item{na_omit}{Remove all missing values, default: 'na_omit = TRUE'.}

\item{plus}{Change the \code{+} sign before variable names.}

\item{indicate}{indicate the calculation progress.}

\item{...}{Further optional arguments.}
}
\value{
A table with effect estimates and their changes at all steps.
}
\description{
\code{'chest_clogit'} is used to fit many \emph{Conditional Logistic Regression}
models to assess confounding effects.
}
\examples{
vlist <- c("Age", "Sex", "Married", "Cancer", "CVD", "Education", "Income")
chest_clogit(
  crude = "Endpoint ~ Diabetes + strata(mid)",
  xlist = vlist, data = diab_df
)
}
\seealso{
chest

'clogit' in \pkg{'survival'}
}
