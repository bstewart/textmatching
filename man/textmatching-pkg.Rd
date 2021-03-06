% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/textmatching-pkg.R
\docType{package}
\name{textmatching-pkg}
\alias{textmatching-pkg}
\title{Text Matching}
\description{
Note: this package is in the very early stages.  Our plan is to implement
new functionality in future versions.  As a result the API here might change
substantially. Because functionality is early we haven't implemented diagnostics
but please see the paper for ideas.
}
\details{
This package implements functions designed to help use \pkg{stm} to perform adjustment
for text-based confounders.  The proposed method has four steps
(see pg 5 of the Early Access publication).

Step 1: estimate a structural topic model including the treatment
as a content covariate.  This step can be done using \pkg{stm}.

Step 2: extract each document's topics calculated as though 
treated. This can be done using \code{\link{refit}}.

Step 3: extract each document's projection onto the treated
variable.  This can be done using \code{\link{project}}.

Step 4: match on results of steps 2 and 3.  This can be done
using \pkg{cem} or other matching package of your choice. We include
the \code{\link{cem_match}} wrapper for convenience.

Pre-Fit Models and Data: \code{\link{sim}}.  See the Examples in the help 
file of \code{\link{sim}} for a walkthrough of all functionality.

Please be sure to read your documents! This package currently only offers
basic functionality so it is easy to overmatch or undermatch if you aren't
carefully examining the matched pairs the algorithm returns.
}
\references{
Roberts, M., Stewart, B., Nielsen, R. (2020)
"Adjusting for Confounding with Text Matching." 
In American Journal of Political Science

Additional papers at: structuraltopicmodel.com
}
\seealso{
\code{\link{stm}}
}
\author{
Author: Margaret E. Roberts, Brandon M. Stewart and Richard Nielsen

Maintainer: Brandon Stewart <bms4@princeton.edu>
}
\keyword{package}
