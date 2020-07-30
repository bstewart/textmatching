#' Text Matching
#' 
#' This package implements functions designed to help use \pkg{stm} to perform adjustment
#' for text-based confounders.  The proposed method has four steps
#' (see pg 5 of the Early Access publication).
#' 
#' Step 1: estimate a structural topic model including the treatment
#' as a content covariate.  This step can be done using \pkg{stm}.
#' 
#' Step 2: extract each document's topics calculated as though 
#' treated. This can be done using \code{\link{refit}}.
#' 
#' Step 3: extract each document's projection onto the treated
#' variable.  This can be done using \code{\link{project}}.
#' 
#' Step 4: match on results of steps 2 and 3.  This can be done
#' using \pkg{cem} or other matching package of your choice.
#' 
#' Pre-Fit Models and Data: \code{\link{sim_model}}, \code{\link{sim_documents}}
#' 
#' @name textmatching-pkg
#' @docType package
#' @author Author: Margaret E. Roberts, Brandon M. Stewart and Richard Nielsen
#' 
#' Maintainer: Brandon Stewart <bms4@@princeton.edu>
#' @seealso \code{\link{stm}}
#' @references 
#' Roberts, M., Stewart, B., Nielsen, R. (2020)
#' "Adjusting for Confounding with Text Matching." 
#' In American Journal of Political Science
#' 
#' Additional papers at: structuraltopicmodel.com
#' @keywords package
NULL