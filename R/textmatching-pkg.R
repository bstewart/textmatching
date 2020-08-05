#' Text Matching
#' 
#' Note: this package is in the very early stages.  Our plan is to implement
#' new functionality in future versions.  As a result the API here might change
#' substantially. Because functionality is early we haven't implemented diagnostics
#' but please see the paper for ideas.
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
#' using \pkg{cem} or other matching package of your choice. We include
#' the \code{\link{cem_match}} wrapper for convenience.
#' 
#' Pre-Fit Models and Data: \code{\link{sim}}.  See the Examples in the help 
#' file of \code{\link{sim}} for a walkthrough of all functionality.
#' 
#' Please be sure to read your documents! This package currently only offers
#' basic functionality so it is easy to overmatch or undermatch if you aren't
#' carefully examining the matched pairs the algorithm returns.
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

#' Simulated Matching Data
#' 
#' A 270 document set along with a prefit topic model that is used to demonstrate
#' the matching functionality.
#' 
#' This is a set of documents and a prefit topic model used to demonstrate functionality
#' of the package.  It is loosely based off of the gender citation example in Roberts,
#' Stewart and Nielsen (2020). The data is simulated such that the true treatment effect
#' is 1 for all units. There is separable 'unobserved' confounding provided by the 
#' binary variable \code{confound} variables which are themselves based on real data.  
#' The outcome \code{simy} is purely synthetic.
#' 
#' Note that due to data size limitations on CRAN we only included a subset of the documents
#' and a prefit topic model.  Because there are so many fewer documents than were used to fit
#' the original topic model, any model fit with this data would likely look substantially different.
#' The original model was fit on 3201 documents using the treatment as the content covariate with 15
#' topics.  All other settings were at their default.    
#' 
#' Because we had to select subsets of the data, we emphasize that the example here isn't reflective 
#' of a real problem, its just a way of getting a handle on the objects in the code.
#' 
#' @name sim
#' @aliases sim sim_documents sim_meta sim_topics sim_vocab
#' @docType data
#' @format stm formatted object corresponding to simulated documents
#' \describe{ 
#' \item{\code{treat}}{a binary treatment variable}
#' \item{\code{confound}}{an unknown binary confounding variable} 
#' \item{\code{simy}}{a simulated outcome} 
#' }
#' @source  Roberts, M., Stewart, B., Nielsen, R. (2020)
#' "Adjusting for Confounding with Text Matching." 
#'  In American Journal of Political Science
#' @keywords datasets
#' @examples
#' #We start by assuming that you have run a topic model in stm using
#' #your treatment variable as a content covariate. This is step 1.
#'
#' #We have done this already and the following command loads the 
#' #topic model as well as the documents, vocab and meta data objects.
#' #See the stm package for more details about these model objects.
#' data(sim)
#' 
#' #Step 2 is to recalculate the topics as though they were observed
#' #as treated.  We do this using the refit() function.
#' refitted <- refit(sim_topics, sim_documents, content_level="1")
#' 
#' #to this we needed to specify the value of the treatment (here "1").  
#' #If you have forgotten content_levels() will tell you the levels
#' #for a given stm model. 
#' content_levels(sim_topics)
#' 
#' #Step 3 is to calculate the projection onto the treatment variable
#' projection <- project(sim_topics, sim_documents, interactions = FALSE)
#' #NB: here we have turned off interactions purely for speed during
#' #CRAN checks.  Consider including them if you believe topic-specific
#' #word choice is relevant.  See description above.
#' 
#' #Finally Step 4 is to match using CEM or other matching method of your
#' #choice
#' matched <- cem_match(refitted,projection=projection, sim_meta$treat,
#'                      projection_breaks=2)
#' #note here we use a much weaker match on the projections because the data
#' #have already been trimmed a lot.
#' 
#' #Now the matched data can be analyzed using standard tools from cem
#' cem::att(matched, simy ~ treat, data=sim_meta)
#' #the estimator overestimates a bit but contains the truth in the CI
#' 
#' #We can compare this to the unadjusted difference in means (overestimates)
#' summary(lm(simy ~ treat, data=sim_meta))
#' #and the oracle estimator (based on unobserved covariates)
#' summary(lm(simy ~ treat + confound1 + confound2 + confound3,data=sim_meta))
#' 
#' #Please, be sure to diagnose your matches!!! The key advantage of matching
#' #is being able to examine matched pairs.  It is always important to read 
#' #the documents!
#'
NULL