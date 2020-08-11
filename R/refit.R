#' Refit the Documents Under an Alternate Treatment Level
#'
#' A function to refit all the document loadings in an STM under an alternative content covariate level.
#'
#' This function cycles through all the documents and refits the theta parameter (the document-topic loadings)
#' as though they all had the same level of the content covariate as specified by \code{content_level}.
#' For documents that already had that content_level the results should be very close to their originally
#' learned value of theta if the model had converged.  However, because \pkg{stm} is an iterative algorithm
#' and the global parameters are updated last, they could be different.
#'
#' @param stm_model the stm content covariate model from which to develop the projection
#' @param documents the documents that we want to refit.  This currently only
#' works if these are the original documents used to fit the model.  However, this same
#' refitting procedure could be done by using \link[stm]{fitNewDocuments} in the \pkg{stm}.
#' @param content_level a string containing the level under which to refit the documents
#' @param verbose a logical indicating if progress should be printed to the screen
#' @return list
#' \item{content_level}{the level of the content covariate}
#' \item{theta}{the document-topic matrix refit under the new content level}
#' @references
#' Roberts, M., Stewart, B., Nielsen, R. (2020)
#' "Adjusting for Confounding with Text Matching." 
#' In American Journal of Political Science
#' @examples
#' \donttest{
#'  data(sim)
#'  refitted <- refit(sim_topics, sim_documents, content_level="1")
#' }
#' @export
refit <- function(stm_model, 
                  documents, 
                  content_level, 
                  verbose=TRUE) {
  
  #Process the content covariate level to refit under
  if(!inherits(content_level,"character")) stop("treatment_level argument must be a character.")
  treatment_level <- match.arg(content_level, choices=stm_model$settings$covariates$yvarlevels)
  bnum <- match(treatment_level, stm_model$settings$covariates$yvarlevels)
  beta.index <- rep(bnum,length(documents))
  
  #extract everything to refit under
  beta <- lapply(stm_model$beta$logbeta, exp)
  sigma <- stm_model$sigma
  lambda.old <- stm_model$eta
  update.mu=(!is.null(stm_model$mu$gamma))
  mu <- stm_model$mu$mu
  
  #quickly define useful constants
  V <- ncol(beta[[1]])
  K <- nrow(beta[[1]])
  N <- length(documents)
  A <- length(beta)
  ctevery <- ifelse(N>100, floor(N/100), 1)
  if(!update.mu) mu.i <- as.numeric(mu)
  
  # 1) Initialize Sufficient Statistics 
  lambda <- vector("list", length=N)
  
  # 2) Precalculate common components
  sigobj <- try(chol.default(sigma), silent=TRUE)
  if(inherits(sigobj,"try-error")) {
    sigmaentropy <- (.5*determinant(sigma, logarithm=TRUE)$modulus[1])
    siginv <- solve(sigma)
  } else {
    sigmaentropy <- sum(log(diag(sigobj)))
    siginv <- chol2inv(sigobj)
  }
  # 3) Document Scheduling
  for(i in 1:N) {
    #update components
    doc <- documents[[i]]
    words <- doc[1,]
    aspect <- beta.index[i]
    init <- lambda.old[i,]
    if(update.mu) mu.i <- mu[,i]

    #infer the document
    doc.results <- stm::optimizeDocument(doc, eta=init, mu=mu.i,
                                         sigmainv=siginv, beta= beta[[aspect]],
                                         sigma=sigma, sigmaentropy=sigmaentropy,
                                         posterior=FALSE)
    lambda[[i]] <- doc.results$lambda
    if(verbose && i%%ctevery==0) cat(".")
  }
  if(verbose) cat("\n") #add a line break for the next message.

  #4) Combine and Return Sufficient Statistics
  lambda <- do.call(rbind, lambda)
  lambda <- cbind(lambda,0)
  theta <- exp(lambda - matrixStats::rowLogSumExps(lambda))
  return(list(theta=theta, content_level=content_level))
}
