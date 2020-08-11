#' Create a projection for text matching
#'
#' Calculates the linear information in the word counts apart from the topics
#' about the treatment.
#' 
#' The function returns one loading per document, per level of the factor.
#' Thus in the standard case of two levels (treatment/control) the projection
#' is actually two-dimensional (indicating words that are particularly indicative
#' of treatment and words particularly indicative of control).
#' 
#' When \code{interactions=FALSE} only the content covariate parameters
#' are used (and not the topic-covariate interactions).  This may often
#' be a decent approximation to the full calculation because the topic-covariate
#' interactions are typically very sparse.
#' 
#' When \code{interactions=TRUE} information from the interaction of the topic
#' and the content covariate is included in the projections.  The software offers
#' three ways to do this based on the options set for \code{type}. In each case
#' the difference is how we reweight the topic-specific components of the interaction.
#' 
#' When \code{type="theta"} (the option used in the paper), we simply use the theta
#' values estimated under the model.  When \code{type="phi"}, we recompute the token-level
#' topic loadings conditional on the document-topics theta.  This allows individual words
#' to have their own topic-specific loadings.  When \code{type="refit"}, we recompute
#' the token-level topic loadings but under each different level of the content covariate.
#' The option is called \code{"refit"} because it is essentially refitting the tokens under
#' each different potential level of the content covariate when calculating the projection to
#' that level.
#'
#' @param stm_model the stm content covariate model from which to develop the projection
#' @param documents the documents that we want the projection for.  Note that 
#' these must be aligned with the vocabulary.  If they were not the original documents
#' used to fit the model, see \link[stm]{alignCorpus}.
#' @param interactions a logical which defaults to TRUE. Determines whether or not the topic-aspect
#' interactions are included.
#' @param type determines how the topic-covariate interactions are included (see details below). 
#' If interactions=FALSE this has no effect.
#' @param verbose a logical indicating if progress should be printed to the screen
#' @return list
#'
#' \item{projection}{the projection of word count information on the document} 
#' \item{diagnostic}{the sum of interaction projections for each word type 
#' normalized by the total number of words.  This is helpful for assessing which
#' elements of the vocabularly are contributing to the topic-specific elements of
#' the projection.  For non-topic specific parts, the relative contributions can 
#' be read directly off the kappa object in stm.} 
#' @references
#' Roberts, M., Stewart, B., Nielsen, R. (2020)
#' "Adjusting for Confounding with Text Matching." 
#' In American Journal of Political Science
#' @examples
#' \donttest{
#' data(sim)
#' projection <- project(sim_topics,sim_documents)
#' }
#' @export
project <- function(stm_model, documents, 
                    interactions=TRUE, 
                    type=c("theta", "phi","refit"),
                    verbose=TRUE) {
  
  #match arguments and check type 
  int.type <- match.arg(type)
  if(!inherits(stm_model, "STM")) stop("stm_model is not an stm object")
  if(length(stm_model$beta$logbeta)<2) stop("stm_model does not have a content covariate.")
  
  #deal with older versions of stm by automatically coding contrast
  if(is.null(stm_model$settings$kappa$contrast)) stm_model$settings$kappa$contrast <- FALSE
  #if we have the contrast coding we are effectively working with one less element
  contrast <- stm_model$settings$kappa$contrast
  if(contrast) { 
    index <- index[-length(index)]  
  }
  if(interactions & contrast) {
    if(int.type%in% c("refit", "phi")) stop("Only type='theta' works with contrast")
  }
  #let's start a timer
  t1 <- proc.time()
  
  #pull some key elements out of the model for easy reference
  kappa <- stm_model$beta$kappa$params
  K <- stm_model$setting$dim$K
  A <- stm_model$setting$dim$A
  index <- sort(unique(stm_model$settings$covariates$betaindex))
  

  
  #Compute the projection for the non-interactive elements
      # in Equation 5 of the paper this is \sum_l w_{i,l}\kappa_{t,c}^{cov}
  proj <- vector(mode="list", length=length(index))
  for(i in 1:length(index)) {
    #for each value grab the weighted sum of the kappa elements
    #for each level of A
    proj[[i]] <- unlist(lapply(documents, function(doc) {
      sum((kappa[[K+index[i]]])[doc[1,]]*doc[2,])/sum(doc[2,])
    }))
  }
  

  
  if(interactions & int.type=="refit") {
    #Plan:
    # for each aspect {
    # - compute phi (the posterior mean of the topic indicator)
    # - for each word, multiply through the relevant interaction
    # - sum over the document and normalize
    # }
    if(verbose) {
      dstar <- max(round(length(documents)/100),1)
      msg <- sprintf("Starting Interactions...\nThis may take a while. I will print a dot every %i docs. \n", dstar)
      cat(msg)
    }
    
    logbeta <- stm_model$beta$logbeta
    
    #we want to store the contributions aggregated over word
    diagnostic <- vector(mode="list", length=length(index))
    for(i in 1:length(diagnostic)) {
      diagnostic[[i]] <- rep(0,stm_model$settings$dim$V)
    }
    agg.wcounts <- rep(0, length(index))
    
    for(d in 1:length(documents)) {
      doc <- documents[[d]]
      for(a in 1:A) {
        expeta <- c(exp(stm_model$eta[d,]),1)
        #pieces for the derivatives of the exp(eta)beta part
        EB <- expeta*exp(logbeta[[index[a]]][,doc[1,],drop=FALSE]) #calculate exp(eta)\beta for each word
        EB <- t(EB)/colSums(EB) #transpose and norm by (now) the row
        
        #at this point EB is the phi matrix
        phi <- t(EB*(doc[2,])) #multiply through by word count
        #create a sparse K by V matrix containing interaction components  
        kappamat <- Matrix::Matrix(do.call(rbind,kappa[(K+ length(index) + (a-1)*K + 1):(K+ length(index) + a*K)]))
        kappamat <- kappamat[,doc[1,]]
        word.proj <- Matrix::colSums(kappamat*phi)
        #add in contributions from interaction and word counts
        diagnostic[[a]][doc[1,]] <-diagnostic[[a]][doc[1,]] +  word.proj
        agg.wcounts[a] <- agg.wcounts[a] + sum(doc[2,])
        #elementwise multiply and sum
        #now add it to the existing projection
        proj[[a]][d] <- proj[[a]][d] + sum(word.proj)/sum(doc[2,])
      }
      if(verbose & d%%dstar==0) cat(".")
    }
  }
  
  
  if(interactions & int.type=="phi") {
    if(verbose) {
      dstar <- max(round(length(documents)/100),1)
      msg <- sprintf("Starting Interactions...\nThis may take a while. I will print a dot every %i docs. \n", dstar)
      cat(msg)
    }
    logbeta <- stm_model$beta$logbeta
    #we want to store the contributions aggregated over word
    diagnostic <- vector(mode="list", length=length(index))
    for(i in 1:length(diagnostic)) {
      diagnostic[[i]] <- rep(0,stm_model$settings$dim$V)
    }
    agg.wcounts <- rep(0, length(index))
    bindex <- stm_model$settings$covariates$betaindex
    for(d in 1:length(documents)) {
      doc <- documents[[d]]
      expeta <- c(exp(stm_model$eta[d,]),1)
      #pieces for the derivatives of the exp(eta)beta part
      EB <- expeta*exp(logbeta[[bindex[d]]][,doc[1,],drop=FALSE]) #calculate exp(eta)\beta for each word
      EB <- t(EB)/colSums(EB) #transpose and norm by (now) the row
      
      #at this point EB is the phi matrix
      phi <- t(EB*(doc[2,])) #multiply through by word count
      for(a in 1:length(index)) {
        #create a sparse K by V matrix containing interaction components  
        kappamat <- Matrix::Matrix(do.call(rbind,kappa[(K+ length(index) + (a-1)*K + 1):(K+ length(index) + a*K)]))
        kappamat <- kappamat[,doc[1,]]
        word.proj <- Matrix::colSums(kappamat*phi)
        #add in contributions from interaction and word counts
        diagnostic[[a]][doc[1,]] <-diagnostic[[a]][doc[1,]] +  word.proj
        agg.wcounts[a] <- agg.wcounts[a] + sum(doc[2,])
        #elementwise multiply and sum
        #now add it to the existing projection
        proj[[a]][d] <- proj[[a]][d] + sum(word.proj)/sum(doc[2,])
      }
      if(verbose & d%%dstar==0) cat(".")
    }}
  
  
  if(interactions & int.type=="theta") {
    if(verbose) {
      dstar <- max(round(length(documents)/100),1)
      msg <- sprintf("Starting Interactions...\nThis may take a while. I will print a dot every %i docs. \n", dstar)
      cat(msg)
    }
    diagnostic <- vector(mode="list", length=length(index))
    for(i in 1:length(diagnostic)) {
      diagnostic[[i]] <- rep(0,stm_model$settings$dim$V)
    }
    agg.wcounts <- rep(0, length(index))
    for(d in 1:length(documents)) {
      doc <- documents[[d]]
      theta <- stm_model$theta[d,]
      wct <- matrix(rep(doc[2,], each=K), nrow=K)
      theta.wct <- wct*theta
      for(a in 1:length(index)) {    
        #create a sparse K by V matrix containing interaction components  
        kappamat <- Matrix::Matrix(do.call(rbind,kappa[(K+ length(index) + (a-1)*K + 1):(K+ length(index) + a*K)]))
        kappamat <- kappamat[,doc[1,]]
        word.proj <- Matrix::colSums(kappamat*theta.wct)
        #add in contributions from interaction and word counts
        diagnostic[[a]][doc[1,]] <-diagnostic[[a]][doc[1,]] +  word.proj
        agg.wcounts[a] <- agg.wcounts[a] + sum(doc[2,])
        #elementwise multiply and sum
        #now add it to the existing projection
        proj[[a]][d] <- proj[[a]][d] + sum(word.proj)/sum(doc[2,])
      }
      if(verbose & d%%dstar==0) cat(".")
    }}
  
  if(!interactions) {
    cat(sprintf("\n Calculation Complete in %.2f seconds",(proc.time() - t1)[3]))
    return(list(projection=proj))
  } else {
    for(a in 1:length(index)) {
      diagnostic[[a]] <- diagnostic[[a]]/agg.wcounts[a]
    }
    cat(sprintf("\n Calculation Complete in %.2f seconds",(proc.time() - t1)[3]))
    return(list(projection=proj, diagnostic=diagnostic))
  }
}
