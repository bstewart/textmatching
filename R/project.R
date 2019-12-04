#' Create a projection for matching
#'
#' Text about that.
#'
#' @param mod test
#' @param docs test
#' @param interactions test
#' @param int.type test
#' @return list
#'
#' \item{mu}{example}
#' @references
#' Roberts, M., Stewart, B., Tingley, D., and Airoldi, E. (2013)
#' "The structural topic model and applied social science." In Advances in
#' Neural Information Processing Systems Workshop on Topic Models: Computation,
#' Application, and Evaluation. http://goo.gl/uHkXAQ
#' @examples
#' \dontrun{
#' example
#' }
#' @export
project <- function(mod, docs, interactions=FALSE,
                    int.type=c("refit", "phi", "theta")) {
  int.type <- match.arg(int.type)
  t1 <- proc.time()
  if(!require(Matrix)) stop("Install the Matrix Package")
  kappa <- mod$beta$kappa$params
  K <- mod$setting$dim$K
  A <- mod$setting$dim$A
  index <- sort(unique(mod$settings$covariates$betaindex))
  #deal with older versions by automatically coding contrast
  if(is.null(mod$settings$kappa$contrast)) mod$settings$kappa$contrast <- FALSE
  contrast <- mod$settings$kappa$contrast
  if(contrast) {
    index <- index[-length(index)]
  }

  proj <- vector(mode="list", length=length(index))
  for(i in 1:length(index)) {
    proj[[i]] <- unlist(lapply(docs, function(doc) {
      sum((kappa[[K+index[i]]])[doc[1,]]*doc[2,])/sum(doc[2,])
    }))
  }

  if(interactions & contrast) {
    if(int.type%in% c("refit", "phi")) stop("Only int.type='theta' works with contrast")
  }

  if(interactions & int.type=="refit") {
    #Projection from above still holds.  We just need to add the interactions
    #Plan:
    # for each aspect {
    # - compute phi
    # - for each word, multiply through the relevant interaction
    # - sum over the document and normalize
    # }
    cat("Starting Interactions...\n This may take a while...it prints a dot every 100 docs. \n")
    logbeta <- mod$beta$logbeta
    #we want to store the contributions aggregated over word
    diagnostic <- vector(mode="list", length=length(index))
    for(i in 1:length(diagnostic)) {
      diagnostic[[i]] <- rep(0,mod$settings$dim$V)
    }
    agg.wcounts <- rep(0, length(index))

    for(d in 1:length(docs)) {
      for(a in 1:A) {
        expeta <- c(exp(mod$eta[d,]),1)

        #pieces for the derivatives of the exp(eta)beta part
        EB <- expeta*exp(logbeta[[index[a]]][,docs[[d]][1,],drop=FALSE]) #calculate exp(eta)\beta for each word
        EB <- t(EB)/colSums(EB) #transpose and norm by (now) the row

        #at this point EB is the phi matrix
        phi <- t(EB*(docs[[d]][2,])) #multiply through by word count
        #create a sparse K by V matrix containing interaction components
        kappamat <- Matrix::Matrix(do.call(rbind,kappa[(K+ length(index) + (a-1)*K + 1):(K+ length(index) + a*K)]))
        kappamat <- kappamat[,docs[[d]][1,]]
        word.proj <- Matrix::colSums(kappamat*phi)
        #add in contributions from interaction and word counts
        diagnostic[[a]][docs[[d]][1,]] <-diagnostic[[a]][docs[[d]][1,]] +  word.proj
        agg.wcounts[a] <- agg.wcounts[a] + sum(docs[[d]][2,])
        #elementwise multiply and sum
        #now add it to the existing projection
        proj[[a]][d] <- proj[[a]][d] + sum(word.proj)/sum(docs[[d]][2,])
      }
      if(d%%100==0) cat(".")
    }
  }
  if(interactions & int.type=="phi") {
    cat("Starting Interactions...\n This may take a while...it prints a dot every 100 docs. \n")
    logbeta <- mod$beta$logbeta
    #we want to store the contributions aggregated over word
    diagnostic <- vector(mode="list", length=length(index))
    for(i in 1:length(diagnostic)) {
      diagnostic[[i]] <- rep(0,mod$settings$dim$V)
    }
    agg.wcounts <- rep(0, length(index))
    bindex <- mod$settings$covariates$betaindex
    for(d in 1:length(docs)) {
      expeta <- c(exp(mod$eta[d,]),1)
      #pieces for the derivatives of the exp(eta)beta part
      EB <- expeta*exp(logbeta[[bindex[d]]][,docs[[d]][1,],drop=FALSE]) #calculate exp(eta)\beta for each word
      EB <- t(EB)/colSums(EB) #transpose and norm by (now) the row

      #at this point EB is the phi matrix
      phi <- t(EB*(docs[[d]][2,])) #multiply through by word count
      for(a in 1:length(index)) {
        #create a sparse K by V matrix containing interaction components
        kappamat <- Matrix(do.call(rbind,kappa[(K+ length(index) + (a-1)*K + 1):(K+ length(index) + a*K)]))
        kappamat <- kappamat[,docs[[d]][1,]]
        word.proj <- Matrix::colSums(kappamat*phi)
        #add in contributions from interaction and word counts
        diagnostic[[a]][docs[[d]][1,]] <-diagnostic[[a]][docs[[d]][1,]] +  word.proj
        agg.wcounts[a] <- agg.wcounts[a] + sum(docs[[d]][2,])
        #elementwise multiply and sum
        #now add it to the existing projection
        proj[[a]][d] <- proj[[a]][d] + sum(word.proj)/sum(docs[[d]][2,])
      }
      if(d%%100==0) cat(".")
    }}
  if(interactions & int.type=="theta") {
    cat("Starting Interactions...\n This may take a while...it prints a dot every 100 docs. \n")
    diagnostic <- vector(mode="list", length=length(index))
    for(i in 1:length(diagnostic)) {
      diagnostic[[i]] <- rep(0,mod$settings$dim$V)
    }
    agg.wcounts <- rep(0, length(index))
    for(d in 1:length(docs)) {
      theta <- mod$theta[d,]
      wct <- matrix(rep(docs[[d]][2,], each=K), nrow=K)
      theta.wct <- wct*theta
      for(a in 1:length(index)) {
        #create a sparse K by V matrix containing interaction components
        kappamat <- Matrix::Matrix(do.call(rbind,kappa[(K+ length(index) + (a-1)*K + 1):(K+ length(index) + a*K)]))
        kappamat <- kappamat[,docs[[d]][1,]]
        word.proj <- Matrix::colSums(kappamat*theta.wct)
        #add in contributions from interaction and word counts
        diagnostic[[a]][docs[[d]][1,]] <-diagnostic[[a]][docs[[d]][1,]] +  word.proj
        agg.wcounts[a] <- agg.wcounts[a] + sum(docs[[d]][2,])
        #elementwise multiply and sum
        #now add it to the existing projection
        proj[[a]][d] <- proj[[a]][d] + sum(word.proj)/sum(docs[[d]][2,])
      }
      if(d%%100==0) cat(".")
    }}
  if(!interactions) {
    cat(sprintf("\n Calculation Complete in %.2f seconds",(proc.time() - t1)[3]))
    return(proj)
  } else {
    for(a in 1:length(index)) {
      diagnostic[[a]] <- diagnostic[[a]]/agg.wcounts[a]
    }
    return(list(projection=proj, diagnostic=diagnostic))
  }
}
