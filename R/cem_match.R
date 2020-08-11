#' Match using CEM
#'
#' A wrapper around CEM to match on refit topics and projection.
#'
#' This function is a convenience wrapper around CEM which returns a \pkg{cem} object.
#'
#' @param theta an output of \code{\link{refit}} containing the refit topic proportions, theta.
#' If given the argument \code{NULL} the function will omit matching on topics.
#' @param projection an output of \code{\link{project}} containing the projection. If given
#' the argument \code{NULL} the function will omit matching on the projection.
#' @param treat the treatment variable.
#' @param topic_breaks the cutpoints used for the topics. If this is a vector it defines the 
#' vector of cutpoints.  If this is a single number, it determines the number of bins.  By default
#' it uses the cutpoints \code{0,.1,1} which corresponds to bins of less than 10% about a topic
#' and more than 10\% about a topic.
#' @param projection_breaks the cutpoints used for the projection. If this is a vector it defines the 
#' vector of cutpoints.  If this is a single number, it determines the number of bins.  By default
#' it uses 5 inductively learned bins. See \pkg{cem} for more details.
#' @param returnX a logical which if true adds a copy of the data to the return object as a top level
#' item in the list called \code{X}.
#' @param ... additional arguments passed to \link[cem]{cem}
#' @return a \code{cem.match} object
#' @examples
#' \donttest{
#' #See ?sim for a walkthrough
#' data(sim)
#' refitted <- refit(sim_topics, sim_documents, content_level="1")
#' projection <- project(sim_topics, sim_documents)
#' matched <- cem_match(refitted,projection=projection, sim_meta$treat,
#'                      projection_breaks=2)
#' }
#' @export
cem_match <- function(theta, projection, treat, 
                      topic_breaks=c(0,.1,1), projection_breaks=5,
                      returnX=FALSE,...) {
  breaks <- vector(mode="list")
  if(!is.null(theta)) {
    if(!inherits(theta, "matrix")) theta <- theta$theta
    K <- ncol(theta)
    theta <- data.frame(theta)
    for(i in 1:K){
      name <- paste("Topic", i, sep="")
      colnames(theta)[i] <- name
      breaks[[name]] <- topic_breaks
    }
  }
  if(!is.null(projection)) {
    projection <- data.frame(do.call(cbind,projection$projection))
    A <- ncol(projection)
    for(i in 1:A) {
      name <- paste("Proj", i, sep="")
      colnames(projection)[i] <- name
      breaks[[name]] <- projection_breaks
    }
  }
  dat <- data.frame(treat=treat)
  if(!is.null(theta)) dat <- cbind(dat,theta)
  if(!is.null(projection)) dat <- cbind(dat,projection)
  matched <- cem::cem("treat", dat, cutpoints=breaks,...)
  if(returnX) matched$X <- dat
  return(matched)
}
