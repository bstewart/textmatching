#' Content Levels
#'
#' A helper function which returns the levels of the content covariate in an stm model.  Returns \code{NULL}
#' if the model is not a content covariate model.
#'
#' @param stm_model the stm content covariate model
#' @return character vector containing levels of the content covariate.
#' @references
#' Roberts, M., Stewart, B., Nielsen, R. (2020)
#' "Adjusting for Confounding with Text Matching." 
#' In American Journal of Political Science
#' @examples
#' \donttest{
#' data(sim)
#' content_levels(sim_topics)
#' }
#' @export
content_levels <- function(stm_model) {
  return(stm_model$settings$covariates$yvarlevels)
}