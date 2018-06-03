#' @name deviance
#' @title Calculate the Deviance
#'
#' Calculates the deviance from the likelihood.
#' 
#'
#' @param mod is the lmer model object. For exmaple, summary(mod).
#'
#' @details 
#' @export
#'
#
deviance <- function(mod){
  as.numeric(-2*logLik(mod))
}