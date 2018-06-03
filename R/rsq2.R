#' @name rsq2
#' @title R-Squared for Level-2
#'
#' Calculates the proportion of level-2 vairance explained by the model.
#' 
#'
#' @param base is the simpler lmer model object. 
#' @param mod is the more complex lmer model object.
#'
#' @details 
#' @export
#'
#
rsq2 <- function(base, mod){
  datb <- as.data.frame(VarCorr(base))
  datm <- as.data.frame(VarCorr(mod))
  r2 <- (datb[1,4]-datm[1,4])/(datb[1,4])
  if(r2 < 0){return(0)}
  return(r2)
}