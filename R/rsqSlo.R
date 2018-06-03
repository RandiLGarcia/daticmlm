#' @name rsqSlo
#' @title R-Squared for Slope Variance
#'
#' Calculates the proportion of slope vairance explained by the model.
#' 
#'
#' @param base is the simpler lmer model object. 
#' @param mod is the more complex lmer model object.
#' @param par is the parameter number for the slope variance you want to test.
#'
#' @details 
#' @export
#'
#
rsqSlo <- function(base, mod, par = 2){
  datb <- as.data.frame(VarCorr(base))
  datm <- as.data.frame(VarCorr(mod))
  r2 <- (datb[par,4]-datm[par,4])/(datb[par,4])
  if(r2 < 0){return(0)}
  return(r2)
}