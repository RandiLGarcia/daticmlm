#' @name rsq1
#' @title R-Squared for Level-1
#'
#' Calculates the proportion of level-1 vairance explained by the model.
#' 
#'
#' @param base is the simpler lmer model object. 
#' @param mod is the more complex lmer model object.
#'
#' @details 
#' @export
#'
#
rsq1 <- function(base, mod){
  datb <- as.data.frame(VarCorr(base))
  datm <- as.data.frame(VarCorr(mod))
  r2 <- (datb[nrow(datb),4]-datm[nrow(datm),4])/(datb[nrow(datb),4])
  if(r2 < 0){return(0)}
  return(r2)
}