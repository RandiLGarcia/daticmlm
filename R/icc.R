#' @name icc
#' @title Intraclass Correlation
#'
#' Calculate the Intaclass Corrlation (iCC).
#' 
#'
#' @param mod is the lmer model object. For exmaple, summary(mod).
#'
#' @details 
#' @export
#'
#
icc <- function(mod){ 
  dat <- as.data.frame(VarCorr(mod))
  dat[1,4]/(dat[1,4] + dat[2,4])
}