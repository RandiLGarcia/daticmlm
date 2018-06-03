#' @name testvar
#' @title Significance Test for (some) Random Effects
#'
#' Calculates the p-value for the test of the slope variance and intercept-slope covariance using the likelihood ratio test method.
#' 
#'
#' @param outp is the lmer model object. 
#' @param p is string of the variance component you want tested (place in double quotes).
#' @param testcov is FALSE by default for the variance component. If testcov = TRUE, the function retuns the test of the intercept-component covariance.
#'
#' @details 
#' @export
#'
#
testvar <- function(outp, p = "intercept", testcov = FALSE){
  require(stringr)
  
  callStuff <- as.character(outp@call)
  dat <- get(callStuff[3])
  rdat <- data.frame(VarCorr(outp))
  ran_list <- rdat[nrow(rdat)-1, c(1:nrow(rdat)-1)]
  out <- str_remove_all(substr(callStuff[2], 1, str_locate(callStuff[2], "~")-1), " ")
  
  fmr <- str_remove_all(substr(callStuff[2], str_locate(callStuff[2], "\\("), str_locate(callStuff[2], "\\)")), " ")
  fmf <- str_remove_all(substr(callStuff[2], 1, str_locate(callStuff[2], "\\(")-1), " ")
  fmr_omit <- str_remove(fmr, str_c("\\+", p))
  fmr_sep <- str_c(fmr_omit,"+","(",p,"-1","|", ran_list$grp[1],")")
  
  form <- formula(str_c(fmf, fmr, collapse = ""))
  form_omit <- formula(str_c(fmf, fmr_omit, collapse = ""))
  form_sep <- formula(str_c(fmf, fmr_sep, collapse = ""))
  
  mod_full <- lmer(form, data = dat)
  mod_omit <- lmer(form_omit, data = dat)
  mod_sep <- lmer(form_sep, data = dat)
  
  #needs a routine for creating the formulas for when the intercept is removed
  if(p == "intercept"){
    # fmr_omit <- str_remove(fmr, str_c("\\+", p))
    # fmr_sep <- str_c(fmr_omit,"+","(",p,"-1","|", ran_list$grp[1],")"
    # form_omit <- formula(str_c(fmf, fmr_omit, collapse = ""))
    # form_sep <- formula(str_c(fmf, fmr_sep, collapse = ""))
    # 
    # mod_omit <- lmer(form_omit, data = dat)
    # mod_sep <- lmer(form_sep, data = dat)
  }
  
  pvalCorrected <- function(chisq, df){
    (pchisq(chisq, df, lower.tail=FALSE) + pchisq(chisq, df - 1, lower.tail=FALSE))/2
  }
  
  if(testcov){
    fff <- -2*logLik(mod_sep)[1]+2*logLik(mod_full)[1]
    pval <- pvalCorrected(as.numeric(fff), 1)
    
    return(data.frame(Var1 = ran_list$var1[1],
                      Var2 = ran_list$var2[1],
                      Chi_Squared = fff,
                      df = 1,
                      p_value = pval))
  }
  
  fff <- -2*logLik(mod_omit)[1]+2*logLik(mod_sep)[1]
  pval <- pvalCorrected(as.numeric(fff), 1)
  
  return(data.frame(Outcome = out,
                    VarComponent = p,
                    Chi_Squared = fff,
                    df = 1,
                    p_value = pval))
}