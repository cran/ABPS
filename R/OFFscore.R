#' function OFFscore
#' 
#' The \code{OFFscore} function computes the value of the OFF-hr score,
#' a combination of the haemoglobin level and the percentage of
#' reticulocytes, used for detecting blood doping.
#' 
#' @param HGB the level of haemoglobin HGB [g/L] 
#' @param RETP the percentage of reticulocytes [\%]
#' @return the value of the OFF score.
#'
#' @details
#' 
#' The OFF-hr score is defined as HGB [g/L] - 60*sqrt(RETP [\%]). Note
#' that the units for the HGB level are not the same as the ABPS
#' function (g/l vs g/dl, a ten-fold difference).
#'
#' It is one of the parameters of the Athlete Biological Passport
#' (ABP) program managed by the World Anti-Doping Agency (WADA), and
#' is routinely used to identify athletes who use a substance
#' prohibited by anti-doping rules (see
#' e.g. \url{https://jurisprudence.tas-cas.org/Shared\%20Documents/4006.pdf}).
#'
#' The rationale for using this score for detecting blood doping is
#' the following: if a manipulation (use of transfusion or of
#' erythropoietic stimulating agents (ESA) such as erythropoietin
#' (EPO)) increases the number of circulating red cells (and thus
#' increases the level of haemoglobin), the organism will react by
#' stopping its own production of red blood cells. This negative
#' feedback will be observed in the reduced percentage of
#' reticulocytes (immature red blood cells). Such a combination of
#' elevated HGB and reduced RET\%, which will produce a high OFF
#' score, is found neither naturally, nor as the consequence of a
#' medical condition, and is thus indicative of doping.
#'
#' The OFF score will pick up both withdrawal of blood (which induces
#' a reduction in haemoglobin, a rise in reticulocytes, and thus a
#' rise of RET\% and a reduction of OFF score), and its re-infusion
#' (HGB concentration increases, number of reticulocytes and RET\%
#' decrease, and OFF score increases).
#'
#' @section Note:
#' Since the expected units for the HGB level are not the same as the
#'     ABPS function (g/l vs g/dl, a ten-fold difference), a warning
#'     will be emitted if it seems like the wrong units are used.
#'
#' @section References:
#' Gore, C.J., R. Parisotto, M.J. Ashenden, et al., Second-generation
#' blood tests to detect erythropoietin abuse by athletes.
#' Haematologica, 2003. 88(3): p. 333-44.
#'
#' @examples
#' OFFscore(HGB=146, RETP=0.48)
#'
#' data(blooddoping)
#' # Note that HGB is measured in g/l in the blooddoping and bloodcontrol
#' # datasets; it must first be converted to g/dl (a factor 10) before
#' # being used in the OFF score calculation.
#' OFFscore(blooddoping$HGB * 10, blooddoping$RETP)
#' 
#' @export

OFFscore <- function(HGB, RETP){

  # Very crude range checking for the most common mistake in units
  if (any(HGB<50))
    warning("OFF-score: very low values for HGB; are the units correct ?")
    
  return(HGB - (60*sqrt(RETP)))
}