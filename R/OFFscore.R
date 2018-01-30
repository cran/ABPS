#' function OFFscore
#' 
#' The \code{OFFscore} function computes the value of the OFF-hr
#' score (or OFF score), a combination of the haemoglobin level and
#' the percentage of reticulocytes, used for detecting blood doping.
#' 
#' @param haemdata a vector or data frame containing (at least) the 2
#'     haematological variables, either with the same names as the
#'     parameters below, or (not recommended) without names but in the
#'     same order as the parameters.
#' @param HGB level of haemoglobin HGB [g/dL]
#' @param RETP percentage of reticulocytes [\%]
#'
#' @return value of the OFF score.
#'
#' @details
#' 
#' The OFF-hr score is defined as 10*HGB [g/dL] - 60*sqrt(RETP [\%]).
#'
#' It is one of the parameters of the Athlete Biological Passport
#' (ABP) program managed by the World Anti-Doping Agency (WADA), and
#' is routinely used to identify athletes who use a substance
#' prohibited by anti-doping rules (see
#' e.g. \url{https://jurisprudence.tas-cas.org/Shared\%20Documents/4006.pdf}).
#'
#' The rationale for using this score for detecting blood doping is
#' the following: if a manipulation (use of transfusion or of
#' erythropoietic stimulating agents (ESA) such as recombinant human
#' erythropoietin (rhEPO)) increases the number of circulating red
#' cells (and thus increases the level of haemoglobin), the organism
#' will react by stopping its own production of red blood cells. This
#' negative feedback will be observed in the reduced percentage of
#' reticulocytes (immature red blood cells). Such a combination of
#' elevated HGB and reduced RETP, which will produce a high OFF
#' score, is found neither naturally, nor as the consequence of a
#' medical condition, and is thus indicative of doping.
#'
#' The OFF score will pick up both withdrawal of blood (which induces
#' a reduction in haemoglobin, a rise in reticulocytes, and thus a
#' rise of RETP and a reduction of OFF score), and its re-infusion
#' (HGB concentration increases, number of reticulocytes and RETP
#' decrease, and OFF score increases).
#'
#' @section Note:
#'
#' The values for HGB and RETP can be specified using either a data
#' frame containing (at least) the 2 haematological variables, or
#' using two named parameters, but not both at the same time.
#'
#' The original OFF-hr score, as described by Gore et al., expects
#' the haemoglobin level HGB to be specified in g/L. In order to be
#' coherent with other functions in the package, this function assumes
#' that HGB is specified in g/dL, and will multiply the value by 10.
#' A warning will be emitted if units used seem wrong.
#'
#' @section References:
#' Gore, C.J., R. Parisotto, M.J. Ashenden, et al., Second-generation
#' blood tests to detect erythropoietin abuse by athletes.
#' Haematologica, 2003. 88(3): p. 333-44.
#'
#' @examples
#' OFFscore(HGB=14.6, RETP=0.48)
#'
#' data(blooddoping)
#' OFFscore(HGB=blooddoping$HGB, RETP=blooddoping$RETP)
#' 
#' @export

OFFscore <- function(haemdata=NULL, HGB=NULL, RETP=NULL){

  haemnames <-  c("HGB", "RETP")

  if (is.null(haemdata)) {
    haemdata <- cbind(HGB, RETP)

    if (is.null(haemdata))
      stop("OFFscore requires either a data frame or 2 variables.")

  } else {
    if (!is.null(cbind(HGB, RETP)))
      stop("OFFscore requires either a data frame or separate variables, but not both.")

  }

  # Make sure the data is in a data frame, even if we have only one
  # data point and it is in a vector.
  if (is.null(dim(haemdata))) {
    haemdata <- t(haemdata)
  }

  if (is.null(colnames(haemdata)) && ncol(haemdata)==2)
    colnames(haemdata) <- haemnames

  if (any(is.na(match(haemnames, colnames(haemdata)))))
    stop("ABPS requires 2 haematological variables.")

  # Select only the 2 variables we are interested in, in case there were
  # more in the data, and sort them in the right order.
  haemdata <- haemdata[, haemnames, drop=FALSE]

  # Very crude range checking for the most common mistake in units
  if (any(haemdata[,"HGB"]>50))
    warning("OFF-score: very high values for HGB; are the units correct ?")
    
  return(as.vector(10*haemdata[,"HGB"] - (60*sqrt(haemdata[,"RETP"]))))
}
