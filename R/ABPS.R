#' A function for calculating the Abnormal Blood Profile Score
#'
#' The \code{ABPS} function computes the Abnormal Blood Profile Score
#' from seven haematological markers. Higher values of this composite
#' score are associated with a higher likelihood of blood doping;
#'
#' @param haemdata a vector or data frame containing (at least) the 7
#'     haematological variables, either with the same names as the
#'     parameters below, or (not recommended) without names but in the
#'     same order as the parameters.
#' @param HCT the haematocrit level (in \%)
#' @param HGB the haemoglobin level (in [g/l])
#' @param MCH the mean corpuscular haemoglobin (in [pg])
#' @param MCHC the mean corpuscular haemoglobin concentration (in
#'     [g/dL])
#' @param MCV Mean corpuscular volume (in [fL])
#' @param RBC the red blood cell count (in [10^6/mu l])
#' @param RETP the reticulocytes percent (in \%)
#' 
#' @return a vector containing the ABPS score(s). Scores between 0 and
#'     1 indicate a possible suspicion of doping, while scores above 1
#'     indicate that the likelihood of doping is higher than the
#'     likelihood of no-doping.
#' 
#' @details
#'
#' The ABPS uses the seven haematological variables (HCT, HGB, MCH,
#' MCHC, MCV, RBC, RET\%) in order to obtain a combined score. This
#' score is more sensitive to doping than the individual markers, and
#' allows the detection of several types of blood doping using a
#' single score.
#' 
#' The combined score is based on two classification techniques, a
#' naive Bayesian classifier and an SVM (Support Vector Machine). The
#' two models were trained using a database of 591 blood profiles
#' (including 402 control samples from clean athletes and 189 samples
#' of athletes who abused of an illegal substance); the two scores
#' were then combined using ensemble averaging to obtain the final
#' ABPS score.
#' 
#' The ABPS is part of the Athlete Biological Passport program managed
#' by the World Anti-Doping Agency. While it is not a primary marker
#' of doping, it has been used as corroborative evidence (see e.g.
#' \url{https://jurisprudence.tas-cas.org/Shared\%20Documents/2773.pdf})
#'
#' @section Note:
#'
#' The calculation of the ABPS depends on two sets of parameters, for
#' the two machine learning techniques (naive Bayesian classifier and
#' Support Vector Machine), which are provided in the package.
#' 
#' Each parameter must be in a prespecified range; parameters outside
#' this range are constrained to the min (respectively max) values.
#'
#' Note that several versions of the ABPS were developed (including
#' several different combinations of parameters). The version provided
#' in this package provides the same results as the WADA version
#' included in their ADAMS database. However, some values calculated
#' with other versions of the software have also been distributed (see
#' the help page for the \code{blooddoping} dataset for an example).
#' 
#' @section References:
#' Sottas, P.E., N. Robinson, S. Giraud, et al., Statistical classification of abnormal blood profiles in athletes. Int J Biostat, 2006. 2(1): p. 1557-4679.
#' 
#' \url{https://jurisprudence.tas-cas.org/Shared\%20Documents/2773.pdf}
#' 
#' @examples
#' ABPS(HCT=43.2, HGB=14.6, MCH=31.1, MCHC=33.8, MCV=92.1, RBC=4.69, RETP=0.48)
#' ABPS(data.frame(HCT=43.2, HGB=14.6, MCH=31.1, MCHC=33.8, MCV=92.1, RBC=4.69, RETP=0.48))
#' ABPS(c(43.2, 14.6, 31.1, 33.8, 92.1, 4.69, 0.48))
#' data(blooddoping); ABPS(blooddoping)
#' data(bloodcontrol); ABPS(bloodcontrol)
#' 
#' @export

ABPS <- function(haemdata=NULL, HCT=NULL, HGB=NULL, MCH=NULL, MCHC=NULL, MCV=NULL,
                 RBC=NULL, RETP=NULL) {

  haemnames <-  c("RETP","HGB","HCT","RBC","MCV","MCH","MCHC")
  
  if (is.null(haemdata)) {
    haemdata <- cbind(HCT, HGB, MCH, MCHC, MCV, RBC, RETP)
      
    if (is.null(haemdata))
       stop("ABPS requires either a data frame or 7 variables.") 
  }

  # Make sure the data is in a data frame, even if we have only one
  # data point and it is in a vector.
  if (is.null(dim(haemdata))) {
    haemdata <- t(haemdata) 
  }

  if (is.null(colnames(haemdata)) && ncol(haemdata)==7)
    colnames(haemdata) <- haemnames

  if (any(is.na(match(haemnames, colnames(haemdata)))))
    stop("ABPS requires 7 haematological variables.")

  # Select only the 7 variables we are interested in, in case there were
  # more in the data, and sort them in the right order.
  haemdata <- haemdata[, haemnames, drop=FALSE]
    
  # Values that are outside the bounds used by the scoring algorithm
  # get assigned the minimum (or maximum) value instead.
  haemdata <- t( apply( haemdata, MARGIN=1, FUN=pmax, bayespar_7$mima[,1]) )
  haemdata <- t( apply( haemdata, MARGIN=1, FUN=pmin, bayespar_7$mima[,2]) )
    
  # Computation of Bayes score
  # What we are doing on a single data point:
  #  index <- apply(haemvar > bayespar_7$xabs, MARGIN=1, function(x){ max(which(x)) })

  index <- apply(haemdata, MARGIN=1,
                 FUN=function(x) {
                     apply(x>bayespar_7$xabs, MARGIN=1,
                           function(x) { ifelse( any(is.na(x)), NA, max(which(x)) ) } )
                     }
                )

  # For values that were missing
  index[! is.finite(index)] <- NA

  #probneg <- diag(bayespar_7$yabsnegt[,index])
  #probpos <- diag(bayespar_7$yabspos[,index])

  probneg <- apply( index, MARGIN=2, FUN=function(x) { diag(bayespar_7$yabsnegt[,x]) } )
  probpos <- apply( index, MARGIN=2, FUN=function(x) { diag(bayespar_7$yabspos[,x]) } )
 
  # Classification: Naive Bayes score to compare posterior probability of
  # being positive and posterior probability of being negative
  bayesscore <- log( apply(probpos, MARGIN=2, FUN=prod) / apply(probneg, MARGIN=2, FUN=prod) )
    
  if (any(stats::na.omit(bayesscore>100)))
    warning("ABPS: Bayes score very large, results may be inaccurate.")
    
  # SVM score, using the parameters from the trained model provided
  rbf <- kernlab::rbfdot(sigma=0.5/svmpar_7$kerneloption^2)
  Kmat <- apply(haemdata, MARGIN=1,
                FUN=function(x) {
                    kernlab::kernelMatrix(rbf,
                                          x=(x-svmpar_7$me1)/svmpar_7$st1,
                                          y=svmpar_7$xsup)
                })

  svmscore <- t(Kmat) %*% svmpar_7$w + as.numeric(svmpar_7$bsvm)
  svmscore <- t(svmscore)
    
  # Ensemble averaging of the two scores
  score <- ( 6*bayesscore/as.numeric(bayespar_7$stdb) + svmscore/as.numeric(svmpar_7$stds) )/4.75
  score <- as.vector(score)
    
  return(score)
}
