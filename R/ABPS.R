#' A function for calculating the Abnormal Blood Profile Score
#'
#' The \code{ABPS} function computes the Abnormal Blood Profile Score
#' from seven haematological markers. Higher values of this composite
#' score are associated with a higher likelihood of blood doping.
#'
#' @param haemdata a vector or data frame containing (at least) the 7
#'     haematological variables, either with the same names as the
#'     parameters below, or (not recommended) without names but in the
#'     same order as the parameters.
#' @param HCT haematocrit level [\%]
#' @param HGB haemoglobin level [g/dL]
#' @param MCH mean corpuscular haemoglobin [pg]
#' @param MCHC mean corpuscular haemoglobin concentration [g/dL]
#' @param MCV mean corpuscular volume [fL]
#' @param RBC red blood cell count [10^6/uL]
#' @param RETP reticulocytes percent [\%]
#' 
#' @return a vector containing the ABPS score(s). Scores between 0 and
#'     1 indicate a possible suspicion of doping; a score above 1 should
#'     only be found in 1 in 1000 male athletes.
#' 
#' @details
#'
#' The ABPS uses the seven haematological variables (HCT, HGB, MCH,
#' MCHC, MCV, RBC, RETP) in order to obtain a combined score. This
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
#' The values for the markers can be specified using either a data
#' frame containing (at least) the 7 haematological variables, or
#' using seven named parameters, but not both at the same time.
#'
#' The calculation of the ABPS depends on two sets of parameters, for
#' the two machine learning techniques (naive Bayesian classifier and
#' Support Vector Machine), which are provided in the package.
#' 
#' Each parameter must be in a prespecified range; parameters outside
#' this range are constrained to the min (respectively max) values and
#' a warning is printed. The limits are available in the
#' variable \code{ABPS:::bayespar_7$mima}
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

  # This is the order of names used in the objects containing
  # the model parameter
  haemnames <- c("RETP","HGB","HCT","RBC","MCV","MCH","MCHC")
  
  if (is.null(haemdata)) {
    haemdata <- cbind(HCT, HGB, MCH, MCHC, MCV, RBC, RETP)
      
    if (is.null(haemdata))
       stop("ABPS requires either a data frame or 7 variables.") 
  } else {
      # Make sure that the data was not specified in two different ways
      if (!is.null(cbind(HCT, HGB, MCH, MCHC, MCV, RBC, RETP)))
          stop("ABPS requires either a data frame or separate variables, but not both.")
  }

  # Make sure the data is in a data frame, even if we have only one
  # data point and it is in a vector.
  if (is.null(dim(haemdata))) {
    haemdata <- t(haemdata) 
  }

  # We assume that the variables have been specified in the same order as
  # the function call (but this way of passing variables is not recommended)
  if (is.null(colnames(haemdata)) && ncol(haemdata)==7)
    colnames(haemdata) <- c("HCT", "HGB", "MCH", "MCHC", "MCV", "RBC", "RETP")

  if (any(is.na(match(haemnames, colnames(haemdata)))))
    stop("ABPS requires 7 haematological variables.")

  # Select only the 7 variables we are interested in, in case there were
  # more in the data, and sort them in the right order.
  haemdata <- haemdata[, haemnames, drop=FALSE]
    
  # Values that are outside the bounds used by the scoring algorithm
  # (which depend on the values of the original dataset used to fit the
  # algorithm) get assigned the minimum (or maximum) value instead.
  haemdata.orig <- haemdata
  haemdata <- t( apply( haemdata, MARGIN=1, FUN=pmax, bayespar_7$mima[,1]) )
  haemdata <- t( apply( haemdata, MARGIN=1, FUN=pmin, bayespar_7$mima[,2]) )

  # If any parameter had to be adjusted, print a warning
  differences <- (haemdata.orig != haemdata)
  differences[is.na(differences)] <- FALSE
  if (any(differences)) {
    param_differences <- apply( differences, MARGIN=2, FUN=any )
    warning("some values were outside the bounds used by ABPS and were corrected: ",
            paste(haemnames[param_differences], collapse="," ) )
  }

  # Computation of Bayes score
  # bayespar_7 contains the parameters for the naive Bayesian classifier
  # The first "apply" executes the code over all observations
  # bayespar_7$xabs contains a split of the range of each parameter into
  # 500 steps. For each parameter, we find the bin that contains the actual
  # value
  index <- apply(haemdata, MARGIN=1,
                 FUN=function(x) {
                     apply(x>bayespar_7$xabs, MARGIN=1,
                           function(x) { ifelse( any(is.na(x)), NA, max(which(x)) ) } )
                     }
                )

  # Set the index to NA when a value was missing
  index[! is.finite(index)] <- NA

  # Using the index, we can find the positive and negative probabilites for
  # each parameter
  probneg <- apply( index, MARGIN=2, FUN=function(x) { diag(bayespar_7$yabsnegt[,x]) } )
  probpos <- apply( index, MARGIN=2, FUN=function(x) { diag(bayespar_7$yabspos[,x]) } )
 
  # Classification: Naive Bayes score to compare posterior probability of
  # being positive and posterior probability of being negative
  # For a given observation, the probabilities for the different parameters
  # are multiplied.
  bayesscore <- log( apply(probpos, MARGIN=2, FUN=prod) / apply(probneg, MARGIN=2, FUN=prod) )

  # This test is present in the original matlab code for ABPS, and was kept in
  # the R code for consistency.
  # However, based on the values obtained for all the possible combinations of
  # min/max values for all the parameters, the warning should actually never
  # be printed.
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
    
  # Ensemble averaging of the two scores. The values were determined at the
  # time the models were fitted.
  score <- ( 6*bayesscore/as.numeric(bayespar_7$stdb) + svmscore/as.numeric(svmpar_7$stds) )/4.75
  score <- as.vector(score)
    
  return(score)
}
