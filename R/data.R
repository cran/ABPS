#' Blood samples from an athlete convicted of doping.
#'
#' A dataset containing the result of the analysis of 13 blood samples,
#' taken over a period of 5 years, of a female athlete who was convicted
#' of doping on the basis of the Athlete Biological Passport.
#'
#' @format A data frame with 13 rows and 11 variables (including 10
#' haematological variables):
#' \describe{
#'   \item{date}{the date of test}
#'   \item{HCT}{haematocrit [\%]}
#'   \item{HGB}{haemoglobin [g/dL]}
#'   \item{MCH}{mean corpuscular haemoglobin [pg]}
#'   \item{MCHC}{mean corpuscular haemoglobin concentration [g/dL]}
#'   \item{MCV}{mean corpuscular volume [fL]}
#'   \item{RBC}{red blood cell count [10^6/uL]}
#'   \item{RETC}{reticulocyte count [10^6/uL]}
#'   \item{RETP}{reticulocyte percentage [\%]}
#'   \item{OFFscore}{OFF-score}
#'   \item{ABPS}{Abnormal Blood Profile Score}
#' }
#'
#' In November 2012, the athlete was convicted of doping by the Court
#' of arbitration for Sport, the evidence showing at least two occurences
#' of doping, in summer 2009 (shortly before the test of 2 July 2009) and
#' in June 2011, likely using an agent such as recombinant erythropoietin
#' (rhEPO).
#'
#' Doping is indicated in particular by high haemoglobin values associated
#' with very low reticulocyte \% and high OFF scores (a combination of these
#' two variables).
#'
#' @section Note: The data tables published in the original source
#'     contain several typos, as confirmed by the World Anti-Doping
#'     Agency (WADA). In particular, values for RETP in rows 8 and 10
#'     were swapped and the MCHC value for row 7 was incorrect. This
#'     package's source code contains both the original data and the
#'     details of the corrections that were applied.
#'
#' The ABPS values provided are very close (within <2\%) to those
#' obtained using the \code{ABPS} function, but some of them were likely
#' calculated using different (older) versions of the ABPS code, which
#' may explain some of these differences. 
#' 
#' @source \url{https://jurisprudence.tas-cas.org/Shared\%20Documents/2773.pdf}
"blooddoping"

#' Blood samples from different individuals.
#'
#' A dataset containing the result of the analysis of 13 blood samples
#' from different individuals.
#'
#' @format A data frame with 13 rows and 12 haematological variables:
#' \describe{
#'   \item{HCT}{haematocrit [\%]}
#'   \item{HGB}{haemoglobin [g/dL]}
#'   \item{IRF}{immature reticulocyte fraction [\%]}
#'   \item{MCH}{mean corpuscular haemoglobin [pg]}
#'   \item{MCHC}{mean corpuscular haemoglobin concentration [g/dL]}
#'   \item{MCV}{mean corpuscular volume [fL]}
#'   \item{RBC}{red blood cell count [10^6/uL]}
#'   \item{RDW.SD}{red blood cell distribution width [fL]}
#'   \item{RETC}{reticulocyte count [10^6/uL]}
#'   \item{RETP}{reticulocyte percentage [\%]}
#'   \item{OFFscore}{OFF-score}
#'   \item{ABPS}{Abnormal Blood Profile Score}
#' }
#'
#' These samples are assumed to represent normal population.
#'
#' @section Note: One of the rows actually belongs to one of the
#'     authors of this package, who promises that he was not doped.
#'
#' @source Swiss Laboratory for Doping Analyses (LAD), with some calculations
#' performed by the World Anti-Doping Agency (WADA).
"bloodcontrol"
