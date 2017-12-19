#' Blood samples from an athlete convicted of doping.
#'
#' A dataset containing the result of the analysis of 13 blood samples,
#' taken over 5 years, of a female athlete who was convicted of doping
#' on the basis of the Athlete Biological Passport.
#'
#' @format A data frame with 13 rows and 11 variables (including 10
#' haematological variables):
#' \describe{
#'   \item{date}{the date of test}
#'   \item{HCT}{haematocrit [\%]}
#'   \item{HGB}{haemoglobin [g/dl]}
#'   \item{MCH}{mean corpuscular haemoglobin [pg]}
#'   \item{MCHC}{mean corpuscular haemoglobin concentration [g/dl]}
#'   \item{MCV}{mean corpuscular volume [fl]}
#'   \item{RBC}{red blood cell count [10^6/ul]}
#'   \item{RETC}{reticulocyte count [10^6/ul]}
#'   \item{RETP}{reticulocyte percentage [\%]}
#'   \item{OFFscore}{Off-score}
#'   \item{ABPS}{Abnormal blood profile score}
#' }
#'
#' In November 2012, the athlete was convicted of doping by the Court
#' of arbitration for Sport, the evidence showing at least two occurences
#' of doping, in summer 2009 (shortly before the test of 2 July 2009) and
#' in June 2011, likely using an agent such as erythropoietin (EPO).
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
#' obtained using the \code{ABPS} function, but they were likely
#' calculated using different (older) version of the ABPS code, which
#' explains some small changes. 
#' 
#' @source \url{https://jurisprudence.tas-cas.org/Shared\%20Documents/2773.pdf}
"blooddoping"

#' Blood samples from different athletes.
#'
#' A dataset containing the result of the analysis of 13 blood samples
#' from different athletes.
#'
#' @format A data frame with 13 rows and 12 haematological variables:
#' \describe{
#'   \item{HCT}{Haematocrit [\%]}
#'   \item{HGB}{Haemoglobin [g/dl]}
#'   \item{IRF}{Immature reticulocyte fraction [\%]}
#'   \item{MCH}{Mean corpuscular haemoglobin pg}
#'   \item{MCHC}{Mean corpuscular haemoglobin concentration g/dl}
#'   \item{MCV}{Mean corpuscular volume [fL]}
#'   \item{RBC}{Eed blood cell count 10^6/ul}
#'   \item{RDW.SD}{Red blood cell distribution width [fL]}
#'   \item{RETC}{Reticulocyte count 10^6/ul}
#'   \item{RETP}{Reticulocyte percentage \%}
#'   \item{OFFscore}{Off-score}
#'   \item{ABPS}{Abnormal blood profile score}
#' }
#'
#' Based on the values, there is no indication that these athletes
#' were doped.
#'
#' @section Note: One of the rows does not actually belong to an
#'     athlete, but to one of the authors of this package, who promises
#'     that he was not doped.
#'
#' @source Swiss Laboratory for Doping Analyses (LAD), with some calculations
#' performed by the World Anti-Doping Agency (WADA).
"bloodcontrol"
