#' Pre-post cognitive testing scores
#'
#' Paired pre-post data on a set of cognitive tests
#'
#' @format ## `paireddata`
#' A data frame with 28 rows and 13 columns:
#' \describe{
#'   \item{MTTICMD}{The difference between the median latency of response (from stimulus appearance to button press) on the trials that were congruent versus the trials that were incongruent.}
#'   \item{MTTLMD}{The median latency of response (from stimulus appearance to button press).}
#'   \item{MTTMTCMD}{The difference between the median latency of response (from stimulus appearance to button press) during assessed blocks in which both rules are used versus assessed blocks in which only a single rule is used}
#'   \item{MTTTIC}{The number of trials for which the outcome was an incorrect response (subject pressed the incorrect button within the response window).}
#'   \item{PALFAMS}{The number of times a subject chose the correct box on their first attempt when recalling the pattern locations.}
#'   \item{PALTEA}{The number of times the subject chose the incorrect box for a stimulus on assessment problems (PALTE), plus an adjustment for the estimated number of errors they would have made on any problems, attempts and recalls they did not reach.}
#'   \item{RTIFMDMT}{The median time taken for a subject to release the response button and select the target stimulus after it flashed yellow on screen.}
#'   \item{RTIFMDRT}{The median duration it took for a subject to release the response button after the presentation of a target stimulus.}
#'   \item{RTISMDMT}{The median time taken for a subject to release the response button and select the target stimulus after it flashed yellow on screen.}
#'   \item{RTISMDRT}{The median duration it took for a subject to release the response button after the presentation of a target stimulus.}
#'   ...
#' }
#'
"paireddata"




#' Data for SUV and cognitive scores (ESKD vs controls)
#'
#' Data from a pilot study comparing ESKD patients to controls
#' on SUV and a set of cognitive variables
#'
#' @format ## `groupdata`
#' A data frame with 20 rows and 17 columns:
#' \describe{
#'   \item{id}{Id variables}
#'   \item{group}{Group indicator, 0:Control, 1:ESKD}
#'   \item{age}{Age}
#'   \item{sex}{Sex}
#'   \item{bmi}{Body Mass Index}
#'   \item{mean_suv}{Standardized Uptake Values from SPECT-CT}
#'   \item{blind_moca_uncorrected}{Montreal Congitive Assessment (MoCA)}
#'   \item{craft_verbatim}{}
#'   \item{craft_delay_verbatim}{}
#'   \item{number_span_forward}{}
#'   \item{number_span_backward}{}
#'   \item{fluency_f_words_correct}{}
#'   \item{oral_trail_part_a}{}
#'   \item{oral_trail_part_b}{}
#'   \item{fluency_animals}{}
#'   \item{fluency_vegetables}{}
#'   \item{verbal_naming_cues}{}
#'   ...
#' }
#' @source <https://www.who.int/teams/global-tuberculosis-programme/data>
"groupdata"
