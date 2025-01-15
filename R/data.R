#' @title Emerging Infectious Network (EIN) survey data
#'
#' @description Responses to survey questions about advanced molecular
#' diagnostic testing for infectious diseases presented to medical personnel
#' who are members of EIN.
#'
#' @format ## `ein`
#' A `data.frame` with 643 observations and 3 variables.
#' \describe{
#'  \item{`wave`}{The data collect period the responses were collected
#'  during.}
#'  \item{`q1`}{The responses to the types of advanced molecular
#'  diagnostic tests that have been ordered or used. This is an example of
#'  responses based on select that all apply data collection process. Possible
#'  values are *Broad range 16S rRNA gene sequencing* (`Broadrange`), *Whole
#'  genome sequencing* (`Wholegenome`), *Metagenomic next-generation sequencing*
#'  (`MNGS`), *None of the above* (`None`), and *Other* (`Other`), where the
#'  data values are listed within the parentheses. When multiple responses are
#'  present the data values are separated by `"|"`.}
#'  \item{`q2`}{The responses to how often within the past two years
#'  advanced molecular diagnostic testing was performed. Possible values are
#'  *Not at all*, *Once*, *Rarely* (quarterly or less often), *Sometimes*
#'  (monthly), and *Often* (weekly). This is an example of where the responses
#'  collected come from the multinomial distribution.}
#' }
#'
#' @source [Difficult Diagnoses](https://ein.idsociety.org/surveys/survey/166/)
#' @usage data(ein)
#' @examples
#' data(ein)
#' str(ein)
"ein"
