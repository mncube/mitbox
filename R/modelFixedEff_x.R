#' modelFixedEff_x
#'
#' Adds to the output of merTools::modelFixedEff
#'
#' The statistical literature suggests using the median p-value when conducting
#' logistic regression on multiple imputation data:
#'
#' "In context of significance testing for logistic regression, Eekhout, Wiel,
#' and Heymans (2017) suggest taking the median of the mp-values as the combined
#' p-value, an exceedingly simple method. It nevertheless appears to outperform
#' more sophisticated techniques if the variable to be tested is categorical
#' with more than two categories. It would be useful to explore whether this
#' median P rule has wider validity."
#'
#' https://stefvanbuuren.name/fimd/sec-multiparameter.html#sec:chi
#'
#'
#' @param modList a merModList object
#' @param exponentiate if TRUE, coefficients will be exponentiated
#' @param ... pass arguments to tidy
#'
#' @return output of merTools::modelFixedEff plus: median p-value
#' @export
modelFixedEff_x <- function(modList, exponentiate = TRUE, ...){

  #Create visible binding for globals
  med.p.value <- NULL
  p.value <- NULL
  term <- NULL

  #Run merTools::modelFixedEff
  fixEst <- merTools::modelFixedEff(modList, exponentiate= exponentiate, ...)

  #Get the median p-value from modList object
  fixEst_x <- lapply(modList, broom.mixed::tidy,effects = "fixed",
                     exponentiate= exponentiate, ...)

  fixEst_x <- do.call(rbind, fixEst_x) %>%
    dplyr::group_by(term) %>%
    dplyr::summarise(med.p.value = stats::median(p.value)) %>%
    dplyr::ungroup() %>%
    dplyr::select(term, med.p.value)

  #Join med.p.value with modelFixedEff output
  fixEst <- fixEst %>%
    dplyr::left_join(fixEst_x, by = c("term"))

  #Return results
  return(fixEst)
}
