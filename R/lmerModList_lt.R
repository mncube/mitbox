#' lmerModList_lt
#'
#' Like lmerModList from the merTools package but calls lmerTest::lmer intead of
#' lme4::lmer
#'
#' @param formula a formula to pass to lmer
#' @param data a list of data frames
#' @param parallel a logical variable indicating whether the models should be run
#' run in parallel
#' @param ... pass additional arguments to lmer
#'
#' @return a list of fitted merMod objects (the class merModList has been removed)
#' @export
lmerModList_lt <- function(formula, data, parallel = FALSE, ...){
  if(parallel) {
    if (requireNamespace("future.apply", quietly=TRUE)) {
      ml <- future.apply::future_lapply(data,
                                        function(d) lmerTest::lmer(formula,
                                                                   data = d,
                                                                   ...))
    }
    warning("Parallel set but future.apply not available. Running sequentially.")
    ml <- lapply(data, function(d) lmerTest::lmer(formula, data = d, ...))
  } else {
    ml <- lapply(data, function(d) lmerTest::lmer(formula, data = d, ...))
  }

  #class(ml) <- "merModList"
  return(ml)
}
