#' Glancer
#'
#' Call glance on a list of model fits
#'
#' @param obj A list of model fits
#'
#' @return A list of results from the broom.mixed:glance
#'
#' @export
glancer <- function(obj){
  if (methods::is(obj, "merModList")){
    lapply(obj, broom.mixed::glance)
  }
}
