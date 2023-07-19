#' Calculate the comparison rate (jämförelseränta)
#'
#' @param date date
#'
#' @return comparison rate (double)
#' @export
#'
#' @examples
#' calculate_comp_rate(Sys.Date() + 89:91)
#'
#'
calculate_comp_rate <- Vectorize(function(date) {
  general_rate_level(date) + 1
})
