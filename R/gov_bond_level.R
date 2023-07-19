#' Title
#'
#' @param date Date for which to calculate the government bond level
#' @param ... passed on to get_ref_rates()
#'
#' @examples
#' gov_bond_level(Sys.Date() + 90)
#'
#'
#' @export
#'
#' @importFrom dplyr filter

gov_bond_level <- Vectorize(function(date, ...) {
  date <- as.Date(date)

  ref_rates <- get_ref_rates(...)

  after <- ref_rates %>%
    dplyr::filter(Instype != "BO") %>%
    dplyr::filter(Maturity >= date) %>%
    dplyr::arrange(Maturity) %>%
    dplyr::slice(1)

  before <- ref_rates %>%
    dplyr::filter(Instype != "BO") %>%
    dplyr::filter(Maturity <= date) %>%
    dplyr::arrange(desc(Maturity)) %>%
    dplyr::slice(1)

  both <- dplyr::bind_rows(before, after) %>%
    dplyr::distinct()

  if(nrow(both) == 1){
    return(unlist(both[1, "Fixingrta"]))
  }

  both %>%
    dplyr::ungroup() %>%
    mutate(time_diff = abs(date - Maturity)) %>%
    mutate(weight = sum(time_diff) - time_diff) %>%
    dplyr::summarise(level = sum(Fixingrta * as.numeric(weight) /
                            as.numeric(sum(time_diff)))) %>%
    dplyr::pull(level)

})
