#' Get reference rates from konsumenternas
#'
#' @param trade_date date of trade, default = today
#'
#' @return tuyi
#' @export
#'
#' @examples
#' get_ref_rates()
#'
#'
#' @importFrom httr2 request req_perform resp_body_string
#' @importFrom dplyr mutate across
#' @importFrom memoise memoise

get_ref_rates <- memoise(function(trade_date = yesterday()) {
  string <-
  request(glue::glue("https://konsumenternassixdatarse-24-1-0-standard-prod.capitex.vitec.net/j/FetchSecurities?datum={trade_date}&version=V2&callback=data")) %>%
    req_perform() %>%
    resp_body_string() %>%
    stringi::stri_unescape_unicode() %>%
    stringr::str_remove("^data..") %>%
    stringr::str_remove("..$")

  xml2::read_xml(string) %>%
    xml2::as_list() %>%
    tibble::as_tibble() %>%
    tidyr::unnest_wider(DocumentElement) %>%
    tidyr::unnest(cols = c(Date, ISINCODE, Instype, Maturity, Fixingrta)) %>%
    tidyr::unnest(cols = c(Date, ISINCODE, Instype, Maturity, Fixingrta)) %>%
    mutate(across(.cols = tidyselect::everything(),
                  .fns = stringr::str_trim)) %>%
    mutate(across(.cols = c(Date, Maturity),
                  .fns = as.Date)) %>%
    mutate(across(.cols = c(Fixingrta),
                  .fns = as.numeric))

})
