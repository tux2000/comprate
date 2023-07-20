#' Export comparison rates to json file
#'
#' @param offsets vector of offsets (days from today)
#' @param file output file
#'
#' @return Nothing, used for side effects
#' @export
#'

export_json <- function(offsets, file) {
  df <- data.frame(offset = offsets) %>%
    mutate(date = Sys.Date() + .data[["offset"]]) %>%
    mutate(comparison_rate = calculate_comp_rate(.data[["date"]]))

  jsonlite::write_json(df, file)
}
