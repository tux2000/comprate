mortgage_bonds_addition <- memoise(function(date, ...) {
  date <- as.Date(date)

  ref_rates <- get_ref_rates(...)

  df_diff <- ref_rates %>%
    filter(.data[["Instype"]] == "BO") %>%
    mutate(gov_bond = gov_bond_level(.data[["Maturity"]])) %>%
    mutate(diff = .data[["Fixingrta"]] - .data[["gov_bond"]])

  model <- stats::lm(diff ~ Maturity, data = df_diff)

  stats::predict(model, data.frame(Maturity = date))

})
