general_rate_level <- function(date) {
  gov_bond_level(date) + mortgage_bonds_addition(date)
}
