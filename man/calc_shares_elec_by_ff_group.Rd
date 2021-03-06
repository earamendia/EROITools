% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers_shares.R
\name{calc_shares_elec_by_ff_group}
\alias{calc_shares_elec_by_ff_group}
\title{Calculates the share of electricity supplied by each fossil fuel within each fossil fuel group}
\usage{
calc_shares_elec_by_ff_group(
  .tidy_iea_df,
  supply_mats_list = c(IEATools::psut_cols$V),
  matnames = IEATools::mat_meta_cols$matnames,
  product.group = "Product.Group",
  share = "Share",
  energy.stage = "Energy.stage",
  country = IEATools::iea_cols$country,
  method = IEATools::iea_cols$method,
  energy_type = IEATools::iea_cols$energy_type,
  last_stage = IEATools::iea_cols$last_stage,
  year = IEATools::iea_cols$year,
  unit = IEATools::iea_cols$unit,
  product = IEATools::iea_cols$product,
  e.dot = IEATools::iea_cols$e_dot,
  product_without_origin = "product_without_origin",
  non_energy_uses = "Non_Energy_Uses"
)
}
\arguments{
\item{country, method, energy_type, last_stage, year, unit, product, e_dot}{See \code{IEATools::iea_cols}.}
}
\value{

}
\description{
This function calculates the share of electricity supplied by each fossil fuel within each fossil fuel group,
for the two following fossil fuel groups "Oil and gas products", and "All fossil fuels". So it gives the share supplied
by respectively Coal products, Oil products, and Natural gas, in each of those groups. The shares then add up to unity necessarily.
}
