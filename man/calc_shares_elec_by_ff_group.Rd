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
\item{.tidy_iea_df}{The name of the \code{.tidy_iea_df} for which the share of electricity supplied by each fossil fuel need to be calculated.}

\item{supply_mats_list}{The list of the supply matrices to be used for calculating the electricity supply mix.
Default is \code{c(IEATools::psut_cols$V)}.}

\item{matnames}{The name of the column containing matrices names.
Default is "matnames".}

\item{product.group}{The name of the column containing the product group name.
Default is "Product.Group".}

\item{share}{The name of the column containing the shares of each fossil fuel within the electricity supply.
Default is "Share".}

\item{energy.stage}{The name of the column containing the energy stage.
Default is "Energy.stage".}

\item{country, method, energy_type, last_stage, year, unit, product, e.dot}{See \code{IEATools::iea_cols}.}

\item{product_without_origin}{The name of the column containing the product name without the product origin.
Default is "product_without_origin".}

\item{non_energy_uses}{The name of the column stating whether non-energy uses are included in the calculation of the shares of supply.
Default is "Non_Energy_Uses".}
}
\value{
A \code{.tidy_iea_df} with the shares of electricity supply specified by fossil fuel group.
}
\description{
This function calculates the share of electricity supplied by each fossil fuel within each fossil fuel group,
for the two following fossil fuel groups "Oil and gas products", and "All fossil fuels". So it gives the share supplied
by respectively Coal products, Oil products, and Natural gas, in each of those groups. The shares then add up to unity necessarily.
}
\examples{
ECCTools::tidy_AB_data \%>\% 
   IEATools::add_psut_matnames() \%>\% 
   ECCTools::transform_to_dta() \%>\% 
   calc_shares_elec_by_ff_group()
}
