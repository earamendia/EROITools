% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/useful_stage.R
\name{calc_avg_efficiency_by_ff_group}
\alias{calc_avg_efficiency_by_ff_group}
\title{Calculates average final-to-useful efficiency by fossil fuel group}
\usage{
calc_avg_efficiency_by_ff_group(
  .tidy_efficiencies_df,
  .tidy_iea_df,
  include_non_energy_uses = FALSE,
  final_use_mats = c(IEATools::psut_cols$Y, IEATools::psut_cols$U_eiou),
  list_oil_products = IEATools::oil_and_oil_products,
  list_coal_products = IEATools::coal_and_coal_products,
  list_gas_products = IEATools::primary_gas_products,
  product.group = "Product.Group",
  country = IEATools::iea_cols$country,
  method = IEATools::iea_cols$method,
  energy_type = IEATools::iea_cols$energy_type,
  last_stage = IEATools::iea_cols$last_stage,
  year = IEATools::iea_cols$year,
  product = IEATools::iea_cols$product,
  share = "Share",
  useful_stage_eroi = "Useful_Stage_EROI",
  group.eroi = "Group.eroi",
  energy.stage = "Energy.stage",
  product_without_origin = "product_without_origin",
  average_efficiency = "Average_Efficiency_Col",
  aggregated_efficiency = "Aggregated_Efficiency",
  calc_method = c("dta", "gma")
)
}
\arguments{
\item{.tidy_efficiencies_df}{The tidy efficiencies data frame which provides the efficiencies to use for each energy product.}

\item{.tidy_iea_df}{The \code{.tidy_iea_df}, from which the shares of use of each energy product will be determined.}

\item{include_non_energy_uses}{A boolean stating whether non_energy_uses should be used in the calculation of the use shares of each energy product/
Default is FALSE.}

\item{final_use_mats}{The list of matrices that should be used for the calculation of the use shares of each energy product.
Default is \code{c(IEATools::psut_cols$Y, IEATools::psut_cols$U_eiou)}.}

\item{list_oil_products}{The list of oil products to use for the calculation of the use shares of each energy product.
Default is \code{IEATools::oil_and_oil_products}.}

\item{list_coal_products}{The list of coal products to use for the calculation of the use shares of each energy product.
Default is \code{IEATools::coal_and_coal_products}.}

\item{list_gas_products}{The list of gas products to use for the calculation of the use shares of each energy product.
Default is \code{IEATools::primary_gas_products}.}

\item{product.group}{The name of the column containing the name of the product group.
Default is "Product.Group".}

\item{country, method, energy_type, last_stage, year, product}{See \code{IEATools::iea_cols}.}

\item{share}{The name of the column name containing the shares of use of each energy product.
Default is "Share".}

\item{useful_stage_eroi}{The name of the column containing the useful stage EROI.
Default is "Useful_Stage_EROI".}

\item{group.eroi}{The name of the column containing the group level EROI.
Default is "Group.eroi".}

\item{energy.stage}{The name of the column containing the energy stage for which the efficiencies are calculated.
Default is "Energy.stage".}

\item{product_without_origin}{The name of the column containing the product name excluding its origin.
Default is "product_without_origin".}

\item{average_efficiency}{The name of the column containing the average efficiency of each energy product.
Default is "Average_Efficiency_Col".}

\item{aggregated_efficiency}{The name of the column containing the aggregated efficiency for each fossil fuel group.
Default is "Aggregated_Efficiency".}

\item{calc_method}{The calculation method being used, either DTA if working on single country, or GMA if working with a multi-regional framework.
Default is "dta".}
}
\value{
A tidy data frame with average final-to-useful efficiencies calculated for each fossil fuel group.
}
\description{
This function calculates the average final-to-useful efficiencies for each fossil fuel group using the average efficiency of each energy product
and the shares of use of each energy product within each fossil fuel group.
}
\examples{
  tidy_AB_dta <- ECCTools::tidy_AB_data \%>\%
  IEATools::add_psut_matnames() \%>\% 
ECCTools::transform_to_dta(requirement_matrices_list = c("U_feed"),
                        select_dta_observations = FALSE)
# Building efficiencies data frame:
length_to_use <- tidy_AB_dta \%>\% 
 dplyr::select(Country, Method, Energy.type, Year, Product) \%>\% 
 dplyr::distinct() \%>\% 
 nrow()
tidy_FU_efficiencies <- tidy_AB_dta \%>\% 
 dplyr::select(Country, Method, Energy.type, Year, Product) \%>\% 
 dplyr::distinct() \%>\% 
 dplyr::mutate(
   Average_Efficiency_Col = seq(0.15, 1, 0.85/(length_to_use-1))
 )
# Calculating aggregated efficiencies
aggregated_efficiencies_dta <- calc_avg_efficiency_by_ff_group(
 .tidy_efficiencies_df = tidy_FU_efficiencies,
 .tidy_iea_df = tidy_AB_dta,
 calc_method = "dta"
)
}
