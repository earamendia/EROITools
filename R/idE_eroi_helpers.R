
# Calculates all the final energy consumption, whatever the form (fuel, elect, heat)
# that is provided from fossil fuels, by fossil fuel group.
# We need that function for adding indirect energy and to have EROIs with idE.

#' Title
#'
#' @param .tidy_iea_df
#' @param include_non_energy_uses
#' @param list_use_mats
#' @param list_supply_mats
#' @param list_oil_products
#' @param list_coal_products
#' @param list_gas_products
#' @param list_non_energy_flows
#' @param exports
#' @param losses
#' @param country
#' @param method
#' @param energy_type
#' @param last_stage
#' @param year
#' @param product
#' @param unit
#' @param flow
#' @param e_dot
#' @param matnames
#' @param product.group
#' @param energy.stage
#'
#' @return
#' @export
#'
#' @examples
calc_fec_from_ff_by_group <- function(.tidy_iea_df,
                                      include_non_energy_uses = FALSE,
                                      list_use_mats = c(IEATools::psut_cols$Y, IEATools::psut_cols$U_eiou),
                                      list_supply_mats = "V",
                                      list_oil_products = IEATools::oil_and_oil_products,
                                      list_coal_products = IEATools::coal_and_coal_products,
                                      list_gas_products = IEATools::primary_gas_products,
                                      list_non_energy_flows = IEATools::non_energy_flows,
                                      exports = IEATools::interface_industries$exports,
                                      losses = IEATools::tfc_compare_flows$losses,
                                      country = IEATools::iea_cols$country,
                                      method = IEATools::iea_cols$method,
                                      energy_type = IEATools::iea_cols$energy_type,
                                      last_stage = IEATools::iea_cols$last_stage,
                                      year = IEATools::iea_cols$year,
                                      product = IEATools::iea_cols$product,
                                      unit = IEATools::iea_cols$unit,
                                      flow = IEATools::iea_cols$flow,
                                      e_dot = IEATools::iea_cols$e_dot,
                                      matnames = IEATools::mat_meta_cols$matnames,
                                      product.group = "Product.Group",
                                      energy.stage = "Energy.stage"){
  
  
  to_return <- dplyr::bind_rows(
    calc_fec_from_ff_as_fuel_by_group(.tidy_iea_df,
                                      list_use_mats = list_use_mats,
                                      list_oil_products = list_oil_products,
                                      list_coal_products = list_coal_products,
                                      list_gas_products = list_gas_products,
                                      exports = exports,
                                      losses = losses,
                                      include_non_energy_uses = include_non_energy_uses,
                                      list_non_energy_flows = list_non_energy_flows),
    # Here no need to have a switch as elec is only used for energy uses
    calc_fec_from_ff_as_elec_by_group(.tidy_iea_df,
                                      list_use_mats = list_use_mats,
                                      list_supply_mats = list_supply_mats,
                                      list_oil_products = list_oil_products,
                                      list_coal_products = list_coal_products,
                                      list_gas_products = list_gas_products,
                                      exports = exports,
                                      losses = losses),
    # Likewise - no need to have a switch as heat is used for energy uses
    calc_fec_from_ff_as_heat_by_group(.tidy_iea_df,
                                      list_use_mats = list_use_mats,
                                      list_supply_mats = list_supply_mats,
                                      list_oil_products = list_oil_products,
                                      list_coal_products = list_coal_products,
                                      list_gas_products = list_gas_products,
                                      exports = exports,
                                      losses = losses)
  ) %>%
    dplyr::group_by(
      .data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[unit]],
      .data[[product.group]], .data[[energy.stage]]
    ) %>%
    dplyr::mutate(
      "{e_dot}" := abs(.data[[e_dot]])
    ) %>% 
    dplyr::summarise(
      "{e_dot}" := sum(.data[[e_dot]])
    )
  
  return(to_return)
}


# Calculates final energy consumption by fossil fuel group, only when used as fuel.
#' Title
#'
#' @param .tidy_iea_df
#' @param include_non_energy_uses
#' @param list_use_mats
#' @param list_oil_products
#' @param list_coal_products
#' @param list_gas_products
#' @param list_non_energy_flows
#' @param exports
#' @param losses
#' @param country
#' @param method
#' @param energy_type
#' @param last_stage
#' @param year
#' @param product
#' @param unit
#' @param flow
#' @param e_dot
#' @param matnames
#' @param product.group
#' @param energy.stage
#'
#' @return
#' @export
#'
#' @examples
calc_fec_from_ff_as_fuel_by_group <- function(.tidy_iea_df,
                                              include_non_energy_uses = FALSE,
                                              list_use_mats = c(IEATools::psut_cols$Y, IEATools::psut_cols$U_eiou),
                                              list_oil_products = IEATools::oil_and_oil_products,
                                              list_coal_products = IEATools::coal_and_coal_products,
                                              list_gas_products = IEATools::primary_gas_products,
                                              list_non_energy_flows = IEATools::non_energy_flows,
                                              exports = IEATools::interface_industries$exports,
                                              losses = IEATools::tfc_compare_flows$losses,
                                              country = IEATools::iea_cols$country,
                                              method = IEATools::iea_cols$method,
                                              energy_type = IEATools::iea_cols$energy_type,
                                              last_stage = IEATools::iea_cols$last_stage,
                                              year = IEATools::iea_cols$year,
                                              product = IEATools::iea_cols$product,
                                              unit = IEATools::iea_cols$unit,
                                              flow = IEATools::iea_cols$flow,
                                              e_dot = IEATools::iea_cols$e_dot,
                                              matnames = IEATools::mat_meta_cols$matnames,
                                              product.group = "Product.Group",
                                              energy.stage = "Energy.stage",
                                              product_without_origin = "product_without_origin"){
  
  ### Preparing the .tidy_iea_df so that it has a new "product_without_origin" column,
  # which will be equal to "product" when we are not using a MR-PSUT framework
  cols_to_check <- c(product_without_origin = NA_character_)
  
  .tidy_iea_df <- .tidy_iea_df %>%
    tibble::add_column(!!!cols_to_check[!names(cols_to_check) %in% names(.)]) %>%
    dplyr::mutate(
      "{product_without_origin}" := dplyr::case_when(
        is.na(.data[[product_without_origin]]) ~ .data[[product]],
        TRUE ~ .data[[product_without_origin]]
      )
    )
  
  list_elect_heat_from_ff <- c("Electricity [from Oil products]", "Electricity [from Coal products]", "Electricity [from Natural gas]",
                               "Heat [from Oil products]", "Heat [from Coal products]", "Heat [from Natural gas]")
  
  if (isTRUE(include_non_energy_uses)){
    list_non_energy_flows <- NA
  }
  
  # First step, Summing up all final fossil fuel energy, excluding elec and heat
  # As "All fossil fuels" group
  final_ff_energy_excl_elec_heat_1 <- .tidy_iea_df %>%
    dplyr::filter(
      (.data[[matnames]] %in% list_use_mats & (.data[[product_without_origin]] %in% c(list_oil_products, list_coal_products, list_gas_products)))
    ) %>%
    dplyr::filter(.data[[flow]] != losses) %>%
    dplyr::filter(! stringr::str_detect(.data[[flow]], exports)) %>%
    dplyr::filter(! .data[[flow]] %in% list_non_energy_flows) %>%
    dplyr::mutate(
      "{product.group}" := "All fossil fuels",
      "{energy.stage}" := "Final"
    )
  
  # Second step, Summing up all final fossil fuel energy, excluding elec and heat
  # As "Oil products", "Natural gas", and "Coal products" groups
  final_ff_energy_excl_elec_heat_2 <- .tidy_iea_df %>%
    dplyr::filter(
      (.data[[matnames]] %in% list_use_mats & (.data[[product_without_origin]] %in% c(list_oil_products, list_coal_products, list_gas_products)))
    ) %>%
    dplyr::filter(.data[[flow]] != losses) %>%
    dplyr::filter(! stringr::str_detect(.data[[flow]], exports)) %>%
    dplyr::filter(! .data[[flow]] %in% list_non_energy_flows) %>%
    dplyr::mutate(
      "{product.group}" := dplyr::case_when(
        .data[[product_without_origin]] %in% list_oil_products ~ "Oil products",
        .data[[product_without_origin]] %in% list_gas_products ~ "Natural gas",
        .data[[product_without_origin]] %in% list_coal_products ~ "Coal products"
      ),
      "{energy.stage}" := "Final"
    )
  
  # Second step, Summing up all final fossil fuel energy, excluding elec and heat
  # As "Oil and gas products" group
  final_ff_energy_excl_elec_heat_3 <- .tidy_iea_df %>%
    dplyr::filter(
      (.data[[matnames]] %in% list_use_mats & (.data[[product_without_origin]] %in% c(list_oil_products, list_coal_products, list_gas_products)))
    ) %>%
    dplyr::filter(.data[[flow]] != losses) %>%
    dplyr::filter(! stringr::str_detect(.data[[flow]], exports)) %>%
    dplyr::filter(! .data[[flow]] %in% list_non_energy_flows) %>%
    dplyr::mutate(
      "{product.group}" := dplyr::case_when(
        .data[[product_without_origin]] %in% list_oil_products ~ "Oil and gas products",
        .data[[product_without_origin]] %in% list_gas_products ~ "Oil and gas products",
        .data[[product_without_origin]] %in% list_coal_products ~ "Coal products",
      ),
      "{energy.stage}" := "Final"
    ) %>%
    dplyr::filter(.data[[product.group]] != "Coal products")
  
  final_ff_energy_excl_elec_heat <- dplyr::bind_rows(
    final_ff_energy_excl_elec_heat_1,
    final_ff_energy_excl_elec_heat_2,
    final_ff_energy_excl_elec_heat_3
  ) %>% 
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[unit]], .data[[product.group]], .data[[energy.stage]]) %>% 
    dplyr::summarise(
      "{e_dot}" := sum(abs(.data[[e_dot]]))
    )
  
  return(final_ff_energy_excl_elec_heat)
}


# Calculates final energy consumption by fossil fuel group, only when used as electricity
#' Title
#'
#' @param .tidy_iea_df
#' @param list_use_mats
#' @param list_supply_mats
#' @param list_oil_products
#' @param list_coal_products
#' @param list_gas_products
#' @param exports
#' @param losses
#' @param country
#' @param method
#' @param energy_type
#' @param last_stage
#' @param year
#' @param product
#' @param unit
#' @param flow
#' @param e_dot
#' @param matnames
#' @param product.group
#' @param energy.stage
#' @param share
#'
#' @return
#' @export
#'
#' @examples
calc_fec_from_ff_as_elec_by_group <- function(.tidy_iea_df,
                                              list_use_mats = c(IEATools::psut_cols$Y, IEATools::psut_cols$U_eiou),
                                              list_supply_mats = "V",
                                              list_oil_products = IEATools::oil_and_oil_products,
                                              list_coal_products = IEATools::coal_and_coal_products,
                                              list_gas_products = IEATools::primary_gas_products,
                                              exports = IEATools::interface_industries$exports,
                                              losses = IEATools::tfc_compare_flows$losses,
                                              country = IEATools::iea_cols$country,
                                              method = IEATools::iea_cols$method,
                                              energy_type = IEATools::iea_cols$energy_type,
                                              last_stage = IEATools::iea_cols$last_stage,
                                              ledger_side = IEATools::iea_cols$ledger_side,
                                              flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                                              year = IEATools::iea_cols$year,
                                              product = IEATools::iea_cols$product,
                                              unit = IEATools::iea_cols$unit,
                                              flow = IEATools::iea_cols$flow,
                                              e_dot = IEATools::iea_cols$e_dot,
                                              matnames = IEATools::mat_meta_cols$matnames,
                                              product.group = "Product.Group",
                                              energy.stage = "Energy.stage",
                                              share = "Share",
                                              product_without_origin = "product_without_origin"){
  
  ### Preparing the .tidy_iea_df so that it has a new "product_without_origin" column,
  # which will be equal to "product" when we are not using a MR-PSUT framework
  cols_to_check <- c(product_without_origin = NA_character_)
  
  .tidy_iea_df <- .tidy_iea_df %>%
    tibble::add_column(!!!cols_to_check[!names(cols_to_check) %in% names(.)]) %>%
    dplyr::mutate(
      "{product_without_origin}" := dplyr::case_when(
        is.na(.data[[product_without_origin]]) ~ .data[[product]],
        TRUE ~ .data[[product_without_origin]]
      )
    )
  
  
  # Figuring out elec consumption by fossil fuel group:
  elec_consumption_by_ff_group <- .tidy_iea_df %>%
    dplyr::filter(
      (.data[[matnames]] %in% list_use_mats & (stringr::str_detect(.data[[product]], "Electricity")))
    ) %>%
    dplyr::mutate(
      "{product}" := dplyr::case_when(
        stringr::str_detect(.data[[product]], "\\{.*\\}_") ~ stringr::str_replace(.data[[product]], "\\{.*\\}", stringr::str_c("\\{", .data[[country]], "\\}")),
        TRUE ~ .data[[product]]
      )
    ) %>% 
    dplyr::filter(.data[[flow]] != losses) %>%
    dplyr::filter(! stringr::str_detect(.data[[flow]], exports)) %>%
    dplyr::group_by(
      .data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[ledger_side]], .data[[flow_aggregation_point]],
      .data[[flow]], .data[[product]], .data[[unit]], .data[[matnames]], .data[[product_without_origin]]
    ) %>% 
    dplyr::mutate(
      "{e_dot}" := abs(.data[[e_dot]])
    ) %>% 
    dplyr::summarise("{e_dot}" := sum(.data[[e_dot]])) %>% 
    dplyr::left_join(
      calc_share_elec_supply_by_ff_group(.tidy_iea_df,
                                         list_supply_mats = list_supply_mats,
                                         list_oil_products = list_oil_products,
                                         list_coal_products = list_coal_products,
                                         list_gas_products = list_gas_products,
                                         exports = exports,
                                         losses = losses
      ),
      by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {unit})
    ) %>%
    dplyr::mutate(
      "{e_dot}" := .data[[e_dot]] * .data[[share]]
    ) %>%
    dplyr::select(-.data[[share]])
  
  return(elec_consumption_by_ff_group)
}


#' Calculates the share of electricity provided by each fossil fuel group
#' 
#' This function calculates, for each country, the share of electricity that has been supplied by each fossil fuel group,
#' out of the domestically produced electricity. So, shares don't have to add up to unity - 
#' indeed they won't if any electricity is supplied by renewables or nuclear.
#' 
#' Fossil fuel groups used are "All fossil fuels", "Oil and gas products", "Oil products", "Natural gas", and "Coal products",
#' so they are obviously not exclusive.
#'
#' @param .tidy_iea_df The `.tidy_iea_df` for which the share of electricity provided by each fossil fuel group needs to be calculated.
#' @param list_supply_mats The list of the supply matrices to be used for the calculating the shares.
#'                         Default is `c(IEATools::psut_cols$V)`.
#' @param list_oil_products The list of oil products to be used when calculating the use shares.
#'                          Default is `IEATools::oil_and_oil_products`.
#' @param list_coal_products The list of coal products to be used when calculating the use shares.
#'                           Default is `IEATools::coal_and_coal_products`.
#' @param list_gas_products The list of gas products to be used when calculating the use shares.
#'                           Default is `IEATools::primary_gas_products`.
#' @param exports The character string identifying exports flows in the `.tidy_iea_df`.
#'                Default is `IEATools::interface_industries$exports`.
#' @param losses The character string identifying losses flows in the `.tidy_iea_df`.
#'               Default is `IEATools::tfc_compare_flows$losses`.
#' @param country,method,energy_type,last_stage,year,product,unit,flow,e_dot See `IEATools::iea_cols`.
#' @param matnames The name of the column containing the matrices names.
#'                 Default is `IEATools::mat_meta_cols$matnames`.
#' @param product.group The name of the column containing the product group name.
#'                      Default is "Product.Group".
#' @param energy.stage The name of the column containing the energy stage for the calculation of the EROI.
#'                     Default is "Energy.stage".
#' @param share The name of the column containing the shares of heat delivered by each fossil fuel group.
#'
#' @return A tidy data frame containing the shares of heat delivered by each fossil fuel group.
#' @export
#'
#' @examples
calc_share_elec_supply_by_ff_group <- function(.tidy_iea_df,
                                               list_supply_mats = "V",
                                               list_oil_products = IEATools::oil_and_oil_products,
                                               list_coal_products = IEATools::coal_and_coal_products,
                                               list_gas_products = IEATools::primary_gas_products,
                                               exports = IEATools::interface_industries$exports,
                                               losses = IEATools::tfc_compare_flows$losses,
                                               country = IEATools::iea_cols$country,
                                               method = IEATools::iea_cols$method,
                                               energy_type = IEATools::iea_cols$energy_type,
                                               last_stage = IEATools::iea_cols$last_stage,
                                               year = IEATools::iea_cols$year,
                                               product = IEATools::iea_cols$product,
                                               unit = IEATools::iea_cols$unit,
                                               flow = IEATools::iea_cols$flow,
                                               e_dot = IEATools::iea_cols$e_dot,
                                               matnames = IEATools::mat_meta_cols$matnames,
                                               product.group = "Product.Group",
                                               energy.stage = "Energy.stage",
                                               share = "Share",
                                               product_without_origin = "product_without_origin"){
  
  ### Preparing the .tidy_iea_df so that it has a new "product_without_origin" column,
  # which will be equal to "product" when we are not using a MR-PSUT framework
  cols_to_check <- c(product_without_origin = NA_character_)
  
  .tidy_iea_df <- .tidy_iea_df %>%
    tibble::add_column(!!!cols_to_check[!names(cols_to_check) %in% names(.)]) %>%
    dplyr::mutate(
      "{product_without_origin}" := dplyr::case_when(
        is.na(.data[[product_without_origin]]) ~ .data[[product]],
        TRUE ~ .data[[product_without_origin]]
      )
    )
  
  # First, for "All fossil fuels"
  share_elec_supply_ff_1 <- .tidy_iea_df %>%
    dplyr::filter(matnames %in% list_supply_mats) %>%
    dplyr::filter(stringr::str_detect(.data[[product]], "Electricity")) %>%
    dplyr::filter(.data[[flow]] != "Electricity market") %>%
    dplyr::mutate(
      "{product.group}" := dplyr::case_when(
        .data[[product_without_origin]] %in% c("Electricity [from Coal products]", "Electricity [from Natural gas]", "Electricity [from Oil products]") ~ "All fossil fuels",
        TRUE ~ "Others"
      ),
      "{energy.stage}" := "Final"
    ) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[unit]],
                    .data[[product.group]], .data[[energy.stage]]) %>%
    dplyr::mutate(
      "{e_dot}" := abs(.data[[e_dot]])
    ) %>% 
    dplyr::summarise(
      "{e_dot}" := sum(.data[[e_dot]])
    ) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[unit]]) %>%
    dplyr::mutate(
      "{share}" := .data[[e_dot]] / sum(.data[[e_dot]])
    ) %>%
    dplyr::select(-.data[[e_dot]]) %>%
    dplyr::filter(.data[[product.group]] != "Others")
  
  # Second, for "Oil and gas products" and "Coal products"
  share_elec_supply_ff_2 <- .tidy_iea_df %>%
    dplyr::filter(matnames == list_supply_mats) %>%
    dplyr::filter(stringr::str_detect(.data[[product]], "Electricity")) %>%
    dplyr::filter(.data[[flow]] != "Electricity market") %>%
    dplyr::mutate(
      "{product.group}" := dplyr::case_when(
        .data[[product_without_origin]] %in% c("Electricity [from Natural gas]", "Electricity [from Oil products]") ~ "Oil and gas products",
        .data[[product_without_origin]] == "Electricity [from Coal products]" ~ "Coal products",
        TRUE ~ "Others"
      ),
      "{energy.stage}" := "Final"
    ) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[unit]],
                    .data[[product.group]], .data[[energy.stage]]) %>%
    dplyr::mutate(
      "{e_dot}" := abs(.data[[e_dot]])
    ) %>% 
    dplyr::summarise(
      "{e_dot}" := sum(.data[[e_dot]])
    ) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[unit]]) %>%
    dplyr::mutate(
      "{share}" := .data[[e_dot]] / sum(.data[[e_dot]])
    ) %>%
    dplyr::select(-.data[[e_dot]]) %>%
    dplyr::filter(.data[[product.group]] != "Others")
  
  # Third, for "Natural gas" and "Oil products"
  share_elec_supply_ff_3 <- .tidy_iea_df %>%
    dplyr::filter(matnames == list_supply_mats) %>%
    dplyr::filter(stringr::str_detect(.data[[product]], "Electricity")) %>%
    dplyr::filter(.data[[flow]] != "Electricity market") %>%
    dplyr::mutate(
      "{product.group}" := dplyr::case_when(
        .data[[product_without_origin]] == "Electricity [from Oil products]" ~ "Oil products",
        .data[[product_without_origin]] == "Electricity [from Natural gas]" ~ "Natural gas",
        .data[[product_without_origin]] == "Electricity [from Coal products]" ~ "Coal products",
        TRUE ~ "Others"
      ),
      "{energy.stage}" := "Final"
    ) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[unit]],
                    .data[[product.group]], .data[[energy.stage]]) %>%
    dplyr::mutate(
      "{e_dot}" := abs(.data[[e_dot]])
    ) %>% 
    dplyr::summarise(
      "{e_dot}" := sum(.data[[e_dot]])
    ) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[unit]]) %>%
    dplyr::mutate(
      "{share}" := .data[[e_dot]] / sum(.data[[e_dot]])
    ) %>%
    dplyr::select(-.data[[e_dot]]) %>%
    dplyr::filter(.data[[product.group]] != "Others") %>%
    dplyr::filter(.data[[product.group]] != "Coal products")
  
  
  # Binding shares:
  share_elec_supply_ff <- dplyr::bind_rows(
    share_elec_supply_ff_1,
    share_elec_supply_ff_2,
    share_elec_supply_ff_3
  )
  
  return(share_elec_supply_ff)
}


#' Calculates final energy consumption of heat from fossil fuel origin, by fossil fuel group
#'
#' This function calculates the final energy consumption of heat from fossil fuel origin, by fossil fuel group. It does so by determining 
#' the total final consumption of heat (from use matrices) and then applying share of heat supplied by each fossil fuel group (determined from supply matrices).
#'
#' @param .tidy_iea_df The `.tidy_iea_df` for which final energy consumption of heat by fossil fuel group needs to be determined.
#' @param list_use_mats The list of matrices to be used for determining total final energy consumption of heat.
#'                      Default is `c(IEATools::psut_cols$Y, IEATools::psut_cols$U_eiou)`.
#' @param list_supply_mats The list of matrices to be used for determining the shares of supply of heat by fossil fuel group.
#'                         Default is `c(IEATools::psut_cols$V)`.
#' @param list_oil_products The list of oil products to be used when calculating the use shares.
#'                          Default is `IEATools::oil_and_oil_products`.
#' @param list_coal_products The list of coal products to be used when calculating the use shares.
#'                           Default is `IEATools::coal_and_coal_products`.
#' @param list_gas_products The list of gas products to be used when calculating the use shares.
#'                           Default is `IEATools::primary_gas_products`.
#' @param exports The character string identifying exports flows in the `.tidy_iea_df`.
#'                Default is `IEATools::interface_industries$exports`.
#' @param losses The character string identifying losses flows in the `.tidy_iea_df`.
#'               Default is `IEATools::tfc_compare_flows$losses`.
#' @param country,method,energy_type,last_stage,ledger_side,flow_aggregation_point,year,product,unit,flow,e_dot See `IEATools::iea_cols`.
#' @param matnames The name of the column containing the matrices names.
#'                 Default is `IEATools::mat_meta_cols$matnames`.
#' @param product.group The name of the column containing the product group name.
#'                      Default is "Product.Group".
#' @param energy.stage The name of the column containing the energy stage for the calculation of the EROI.
#'                     Default is "Energy.stage".
#' @param share The name of the column containing the shares of supply of heat by fossil fuel group.
#'              Default is "Share".
#' @param product_without_origin The name of the column containing the product name excluding the origin of the product.
#'                               Default is "product_without_origin".
#'
#' @return A tidy data frame containing the total final energy consumption of heat, broken down by the supplying fossil fuel group.
#' @export
#'
#' @examples
calc_fec_from_ff_as_heat_by_group <- function(.tidy_iea_df,
                                              list_use_mats = c(IEATools::psut_cols$Y, IEATools::psut_cols$U_eiou),
                                              list_supply_mats = c(IEATools::psut_cols$V),
                                              list_oil_products = IEATools::oil_and_oil_products,
                                              list_coal_products = IEATools::coal_and_coal_products,
                                              list_gas_products = IEATools::primary_gas_products,
                                              exports = IEATools::interface_industries$exports,
                                              losses = IEATools::tfc_compare_flows$losses,
                                              country = IEATools::iea_cols$country,
                                              method = IEATools::iea_cols$method,
                                              energy_type = IEATools::iea_cols$energy_type,
                                              last_stage = IEATools::iea_cols$last_stage,
                                              ledger_side = IEATools::iea_cols$ledger_side,
                                              flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                                              year = IEATools::iea_cols$year,
                                              product = IEATools::iea_cols$product,
                                              unit = IEATools::iea_cols$unit,
                                              flow = IEATools::iea_cols$flow,
                                              e_dot = IEATools::iea_cols$e_dot,
                                              matnames = IEATools::mat_meta_cols$matnames,
                                              product.group = "Product.Group",
                                              energy.stage = "Energy.stage",
                                              share = "Share",
                                              product_without_origin = "product_without_origin"){
  
  ### Preparing the .tidy_iea_df so that it has a new "product_without_origin" column,
  # which will be equal to "product" when we are not using a MR-PSUT framework
  cols_to_check <- c(product_without_origin = NA_character_)
  
  .tidy_iea_df <- .tidy_iea_df %>%
    tibble::add_column(!!!cols_to_check[!names(cols_to_check) %in% names(.)]) %>%
    dplyr::mutate(
      "{product_without_origin}" := dplyr::case_when(
        is.na(.data[[product_without_origin]]) ~ .data[[product]],
        TRUE ~ .data[[product_without_origin]]
      )
    )
  
  
  # Figuring out heat consumption by fossil fuel group:
  heat_consumption_by_ff_group <- .tidy_iea_df %>%
    dplyr::filter(
      (.data[[matnames]] %in% list_use_mats & (stringr::str_detect(.data[[product]], "Heat")))
    ) %>%
    dplyr::mutate(
      "{product}" := dplyr::case_when(
        stringr::str_detect(.data[[product]], "\\{.*\\}_") ~ stringr::str_replace(.data[[product]], "\\{.*\\}", stringr::str_c("\\{", .data[[country]], "\\}")),
        TRUE ~ .data[[product]]
      )
    ) %>% 
    dplyr::filter(.data[[flow]] != losses) %>%
    dplyr::filter(! stringr::str_detect(.data[[flow]], exports)) %>%
    dplyr::group_by(
      .data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[ledger_side]], .data[[flow_aggregation_point]],
      .data[[flow]], .data[[product]], .data[[unit]], .data[[matnames]], .data[[product_without_origin]]
    ) %>% 
    dplyr::mutate(
      "{e_dot}" := abs(.data[[e_dot]])
    ) %>% 
    dplyr::summarise("{e_dot}" := sum(.data[[e_dot]])) %>% 
    dplyr::left_join(
      calc_share_heat_supply_by_ff_group(.tidy_iea_df,
                                         list_supply_mats = list_supply_mats,
                                         list_oil_products = list_oil_products,
                                         list_coal_products = list_coal_products,
                                         list_gas_products = list_gas_products,
                                         exports = exports,
                                         losses = losses),
      by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {unit})
    ) %>%
    dplyr::mutate(
      "{e_dot}" := .data[[e_dot]] * .data[[share]]
    ) %>%
    dplyr::select(-.data[[share]])
  
  return(heat_consumption_by_ff_group)
}


#' Calculates the share of heat provided by each fossil fuel group
#' 
#' This function calculates, for each country, the share of heat that has been supplied by each fossil fuel group,
#' out of the domestically produced heat. So, shares don't have to add up to unity - 
#' indeed they won't if any heat is supplied by renewables or nuclear.
#' 
#' Fossil fuel groups used are "All fossil fuels", "Oil and gas products", "Oil products", "Natural gas", and "Coal products",
#' so they are obviously not exclusive.
#' 
#' @param .tidy_iea_df The `.tidy_iea_df` for which the share of heat provided by each fossil fuel group needs to be calculated.
#' @param list_supply_mats The list of the supply matrices to be used for the calculating the shares.
#'                         Default is `c(IEATools::psut_cols$V)`.
#' @param list_oil_products The list of oil products to be used when calculating the use shares.
#'                          Default is `IEATools::oil_and_oil_products`.
#' @param list_coal_products The list of coal products to be used when calculating the use shares.
#'                           Default is `IEATools::coal_and_coal_products`.
#' @param list_gas_products The list of gas products to be used when calculating the use shares.
#'                           Default is `IEATools::primary_gas_products`.
#' @param exports The character string identifying exports flows in the `.tidy_iea_df`.
#'                Default is `IEATools::interface_industries$exports`.
#' @param losses The character string identifying losses flows in the `.tidy_iea_df`.
#'               Default is `IEATools::tfc_compare_flows$losses`.
#' @param country,method,energy_type,last_stage,year,product,unit,flow,e_dot See `IEATools::iea_cols`.
#' @param matnames The name of the column containing the matrices names.
#'                 Default is `IEATools::mat_meta_cols$matnames`.
#' @param product.group The name of the column containing the product group name.
#'                      Default is "Product.Group".
#' @param energy.stage The name of the column containing the energy stage for the calculation of the EROI.
#'                     Default is "Energy.stage".
#' @param share The name of the column containing the shares of heat delivered by each fossil fuel group.
#'
#' @return A tidy data frame containing the shares of heat delivered by each fossil fuel group.
#' @export
#'
#' @examples
calc_share_heat_supply_by_ff_group <- function(.tidy_iea_df,
                                               list_supply_mats = c(IEATools::psut_cols$V),
                                               list_oil_products = IEATools::oil_and_oil_products,
                                               list_coal_products = IEATools::coal_and_coal_products,
                                               list_gas_products = IEATools::primary_gas_products,
                                               exports = IEATools::interface_industries$exports,
                                               losses = IEATools::tfc_compare_flows$losses,
                                               country = IEATools::iea_cols$country,
                                               method = IEATools::iea_cols$method,
                                               energy_type = IEATools::iea_cols$energy_type,
                                               last_stage = IEATools::iea_cols$last_stage,
                                               year = IEATools::iea_cols$year,
                                               product = IEATools::iea_cols$product,
                                               unit = IEATools::iea_cols$unit,
                                               flow = IEATools::iea_cols$flow,
                                               e_dot = IEATools::iea_cols$e_dot,
                                               matnames = IEATools::mat_meta_cols$matnames,
                                               product.group = "Product.Group",
                                               energy.stage = "Energy.stage",
                                               share = "Share",
                                               product_without_origin = "product_without_origin"){
  
  ### Preparing the .tidy_iea_df so that it has a new "product_without_origin" column,
  # which will be equal to "product" when we are not using a MR-PSUT framework
  cols_to_check <- c(product_without_origin = NA_character_)
  
  .tidy_iea_df <- .tidy_iea_df %>%
    tibble::add_column(!!!cols_to_check[!names(cols_to_check) %in% names(.)]) %>%
    dplyr::mutate(
      "{product_without_origin}" := dplyr::case_when(
        is.na(.data[[product_without_origin]]) ~ .data[[product]],
        TRUE ~ .data[[product_without_origin]]
      )
    )
  
  # First, calculate shares of the "All fossil fuels" group
  share_heat_supply_ff_1 <- .tidy_iea_df %>%
    dplyr::filter(.data[[matnames]] == list_supply_mats) %>%
    dplyr::filter(stringr::str_detect(.data[[product]], "Heat")) %>%
    dplyr::filter(.data[[flow]] != "Heat market") %>%
    dplyr::mutate(
      "{product.group}" := dplyr::case_when(
        .data[[product_without_origin]] %in% c("Heat [from Coal products]", "Heat [from Natural gas]", "Heat [from Oil products]") ~ "All fossil fuels",
        TRUE ~ "Others"
      ),
      "{energy.stage}" := "Final"
    ) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[unit]],
                    .data[[product.group]], .data[[energy.stage]]) %>%
    dplyr::mutate(
      "{e_dot}" := abs(.data[[e_dot]])
    ) %>% 
    dplyr::summarise(
      "{e_dot}" := sum(.data[[e_dot]])
    ) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[unit]]) %>%
    dplyr::mutate(
      "{share}" := .data[[e_dot]] / sum(.data[[e_dot]])
    ) %>%
    dplyr::select(-.data[[e_dot]]) %>%
    dplyr::filter(.data[[product.group]] != "Others")
  
  # Second, calculate shares of the "Oil and gas products" and "Coal products" group
  share_heat_supply_ff_2 <- .tidy_iea_df %>%
    dplyr::filter(.data[[matnames]] == list_supply_mats) %>%
    dplyr::filter(stringr::str_detect(.data[[product]], "Heat")) %>%
    dplyr::filter(.data[[flow]] != "Heat market") %>%
    dplyr::mutate(
      "{product.group}" := dplyr::case_when(
        .data[[product_without_origin]] %in% c("Heat [from Natural gas]", "Heat [from Oil products]") ~ "Oil and gas products",
        .data[[product_without_origin]] == "Heat [from Coal products]" ~ "Coal products",
        TRUE ~ "Others"
      ),
      "{energy.stage}" := "Final"
    ) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[unit]],
                    .data[[product.group]], .data[[energy.stage]]) %>%
    dplyr::mutate(
      "{e_dot}" := abs(.data[[e_dot]])
    ) %>% 
    dplyr::summarise(
      "{e_dot}" := sum(.data[[e_dot]])
    ) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[unit]]) %>%
    dplyr::mutate(
      "{share}" := .data[[e_dot]] / sum(.data[[e_dot]])
    ) %>%
    dplyr::select(-.data[[e_dot]]) %>%
    dplyr::filter(.data[[product.group]] != "Others")
  
  # Third, calculate shares of the "Oil products" and "Natural gas" groups
  share_heat_supply_ff_3 <- .tidy_iea_df %>%
    dplyr::filter(.data[[matnames]] == list_supply_mats) %>%
    dplyr::filter(stringr::str_detect(.data[[product]], "Heat")) %>%
    dplyr::filter(.data[[flow]] != "Heat market") %>%
    dplyr::mutate(
      "{product.group}" := dplyr::case_when(
        .data[[product_without_origin]] == "Heat [from Oil products]" ~ "Oil products",
        .data[[product_without_origin]] == "Heat [from Natural gas]" ~ "Natural gas",
        .data[[product_without_origin]] == "Heat [from Coal products]" ~ "Coal products",
        TRUE ~ "Others"
      ),
      "{energy.stage}" := "Final"
    ) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[unit]],
                    .data[[product.group]], .data[[energy.stage]]) %>%
    dplyr::mutate(
      "{e_dot}" := abs(.data[[e_dot]])
    ) %>% 
    dplyr::summarise(
      "{e_dot}" := sum(.data[[e_dot]])
    ) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[unit]]) %>%
    dplyr::mutate(
      "{share}" := .data[[e_dot]] / sum(.data[[e_dot]])
    ) %>%
    dplyr::select(-.data[[e_dot]]) %>%
    dplyr::filter(.data[[product.group]] != "Others") %>%
    dplyr::filter(.data[[product.group]] != "Coal products")
  
  # Binding shares:
  share_heat_supply_ff <- dplyr::bind_rows(
    share_heat_supply_ff_1,
    share_heat_supply_ff_2,
    share_heat_supply_ff_3
  )
  
  return(share_heat_supply_ff)
}

