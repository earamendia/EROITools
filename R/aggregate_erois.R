
# This function aggregates final stage EROIs per main groups
# This function aggregates using the shares of each product in the primary production of each country, within a given fossil fuel group.
# So it uses the make matrix V on which it filters for primary energy products to calculate the shares.

#' Title
#'
#' @param .tidy_erois_df 
#' @param .tidy_iea_df 
#' @param include_non_energy_uses 
#' @param primary_production_mats 
#' @param list_primary_oil_products 
#' @param list_primary_coal_products 
#' @param list_primary_gas_products 
#' @param product.group 
#' @param country 
#' @param method 
#' @param energy_type 
#' @param last_stage 
#' @param year 
#' @param product 
#' @param non_energy_uses 
#' @param eroi.method 
#' @param type 
#' @param boundary 
#' @param share 
#' @param eroi 
#' @param group.eroi 
#' @param energy.stage 
#' @param product_without_origin 
#'
#' @return
#' @export
#'
#' @examples
aggregate_primary_stage_erois <- function(.tidy_erois_df,
                                          .tidy_iea_df,
                                          # Whether you want to include non-energy uses products in the EROI calculation
                                          include_non_energy_uses = FALSE,
                                          # Which matrices flows to use for calculating shares
                                          primary_production_mats = c(IEATools::psut_cols$V),
                                          # Lists defining each product group
                                          list_primary_oil_products = IEATools::primary_oil_products,
                                          list_primary_coal_products = IEATools::primary_coal_products,
                                          list_primary_gas_products = IEATools::primary_gas_products,
                                          # Do not change
                                          product.group = "Product.Group",
                                          country = IEATools::iea_cols$country,
                                          method = IEATools::iea_cols$method,
                                          energy_type = IEATools::iea_cols$energy_type,
                                          last_stage = IEATools::iea_cols$last_stage,
                                          year = IEATools::iea_cols$year,
                                          product = IEATools::iea_cols$product,
                                          non_energy_uses = "Non_Energy_Uses",
                                          eroi.method = "Eroi.method",
                                          type = "Type",
                                          boundary = "Boundary",
                                          share = "Share",
                                          eroi = "EROI",
                                          group.eroi = "Group.eroi",
                                          energy.stage = "Energy.stage",
                                          product_without_origin = "product_without_origin"){
  
  
  ### (1) Preparing the .tidy_iea_df so that it has a new "product_without_origin" column,
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
  
  # Pulling out the list of countries contained in .tidy_iea_df
  list_countries_in_tidy_df <- .tidy_iea_df %>%
    dplyr::ungroup() %>%
    dplyr::select(.data[[country]]) %>%
    dplyr::distinct() %>%
    dplyr::pull()
  
  
  ### (2) Calculating tidy shares by product at the primary stage ###
  # Group also includes "All fossil fuels".
  tidy_shares_primary_df <- calc_share_primary_ff_supply_by_product_by_group(.tidy_iea_df,
                                                                             include_non_energy_uses = include_non_energy_uses,
                                                                             primary_production_mats = primary_production_mats,
                                                                             list_primary_oil_products = list_primary_oil_products,
                                                                             list_primary_coal_products = list_primary_coal_products,
                                                                             list_primary_gas_products = list_primary_gas_products)

  
  ### (3) Determining average primary stage EROIs (so, aggregating) from here
  aggregated_primary_stage_erois <- tidy_shares_primary_df %>%
    dplyr::inner_join(.tidy_erois_df %>%
                        dplyr::select(-.data[[country]]), by = c({method}, {energy_type}, {last_stage}, {year}, {product})) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]],
                    .data[[eroi.method]], .data[[type]], .data[[boundary]], .data[[non_energy_uses]], .data[[product.group]], .data[[energy.stage]]) %>%
    dplyr::summarise(
      Group.eroi.inversed = sum(.data[[share]] * (1/.data[[eroi]])) / sum(.data[[share]])
    ) %>% 
    dplyr::mutate(
      "{group.eroi}" := 1 / Group.eroi.inversed
    ) %>% 
    dplyr::select(-Group.eroi.inversed)
  
  return(aggregated_primary_stage_erois)
}






# This function aggregates final stage EROIs per main groups

#' Title
#'
#' @param .tidy_erois_df
#' @param .tidy_iea_df
#' @param include_non_energy_uses
#' @param final_use_mats
#' @param list_oil_products
#' @param list_coal_products
#' @param list_gas_products
#' @param product.group
#' @param country
#' @param method
#' @param energy_type
#' @param last_stage
#' @param year
#' @param product
#' @param non_energy_uses
#' @param eroi.method
#' @param type
#' @param boundary
#' @param share
#' @param eroi
#' @param group.eroi
#' @param energy.stage
#' @param product_without_origin
#'
#' @return
#' @export
#'
#' @examples
aggregate_final_stage_erois <- function(.tidy_erois_df,
                                        .tidy_iea_df,
                                        # Whether you want to include non-energy uses products in the EROI calculation
                                        include_non_energy_uses = FALSE,
                                        # Which matrices flows to use for calculating shares
                                        final_use_mats = c(IEATools::psut_cols$Y, IEATools::psut_cols$U_eiou),
                                        # Lists defining each product group
                                        list_oil_products = IEATools::oil_and_oil_products,
                                        list_coal_products = IEATools::coal_and_coal_products,
                                        list_gas_products = IEATools::primary_gas_products,
                                        # Do not change
                                        product.group = "Product.Group",
                                        country = IEATools::iea_cols$country,
                                        method = IEATools::iea_cols$method,
                                        energy_type = IEATools::iea_cols$energy_type,
                                        last_stage = IEATools::iea_cols$last_stage,
                                        year = IEATools::iea_cols$year,
                                        product = IEATools::iea_cols$product,
                                        non_energy_uses = "Non_Energy_Uses",
                                        eroi.method = "Eroi.method",
                                        type = "Type",
                                        boundary = "Boundary",
                                        share = "Share",
                                        eroi = "EROI",
                                        group.eroi = "Group.eroi",
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
  
  # Pulling out the list of countries contained in .tidy_iea_df
  list_countries_in_tidy_df <- .tidy_iea_df %>%
    dplyr::ungroup() %>%
    dplyr::select(.data[[country]]) %>%
    dplyr::distinct() %>%
    dplyr::pull()
  
  
  ### Big first step - Building the tidy shares data frame ###
  
  # (1) Calculating tidy shares by product at the final (fuel) stage:
  tidy_shares_final_fuel_df <- calc_share_ff_use_by_product_by_group(.tidy_iea_df,
                                                                     include_non_energy_uses = include_non_energy_uses,
                                                                     final_use_mats = final_use_mats,
                                                                     list_oil_products = list_oil_products,
                                                                     list_coal_products = list_coal_products,
                                                                     list_gas_products = list_gas_products)

  
  # (2) Calculating tidy shares by product at the final (elec) stage:
  tidy_shares_final_elec_df <- calc_shares_elec_by_ff_group(.tidy_iea_df)
  
  # (3) Calculating tidy shares by product at the final (heat) stage:
  tidy_shares_final_heat_df <- calc_shares_heat_by_ff_group(.tidy_iea_df)
  
  # (4) Calculating tidy shares by product at the final (fuel+elec+heat) stage:
  tidy_shares_ff_by_group_inc_elec_heat <- calc_shares_ff_by_group_inc_elec_heat(.tidy_iea_df,
                                                                                 include_non_energy_uses = include_non_energy_uses,
                                                                                 final_use_mats = final_use_mats,
                                                                                 list_oil_products = list_oil_products,
                                                                                 list_coal_products = list_coal_products,
                                                                                 list_gas_products = list_gas_products)
  
  # (5) Merging all shares together:
  tidy_shares_df <- dplyr::bind_rows(
    tidy_shares_final_fuel_df,
    tidy_shares_final_elec_df,
    tidy_shares_final_heat_df,
    tidy_shares_ff_by_group_inc_elec_heat
  )
  
  ### Big second step - Determining average final stage EROIs from there ###
  
  aggregated_final_stage_erois <- tidy_shares_df %>%
    dplyr::inner_join(.tidy_erois_df %>%
                        dplyr::select(-.data[[country]]), by = c({method}, {energy_type}, {last_stage}, {year}, {product})) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]],
                    .data[[eroi.method]], .data[[type]], .data[[boundary]], .data[[non_energy_uses]], .data[[product.group]], .data[[energy.stage]]) %>%
    dplyr::summarise(
      Group.eroi.inversed = sum(.data[[share]] * (1/.data[[eroi]])) / sum(.data[[share]])
    ) %>% 
    dplyr::mutate(
      "{group.eroi}" := 1 / Group.eroi.inversed
    ) %>% 
    dplyr::select(-Group.eroi.inversed)
  
  return(aggregated_final_stage_erois)
}



#' Title
#'
#' @param .tidy_erois_df
#' @param .tidy_iea_df
#' @param include_non_energy_uses
#' @param final_use_mats
#' @param primary_production_mats
#' @param list_primary_oil_products
#' @param list_primary_coal_products
#' @param list_primary_gas_products
#' @param list_oil_products
#' @param list_coal_products
#' @param list_gas_products
#' @param product.group
#' @param country
#' @param method
#' @param energy_type
#' @param last_stage
#' @param year
#' @param product
#' @param non_energy_uses
#' @param eroi.method
#' @param type
#' @param boundary
#' @param share
#' @param useful_stage_eroi
#' @param group.eroi
#' @param energy.stage
#' @param product_without_origin
#'
#' @return
#' @export
#'
#' @examples
aggregate_useful_stage_erois <- function(.tidy_erois_df,
                                         .tidy_iea_df,
                                         # Whether you want to include non-energy uses products in the EROI calculation
                                         include_non_energy_uses = FALSE,
                                         # Which matrices flows to use for calculating shares
                                         final_use_mats = c(IEATools::psut_cols$Y, IEATools::psut_cols$U_eiou),
                                         primary_production_mats = c(IEATools::psut_cols$V),
                                         # Lists defining each product group
                                         list_primary_oil_products = IEATools::primary_oil_products,
                                         list_primary_coal_products = IEATools::primary_coal_products,
                                         list_primary_gas_products = IEATools::primary_gas_products,
                                         list_oil_products = IEATools::oil_and_oil_products,
                                         list_coal_products = IEATools::coal_and_coal_products,
                                         list_gas_products = IEATools::primary_gas_products,
                                         # Do not change
                                         product.group = "Product.Group",
                                         country = IEATools::iea_cols$country,
                                         method = IEATools::iea_cols$method,
                                         energy_type = IEATools::iea_cols$energy_type,
                                         last_stage = IEATools::iea_cols$last_stage,
                                         year = IEATools::iea_cols$year,
                                         product = IEATools::iea_cols$product,
                                         non_energy_uses = "Non_Energy_Uses",
                                         eroi.method = "Eroi.method",
                                         type = "Type",
                                         boundary = "Boundary",
                                         share = "Share",
                                         useful_stage_eroi = "Useful_Stage_EROI",
                                         group.eroi = "Group.eroi",
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
  
  # Pulling out the list of countries contained in .tidy_iea_df
  list_countries_in_tidy_df <- .tidy_iea_df %>%
    dplyr::ungroup() %>%
    dplyr::select(.data[[country]]) %>%
    dplyr::distinct() %>%
    dplyr::pull()
  
  
  ### Big first step - Building the tidy shares data frame ###
  
  # (1) Calculating tidy shares by product at the final (fuel) stage:
  tidy_shares_final_fuel_df <- calc_share_ff_use_by_product_by_group(.tidy_iea_df,
                                                                     include_non_energy_uses = include_non_energy_uses,
                                                                     final_use_mats = final_use_mats,
                                                                     list_oil_products = list_oil_products,
                                                                     list_coal_products = list_coal_products,
                                                                     list_gas_products = list_gas_products) %>%
    dplyr::mutate(
      "{energy.stage}" := "Useful (fuel)"
    )
  
  # (2) Calculating tidy shares by product at the final (elec) stage:
  # THIS CHUNK SHOULD BE SQUEEZED IN THE calc_shares_elec_by_ff_group() FUNCTION
  # BUT WE NEED TESTS TO DO THAT...
  tidy_shares_final_elec_df <- calc_shares_elec_by_ff_group(.tidy_iea_df) %>% 
    dplyr::mutate(
      "{energy.stage}" := "Useful (electricity)"
    )
  
  # (3) Calculating tidy shares by product at the final (heat) stage:
  # THIS CHUNK SHOULD BE SQUEEZED IN THE calc_shares_heat_by_ff_group() FUNCTION
  # BUT WE NEED TESTS TO DO THAT...
  tidy_shares_final_heat_df <- calc_shares_heat_by_ff_group(.tidy_iea_df) %>% 
    dplyr::mutate(
      "{energy.stage}" := "Useful (heat)"
    )
  
  # (4) Calculating tidy shares by product at the final (fuel+elec+heat) stage:
  tidy_shares_ff_by_group_inc_elec_heat <- calc_shares_ff_by_group_inc_elec_heat(.tidy_iea_df,
                                                                                 include_non_energy_uses = include_non_energy_uses,
                                                                                 final_use_mats = final_use_mats,
                                                                                 list_oil_products = list_oil_products,
                                                                                 list_coal_products = list_coal_products,
                                                                                 list_gas_products = list_gas_products) %>%
    dplyr::mutate(
      "{energy.stage}" := "Useful (fuel+elec+heat)"
    )
  
  # (5) Merging all shares together:
  tidy_shares_df <- dplyr::bind_rows(
    tidy_shares_final_fuel_df,
    tidy_shares_final_elec_df,
    tidy_shares_final_heat_df,
    tidy_shares_ff_by_group_inc_elec_heat
  )
  
  
  ### Big second step - Determining average useful stage EROIs from there ###
  
  aggregated_useful_erois <- tidy_shares_df %>%
    dplyr::inner_join(.tidy_erois_df, by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {product})) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]],
                    .data[[eroi.method]], .data[[type]], .data[[boundary]], .data[[non_energy_uses]], .data[[product.group]], .data[[energy.stage]]) %>%
    dplyr::summarise(
      Group.eroi.inversed = sum(.data[[share]] * (1/.data[[useful_stage_eroi]])) / sum(.data[[share]])
    ) %>% 
    dplyr::mutate(
      "{group.eroi}" := 1 / Group.eroi.inversed
    ) %>% 
    dplyr::select(-Group.eroi.inversed)
  
  return(aggregated_useful_erois)
}

