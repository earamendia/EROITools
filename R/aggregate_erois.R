
#' Aggregates primary stage EROIs by fossil fuel group
#' 
#' The function calculates the aggregated primary stage EROIs for each fossil fuel group. It determines the shares of supply of each energy product
#' within each fossil fuel group (by default using the V matrix), selects only the list of energy products defined as primary energy products (see arguments),
#' and from these shares determines the average primary stage EROI by fossil fuel group.
#' 
#' The function can work both on a single country Energy Conversion Chain of Domestic Technology Assumption type,
#' or with a multi-regional Energy Conversion Chain for instance using the Global Market Assumption. The input data frame
#' will have to be slightly adapted in this case (for an example see the tests related to the function)
#'
#' @param .tidy_erois_df The tidy erois data frame (calculated for each product via input-output) for which the aggregation needs to be done.
#' @param .tidy_iea_df The `tidy_iea_df` from which the input-output erois have been calculated.
#' @param primary_production_mats The list of matrices to use as primary production matrices to determine the share of supply of each primary energy product.
#'                                Default is `c(IEATools::psut_cols$V)`.
#' @param list_primary_oil_products The list of primary oil products.
#'                                  Default is `IEATools::primary_oil_products`.
#' @param list_primary_coal_products The list of primary coal products.
#'                                  Default is `IEATools::primary_coal_products`.
#' @param list_primary_gas_products The list of primary gas products.
#'                                  Default is `IEATools::primary_gas_products`.
#' @param product.group The name of the column containing the product group name.
#'                      Default is "Product.Group".
#' @param country,method,energy_type,last_stage,year,product See `IEATools::iea_cols`.
#' @param eroi.method The name of the column containing the method used for calculating the erois.
#'                    Default is "Eroi.method".
#' @param type The name of the column containing the type of eroi calculated.
#'             Default is "Type".
#' @param boundary The name of the column containing the boundary for the eroi calculation.
#'                 Default is "Boundary".
#' @param share The name of the column containing the share of each energy product supply within each fossil fuel group.
#'              Default is "Share".
#' @param eroi The name of the column containing the energy-product level eroi value.
#'             Default is "EROI".
#' @param group.eroi The name of the column containing the fossil fuel group level eroi value.
#'             Default is "Group.eroi".
#' @param energy.stage The name of the column containing the energy stage for the calculation of the EROI.
#'             Default is "Energy.stage".
#' @param product_without_origin The name of the column containing the product name excluding the origin of the product.
#'                               Default is "product_without_origin".
#' @param eroi_calc_method The method being used for calculating the erois.
#'                         Default is "dta".
#' @param Group.eroi.inversed Name of the temporary column that computes the inverse of the EROI.
#'
#' @return A data frame returning the aggregated primary stage EROIs for each fossil fuel group.
#' @export
#'
#' @examples
#' tidy_AB_dta <- ECCTools::tidy_AB_data %>%
#' IEATools::add_psut_matnames() %>% 
#' ECCTools::transform_to_dta()
#' tidy_io_AB_dta <- tidy_AB_dta %>% 
#'  IEATools::prep_psut() %>% 
#'  Recca::calc_io_mats(method_q_calculation = "sum_R_V_cols")
#' tidy_AB_erois_dta <- tidy_io_AB_dta %>% 
#'  Recca::calc_E_EIOU() %>% 
#'  Recca::calc_erois() %>% 
#'  EROITools::extract_tidy_product_erois() %>% 
#'  dplyr::mutate(
#'    Eroi.method = "DTA"
#'  ) %>% 
#'  dplyr::relocate(.tidyselect::all_of(Eroi.method), .after = tidyselect::all_of(Year)) %>% 
#' res_dta <- aggregate_primary_stage_erois(
#'  .tidy_erois_df = tidy_AB_erois_dta,
#'  .tidy_iea_df = tidy_AB_dta,
#'  eroi_calc_method = "dta"
#' )
aggregate_primary_stage_erois <- function(.tidy_erois_df,
                                          .tidy_iea_df,
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
                                          eroi.method = "Eroi.method",
                                          type = "Type",
                                          boundary = "Boundary",
                                          share = "Share",
                                          eroi = "EROI",
                                          group.eroi = "Group.eroi",
                                          energy.stage = "Energy.stage",
                                          product_without_origin = "product_without_origin",
                                          Group.eroi.inversed = "Group.eroi.inversed",
                                          eroi_calc_method = c("dta", "gma")){
  
  eroi_calc_method <- match.arg(eroi_calc_method)
  
  ### (1) Preparing the .tidy_iea_df so that it has a new "product_without_origin" column,
  # which will be equal to "product" when we are not using a MR-PSUT framework
  cols_to_check <- c(product_without_origin = NA_character_)
  
  .tidy_iea_df <- tibble::add_column(.tidy_iea_df, !!!cols_to_check[!names(cols_to_check) %in% names(.tidy_iea_df)]) %>% 
    dplyr::mutate(
      "{product_without_origin}" := dplyr::case_when(
        is.na(.data[[product_without_origin]]) ~ .data[[product]],
        TRUE ~ .data[[product_without_origin]]
      )
    )
  
  # Pulling out the list of countries contained in .tidy_iea_df
  list_countries_in_tidy_df <- .tidy_iea_df %>%
    dplyr::ungroup() %>%
    dplyr::select(tidyselect::all_of(country)) %>% 
    dplyr::distinct() %>%
    dplyr::pull()
  
  
  ### (2) Calculating tidy shares by product at the primary stage ###
  # Group also includes "All fossil fuels".
  tidy_shares_primary_df <- calc_share_primary_ff_supply_by_product_by_group(.tidy_iea_df,
                                                                             primary_production_mats = primary_production_mats,
                                                                             list_primary_oil_products = list_primary_oil_products,
                                                                             list_primary_coal_products = list_primary_coal_products,
                                                                             list_primary_gas_products = list_primary_gas_products)

  
  ### (3) Determining average primary stage EROIs (so, aggregating) from here
  if (eroi_calc_method == "dta"){
    
    aggregated_primary_stage_erois <- tidy_shares_primary_df %>%
      dplyr::inner_join(.tidy_erois_df, by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {product})) %>%
      dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]],
                      .data[[eroi.method]], .data[[type]], .data[[boundary]], .data[[product.group]], .data[[energy.stage]]) %>%
      dplyr::summarise(
        "{Group.eroi.inversed}" := sum(.data[[share]] * (1/.data[[eroi]])) / sum(.data[[share]])
      ) %>% 
      dplyr::mutate(
        "{group.eroi}" := 1 / .data[[Group.eroi.inversed]]
      ) %>% 
      dplyr::select(-tidyselect::all_of(Group.eroi.inversed))
    
    return(aggregated_primary_stage_erois)
    
  } else if (eroi_calc_method == "gma"){
    
    aggregated_primary_stage_erois <- tidy_shares_primary_df %>%
      #dplyr::inner_join(.tidy_erois_df, by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {product})) %>%
      dplyr::inner_join(.tidy_erois_df %>%
                          dplyr::select(-tidyselect::all_of(country)), by = c({method}, {energy_type}, {last_stage}, {year}, {product})) %>%
      dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]],
                      .data[[eroi.method]], .data[[type]], .data[[boundary]], .data[[product.group]], .data[[energy.stage]]) %>%
      dplyr::summarise(
        "{Group.eroi.inversed}" := sum(.data[[share]] * (1/.data[[eroi]])) / sum(.data[[share]])
      ) %>% 
      dplyr::mutate(
        "{group.eroi}" := 1 / .data[[Group.eroi.inversed]]
      ) %>% 
      dplyr::select(-tidyselect::all_of(Group.eroi.inversed))
    
    return(aggregated_primary_stage_erois)
    
  }
}


#' Aggregates final stage EROIs by fossil fuel group
#' 
#' The function calculates the aggregated final stage EROIs for each fossil fuel group. It determines the shares of uses of each energy product
#' within each fossil fuel group (by default using the Y and U_eiou matrices), and from these shares determines the average final stage EROI by fossil fuel group.
#' 
#' The function can work both on a single country Energy Conversion Chain of Domestic Technology Assumption type,
#' or with a multi-regional Energy Conversion Chain for instance using the Global Market Assumption. The input data frame
#' will have to be slightly adapted in this case (for an example see the tests related to the function)
#'
#' @param .tidy_erois_df The tidy erois data frame (calculated for each product via input-output) for which the aggregation needs to be done.
#' @param .tidy_iea_df The `tidy_iea_df` from which the input-output erois have been calculated.
#' @param include_non_energy_uses A boolean indicating whether the calculation of use shares includes non-energy uses.
#'                                Default is FALSE.
#' @param final_use_mats The list of matrices to be used for calculating the use shares by energy product.
#'                       Default is `c(IEATools::psut_cols$Y, IEATools::psut_cols$U_eiou)`.
#' @param list_oil_products The list of oil products to be used when calculating the use shares.
#'                          Default is `IEATools::oil_and_oil_products`.
#' @param list_coal_products The list of coal products to be used when calculating the use shares.
#'                           Default is `IEATools::coal_and_coal_products`.
#' @param list_gas_products The list of gas products to be used when calculating the use shares.
#'                           Default is `IEATools::primary_gas_products`.
#' @param product.group The name of the column containing the product group name.
#'                      Default is "Product.Group".
#' @param country,method,energy_type,last_stage,year,product See `IEATools::iea_cols`.
#' @param non_energy_uses The name of the column stating whether non-energy uses are included in the calculation of use shares.
#'                      Default is "Non_Energy_Uses".
#' @param eroi.method The name of the column containing the method used for calculating the erois.
#'                    Default is "Eroi.method".
#' @param type The name of the column containing the type of eroi calculated.
#'             Default is "Type".
#' @param boundary The name of the column containing the boundary for the eroi calculation.
#'                 Default is "Boundary".
#' @param share The name of the column containing the share of each energy product supply within each fossil fuel group.
#'              Default is "Share".
#' @param eroi The name of the column containing the energy-product level eroi value.
#'             Default is "EROI".
#' @param group.eroi The name of the column containing the fossil fuel group level eroi value.
#'             Default is "Group.eroi".
#' @param energy.stage The name of the column containing the energy stage for the calculation of the EROI.
#'             Default is "Energy.stage".
#' @param product_without_origin The name of the column containing the product name excluding the origin of the product.
#'                               Default is "product_without_origin".
#' @param eroi_calc_method The method being used for calculating the erois.
#'                         Default is "dta".
#' @param Group.eroi.inversed Name of the temporary column that computes the inverse of the EROI.
#'
#' @return A tidy data frame containing the aggregated final stage EROIs.
#' @export
#'
#' @examples
#' tidy_AB_dta <- ECCTools::tidy_AB_data %>%
#' IEATools::add_psut_matnames() %>% 
#' ECCTools::transform_to_dta()
#' tidy_io_AB_dta <- tidy_AB_dta %>% 
#'  IEATools::prep_psut() %>% 
#'  Recca::calc_io_mats(method_q_calculation = "sum_R_V_cols")
#' tidy_AB_erois_dta <- tidy_io_AB_dta %>% 
#'  Recca::calc_E_EIOU() %>% 
#'  Recca::calc_erois() %>% 
#'  EROITools::extract_tidy_product_erois() %>% 
#'  dplyr::mutate(
#'    Eroi.method = "DTA"
#'  ) %>% 
#'  dplyr::relocate(.tidyselect::all_of(Eroi.method), .after = tidyselect::all_of(Year)) %>% 
#' res_dta <- aggregate_final_stage_erois(
#'  .tidy_erois_df = tidy_AB_erois_dta,
#'  .tidy_iea_df = tidy_AB_dta,
#'  eroi_calc_method = "dta"
#' )
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
                                        product_without_origin = "product_without_origin",
                                        Group.eroi.inversed = "Group.eroi.inversed",
                                        eroi_calc_method = c("dta", "gma")){
  
  eroi_calc_method <- match.arg(eroi_calc_method)
  
  ### Preparing the .tidy_iea_df so that it has a new "product_without_origin" column,
  # which will be equal to "product" when we are not using a MR-PSUT framework
  cols_to_check <- c(product_without_origin = NA_character_)
  
  .tidy_iea_df <- tibble::add_column(.tidy_iea_df, !!!cols_to_check[!names(cols_to_check) %in% names(.tidy_iea_df)]) %>% 
    dplyr::mutate(
      "{product_without_origin}" := dplyr::case_when(
        is.na(.data[[product_without_origin]]) ~ .data[[product]],
        TRUE ~ .data[[product_without_origin]]
      )
    )
    
    # .tidy_iea_df %>%
    # tibble::add_column(!!!cols_to_check[!names(cols_to_check) %in% names(.)]) %>%
    # dplyr::mutate(
    #   "{product_without_origin}" := dplyr::case_when(
    #     is.na(.data[[product_without_origin]]) ~ .data[[product]],
    #     TRUE ~ .data[[product_without_origin]]
    #   )
    # )
  
  # Pulling out the list of countries contained in .tidy_iea_df
  list_countries_in_tidy_df <- .tidy_iea_df %>%
    dplyr::ungroup() %>%
    dplyr::select(tidyselect::all_of(country)) %>% 
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
  
  if (eroi_calc_method == "dta"){
  
    aggregated_final_stage_erois <- tidy_shares_df %>%
      dplyr::inner_join(.tidy_erois_df, by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {product})) %>%
      dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]],
                      .data[[eroi.method]], .data[[type]], .data[[boundary]], .data[[non_energy_uses]], .data[[product.group]], .data[[energy.stage]]) %>%
      dplyr::summarise(
        "{Group.eroi.inversed}" := sum(.data[[share]] * (1/.data[[eroi]])) / sum(.data[[share]])
      ) %>% 
      dplyr::mutate(
        "{group.eroi}" := 1 / .data[[Group.eroi.inversed]]
      ) %>% 
      dplyr::select(-tidyselect::all_of(Group.eroi.inversed))
    
    return(aggregated_final_stage_erois)
    
  } else if (eroi_calc_method == "gma"){
   
    aggregated_final_stage_erois <- tidy_shares_df %>%
      dplyr::inner_join(.tidy_erois_df %>%
                          dplyr::select(-tidyselect::all_of(country)), by = c({method}, {energy_type}, {last_stage}, {year}, {product})) %>%
      dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]],
                      .data[[eroi.method]], .data[[type]], .data[[boundary]], .data[[non_energy_uses]], .data[[product.group]], .data[[energy.stage]]) %>%
      dplyr::summarise(
        "{Group.eroi.inversed}" := sum(.data[[share]] * (1/.data[[eroi]])) / sum(.data[[share]])
      ) %>% 
      dplyr::mutate(
        "{group.eroi}" := 1 / .data[[Group.eroi.inversed]]
      ) %>% 
      dplyr::select(-tidyselect::all_of(Group.eroi.inversed))
    
    return(aggregated_final_stage_erois)
     
  }
}



#' Aggregates useful stage EROIs by fossil fuel group
#' 
#' The function calculates the aggregated useful stage EROIs for each fossil fuel group. It determines the shares of uses of each energy product
#' within each fossil fuel group (by default using the Y and U_eiou matrices), and from these shares determines the average useful stage EROI by fossil fuel group.
#'
#' The function can work both on a single country Energy Conversion Chain of Domestic Technology Assumption type,
#' or with a multi-regional Energy Conversion Chain for instance using the Global Market Assumption. The input data frame
#' will have to be slightly adapted in this case (for an example see the tests related to the function)
#'
#' @param .tidy_erois_df The tidy erois data frame for which the aggregation needs to be done.
#' @param .tidy_iea_df The `tidy_iea_df` from which the input-output erois have been calculated.
#' @param include_non_energy_uses A boolean indicating whether the calculation of use shares includes non-energy uses.
#'                                Default is FALSE.
#' @param final_use_mats The list of matrices to be used for calculating the use shares by energy product.
#'                       Default is `c(IEATools::psut_cols$Y, IEATools::psut_cols$U_eiou)`.
#' @param list_oil_products The list of oil products to be used when calculating the use shares.
#'                          Default is `IEATools::oil_and_oil_products`.
#' @param list_coal_products The list of coal products to be used when calculating the use shares.
#'                           Default is `IEATools::coal_and_coal_products`.
#' @param list_gas_products The list of gas products to be used when calculating the use shares.
#'                           Default is `IEATools::primary_gas_products`.
#' @param product.group The name of the column containing the product group name.
#'                      Default is "Product.Group".
#' @param country,method,energy_type,last_stage,year,product See `IEATools::iea_cols`.
#' @param non_energy_uses The name of the column stating whether non-energy uses are included in the calculation of use shares.
#'                      Default is "Non_Energy_Uses".
#' @param eroi.method The name of the column containing the method used for calculating the erois.
#'                    Default is "Eroi.method".
#' @param type The name of the column containing the type of eroi calculated.
#'             Default is "Type".
#' @param boundary The name of the column containing the boundary for the eroi calculation.
#'                 Default is "Boundary".
#' @param share The name of the column containing the share of each energy product supply within each fossil fuel group.
#'              Default is "Share".
#' @param useful_stage_eroi The name of the column containing the useful stage EROIs for each energy product.
#' @param group.eroi The name of the column containing the fossil fuel group level eroi value.
#'             Default is "Group.eroi".
#' @param energy.stage The name of the column containing the energy stage for the calculation of the EROI.
#'             Default is "Energy.stage".
#' @param product_without_origin The name of the column containing the product name excluding the origin of the product.
#'                               Default is "product_without_origin".
#' @param eroi_calc_method The method being used for calculating the erois.
#'                         Default is "dta".
#' @param Group.eroi.inversed Name of the temporary column that computes the inverse of the EROI.
#'
#' @return A tidy data frame containing the aggregated useful stage EROIs by fossil fuel group.
#' @export
#'
#' @examples
#' tidy_AB_dta <- ECCTools::tidy_AB_data %>%
#' IEATools::add_psut_matnames() %>% 
#' ECCTools::transform_to_dta()
#' # Calculating IO matrices
#' tidy_io_AB_dta <- tidy_AB_dta %>%
#'  IEATools::prep_psut() %>%
#'  Recca::calc_io_mats(method_q_calculation = "sum_R_V_cols")
#' # Calculating tidy IO EROIs
#' tidy_AB_erois_dta <- tidy_io_AB_dta %>%
#'  Recca::calc_E_EIOU() %>%
#'  Recca::calc_erois() %>%
#'  EROITools::extract_tidy_product_erois() %>%
#'  dplyr::mutate(
#'    Eroi.method = "DTA"
#'  ) %>%
#'  dplyr::relocate(.tidyselect::all_of(Eroi.method), .after = tidyselect::all_of(Year)) %>% 
#' # Pushing to tidy useful stage EROIs
#' length_to_use <- tidy_AB_erois_dta %>% 
#'  dplyr::select(Country, Method, Energy.type, Year, Product) %>% 
#'  dplyr::distinct() %>% 
#'  nrow()
#' tidy_FU_efficiencies_dta <- tidy_AB_erois_dta %>% 
#'  dplyr::select(Country, Method, Energy.type, Year, Product) %>% 
#'  dplyr::distinct() %>% 
#'  dplyr::mutate(
#'    Average_Efficiency_Global = seq(0.15, 1, 0.85/(length_to_use-1))
#'  )
#' tidy_useful_erois_dta <- tidy_AB_erois_dta %>% 
#'  dplyr::left_join(tidy_FU_efficiencies_dta,
#'                   by = c("Country", "Method", "Energy.type", "Year", "Product")) %>%
#'  dplyr::mutate(
#'    Useful_Stage_EROI = Average_Efficiency_Global * EROI
#'  ) %>% 
#'  dplyr::filter(! is.na(Useful_Stage_EROI))
#' # Calculating aggregated EROIs:
#' res_dta <- aggregate_useful_stage_erois(
#'  .tidy_erois_df = tidy_useful_erois_dta,
#'  .tidy_iea_df = tidy_AB_dta,
#'  eroi_calc_method = "dta"
#' )
aggregate_useful_stage_erois <- function(.tidy_erois_df,
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
                                         useful_stage_eroi = "Useful_Stage_EROI",
                                         group.eroi = "Group.eroi",
                                         energy.stage = "Energy.stage",
                                         product_without_origin = "product_without_origin",
                                         Group.eroi.inversed = "Group.eroi.inversed",
                                         eroi_calc_method = c("dta", "gma")){
  
  ### Preparing the .tidy_iea_df so that it has a new "product_without_origin" column,
  # which will be equal to "product" when we are not using a MR-PSUT framework
  cols_to_check <- c(product_without_origin = NA_character_)
  
  .tidy_iea_df <- tibble::add_column(.tidy_iea_df, !!!cols_to_check[!names(cols_to_check) %in% names(.tidy_iea_df)]) %>% 
    dplyr::mutate(
      "{product_without_origin}" := dplyr::case_when(
        is.na(.data[[product_without_origin]]) ~ .data[[product]],
        TRUE ~ .data[[product_without_origin]]
      )
    )
  
  # Pulling out the list of countries contained in .tidy_iea_df
  list_countries_in_tidy_df <- .tidy_iea_df %>%
    dplyr::ungroup() %>%
    dplyr::select(tidyselect::all_of(country)) %>%
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
  
  if (eroi_calc_method == "dta"){
    
    aggregated_useful_erois <- tidy_shares_df %>%
      dplyr::inner_join(.tidy_erois_df, by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {product})) %>%
      dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]],
                      .data[[eroi.method]], .data[[type]], .data[[boundary]], .data[[non_energy_uses]], .data[[product.group]], .data[[energy.stage]]) %>%
      dplyr::summarise(
        "{Group.eroi.inversed}" := sum(.data[[share]] * (1/.data[[useful_stage_eroi]])) / sum(.data[[share]])
      ) %>% 
      dplyr::mutate(
        "{group.eroi}" := 1 / .data[[Group.eroi.inversed]]
      ) %>% 
      dplyr::select(-tidyselect::all_of(Group.eroi.inversed))
    
    return(aggregated_useful_erois)
    
  } else if (eroi_calc_method == "gma"){
    
    aggregated_useful_erois <- tidy_shares_df %>%
      dplyr::inner_join(.tidy_erois_df, by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {product})) %>%
      dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]],
                      .data[[eroi.method]], .data[[type]], .data[[boundary]], .data[[non_energy_uses]], .data[[product.group]], .data[[energy.stage]]) %>%
      dplyr::summarise(
        "{Group.eroi.inversed}" := sum(.data[[share]] * (1/.data[[useful_stage_eroi]])) / sum(.data[[share]])
      ) %>% 
      dplyr::mutate(
        "{group.eroi}" := 1 / .data[[Group.eroi.inversed]]
      ) %>% 
      dplyr::select(-tidyselect::all_of(Group.eroi.inversed))
    
    return(aggregated_useful_erois)
    
  }
}

