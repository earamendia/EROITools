
#' Push final stage EROIs to useful stage EROIs
#' 
#' This function calculates the useful stage EROIs based on the final stage EROIs and on the average final-to-useful efficiencies provided for each energy product.
#' It also works if the efficiencies are provided by end-use, or by sector - then end-use or sector specific useful stage EROIs are returned.
#'
#' @param .tidy_io_erois The `.tidy_io_erois` data frame for which useful stage EROIs need to be calculated.
#' @param tidy_FU_efficiencies A tidy data frame containing the final-to-useful efficiencies to use for the calculation of useful stage EROIs.
#' @param country,method,energy_type,year,product See `IEATools::iea_cols`.
#' @param product_without_origin The name of the column containing the product name excluding the product origin.
#'                               Default is "product_without_origin".
#' @param average_efficiency The name of the column containing the average final-to-useful efficiency to apply to each energy product.
#'                           Default is "Average_Efficiency_Col".
#' @param eroi The name of the column containing EROIs.
#'             Default is "EROI".
#' @param useful_stage_eroi The name of the column containing the useful stage EROI values.
#'                          Default is "Useful_Stage_EROI".
#' @param eroi_calc_method The calculation method being used, either DTA if working on single country, or GMA if working with a multi-regional framework.
#'                         Default is "dta".
#'
#' @return The `.tidy_io_erois` data frame with useful stage EROIs added.
#' @export
#'
#' @examples
push_to_useful_erois <- function(.tidy_io_erois,
                                 tidy_FU_efficiencies,
                                 country = IEATools::iea_cols$country,
                                 method = IEATools::iea_cols$method,
                                 energy_type = IEATools::iea_cols$energy_type,
                                 year = IEATools::iea_cols$year,
                                 product = IEATools::iea_cols$product,
                                 product_without_origin = "product_without_origin",
                                 average_efficiency = "Average_Efficiency_Col",
                                 eroi = "EROI",
                                 useful_stage_eroi = "Useful_Stage_EROI",
                                 eroi_calc_method = c("dta", "gma")){
  
  eroi_calc_method <- match.arg(eroi_calc_method)
  
  if (eroi_calc_method == "dta"){
    # Bind and calculate useful stage EROIs:
    tidy_useful_erois <- .tidy_io_erois %>% 
      dplyr::left_join(
        tidy_FU_efficiencies,
        by = c({country}, {method}, {energy_type}, {year}, {product})
      ) %>% 
      dplyr::mutate(
        "{useful_stage_eroi}" := .data[[eroi]] * .data[[average_efficiency]]
      ) %>% 
      dplyr::filter(! is.na(.data[[useful_stage_eroi]]))
    
  } else if (eroi_calc_method == "gma"){
    # Adapt EROI data frame to add a product without origin column:
    tidy_io_erois_adapted <- .tidy_io_erois %>% 
      dplyr::mutate(
        "{product_without_origin}" := stringr::str_remove(.data[[product]], "\\{.*\\}_")
      ) %>% 
      dplyr::select(-.data[[country]])
    
    # Calculate useful stage EROIs data frame
    tidy_useful_erois <- tidy_FU_efficiencies %>% 
      dplyr::rename(
        "{product_without_origin}" := .data[[product]]
      ) %>% 
      dplyr::left_join(tidy_io_erois_adapted, by = c({method}, {energy_type}, {year}, {product_without_origin})) %>% 
      dplyr::mutate(
        "{useful_stage_eroi}" := .data[[eroi]] * .data[[average_efficiency]]
      ) %>% 
      dplyr::select(-.data[[product_without_origin]]) %>% 
      dplyr::filter(! is.na(.data[[useful_stage_eroi]]))
  }
  
  return(tidy_useful_erois)
}



#' Calculates average final-to-useful efficiency by fossil fuel group
#' 
#' This function calculates the average final-to-useful efficiencies for each fossil fuel group using the average efficiency of each energy product
#' and the shares of use of each energy product within each fossil fuel group.
#'
#' @param .tidy_efficiencies_df The tidy efficiencies data frame which provides the efficiencies to use for each energy product.
#' @param .tidy_iea_df The `.tidy_iea_df`, from which the shares of use of each energy product will be determined.
#' @param include_non_energy_uses A boolean stating whether non_energy_uses should be used in the calculation of the use shares of each energy product/
#'                                Default is FALSE.
#' @param final_use_mats The list of matrices that should be used for the calculation of the use shares of each energy product.
#'                       Default is `c(IEATools::psut_cols$Y, IEATools::psut_cols$U_eiou)`.
#' @param list_oil_products The list of oil products to use for the calculation of the use shares of each energy product.
#'                          Default is `IEATools::oil_and_oil_products`.
#' @param list_coal_products The list of coal products to use for the calculation of the use shares of each energy product.
#'                           Default is `IEATools::coal_and_coal_products`.
#' @param list_gas_products The list of gas products to use for the calculation of the use shares of each energy product.
#'                           Default is `IEATools::primary_gas_products`.
#' @param product.group The name of the column containing the name of the product group.
#'                      Default is "Product.Group".
#' @param country,method,energy_type,last_stage,year,product See `IEATools::iea_cols`.
#' @param share The name of the column name containing the shares of use of each energy product.
#'              Default is "Share".
#' @param useful_stage_eroi The name of the column containing the useful stage EROI.
#'                          Default is "Useful_Stage_EROI".
#' @param group.eroi The name of the column containing the group level EROI.
#'                   Default is "Group.eroi".
#' @param energy.stage The name of the column containing the energy stage for which the efficiencies are calculated.
#'                     Default is "Energy.stage".
#' @param product_without_origin The name of the column containing the product name excluding its origin.
#'                               Default is "product_without_origin".
#' @param calc_method The calculation method being used, either DTA if working on single country, or GMA if working with a multi-regional framework.
#'                    Default is "dta".
#' @param average_efficiency The name of the column containing the average efficiency of each energy product.
#'                           Default is "Average_Efficiency_Col".
#' @param aggregated_efficiency The name of the column containing the aggregated efficiency for each fossil fuel group.
#'                              Default is "Aggregated_Efficiency".
#'
#' @return A tidy data frame with average final-to-useful efficiencies calculated for each fossil fuel group.
#' @export
#'
#' @examples
calc_avg_efficiency_by_ff_group <- function(.tidy_efficiencies_df,
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
                                            share = "Share",
                                            useful_stage_eroi = "Useful_Stage_EROI",
                                            group.eroi = "Group.eroi",
                                            energy.stage = "Energy.stage",
                                            product_without_origin = "product_without_origin",
                                            average_efficiency = "Average_Efficiency_Col",
                                            aggregated_efficiency = "Aggregated_Efficiency",
                                            calc_method = c("dta", "gma")){
  
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
  ) %>% 
    dplyr::mutate(
      "{product_without_origin}" := stringr::str_remove(.data[[product]], "\\{.*\\}_")
    )
  
  
  ### Big second step - Determining average FU efficiencies from there ###
  
  if (calc_method == "dta"){
    
    average_FU_efficiencies <- tidy_shares_df %>% 
      dplyr::ungroup() %>% 
      # Those joins that fail are primary energy products
      # which are systematically ascribed to non-energy uses in the PFU database.
      # very careful here is this changes in the future in the PFU database....!
      dplyr::inner_join(.tidy_efficiencies_df, by = c({country}, {method}, {energy_type}, {year}, {product})) %>% 
      dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[energy.stage]], .data[[year]], .data[[product.group]]) %>% 
      dplyr::summarise(
        "{aggregated_efficiency}" := sum(.data[[share]] * .data[[average_efficiency]]) / sum(.data[[share]])
      )
    
  } else if (calc_method == "gma"){
    
    average_FU_efficiencies <- tidy_shares_df %>% 
      dplyr::ungroup() %>% 
      # Those joins that fail are primary energy products
      # which are systematically ascribed to non-energy uses in the PFU database.
      # very careful here is this changes in the future in the PFU database....!
      dplyr::inner_join(.tidy_efficiencies_df %>% 
                          dplyr::rename("{product_without_origin}" := .data[[product]]), 
                        by = c({country}, {method}, {energy_type}, {year}, {product_without_origin})) %>% 
      dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[energy.stage]], .data[[year]], .data[[product.group]]) %>% 
      dplyr::summarise(
        "{aggregated_efficiency}" := sum(.data[[share]] * .data[[average_efficiency]]) / sum(.data[[share]])
      )
  }
  return(average_FU_efficiencies)
}

