
#' Title
#'
#' @param .tidy_io_erois 
#' @param tidy_FU_efficiencies 
#' @param country 
#' @param method 
#' @param energy_type 
#' @param year 
#' @param product 
#' @param product_without_origin 
#' @param average_efficiency 
#' @param eroi 
#' @param useful_stage_eroi 
#' @param eroi_calc_method 
#'
#' @return
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



#' Title
#'
#' @param .tidy_efficiencies_df 
#' @param .tidy_iea_df 
#' @param include_non_energy_uses 
#' @param final_use_mats 
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
#' @param calc_method 
#'
#' @return
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
                                            product_without_origin = "product_without_origin",
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
  )
  
  
  ### Big second step - Determining average FU efficiencies from there ###
  
  if (calc_method == "dta"){
    
    average_FU_efficiencies <- 1
    
    
  } else if (calc_method == "gma"){
    
    average_FU_efficiencies <- 2
    
  }
  
  return(average_FU_efficiencies)
}






