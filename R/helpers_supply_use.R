# This script provides helpers for summarising EROIs

# This script defines helpers functions for the EROI summaries.

# Note it excludes exports, it excludes U_feed.
#' Title
#'
#' @param .tidy_iea_df
#' @param include_non_energy_uses
#' @param total_use_mats
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
#' @param total_product_use
#'
#' @return
#' @export
#'
#' @examples
calc_total_use_by_product <- function(.tidy_iea_df,
                                      include_non_energy_uses = FALSE,
                                      total_use_mats = c(IEATools::psut_cols$Y, IEATools::psut_cols$U_eiou),
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
                                      total_product_use = "Total_Product_Use",
                                      product_without_origin = "product_without_origin"
){
  
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
  
  if (isFALSE(include_non_energy_uses)){
    
    to_return <- .tidy_iea_df %>%
      dplyr::filter(.data[[product_without_origin]] %in% c(list_coal_products, list_oil_products, list_gas_products)) %>%
      dplyr::filter(
        ! (stringr::str_detect(.data[[flow]], exports) | stringr::str_detect(.data[[flow]], losses))
      ) %>%
      dplyr::filter(matnames %in% total_use_mats) %>%
      dplyr::filter(! .data[[flow]] %in% list_non_energy_flows) %>%
      dplyr::mutate(
        "{e_dot}" = abs(.data[[e_dot]])
      ) %>%
      dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product]],
                      .data[[unit]], .data[[product_without_origin]]) %>%
      dplyr::summarise(
        "{total_product_use}" := sum(.data[[e_dot]])
      )
    
    return(to_return)
    
  } else if (isTRUE(include_non_energy_uses)){
    
    to_return <- .tidy_iea_df %>%
      dplyr::filter(.data[[product]] %in% c(list_coal_products, list_oil_products, list_gas_products)) %>%
      dplyr::filter(
        ! (stringr::str_detect(.data[[flow]], exports) | stringr::str_detect(.data[[flow]], losses))
      ) %>%
      dplyr::filter(matnames %in% total_use_mats) %>%
      dplyr::mutate(
        "{e_dot}" := abs(.data[[e_dot]])
      ) %>%
      dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product]], .data[[unit]]) %>%
      dplyr::summarise(
        "{total_product_use}" := sum(.data[[e_dot]])
      )
    
    return(to_return)
    
  } else {
    stop("The include_non_energy_uses argument must be either TRUE or FALSE.")
  }
}


# Calculate primary products use for each main group: coal products and oil&gas products
#' Title
#'
#' @param .tidy_iea_df
#' @param include_non_energy_uses
#' @param primary_production_mats
#' @param list_primary_oil_products
#' @param list_primary_coal_products
#' @param list_primary_gas_products
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
#' @param total_group_use
#'
#' @return
#' @export
#'
#' @examples
calc_primary_products_supply_by_group <- function(.tidy_iea_df,
                                                  include_non_energy_uses = FALSE,
                                                  primary_production_mats = c(IEATools::psut_cols$V),
                                                  list_primary_oil_products = IEATools::primary_oil_products,
                                                  list_primary_coal_products = IEATools::primary_coal_products,
                                                  list_primary_gas_products = IEATools::primary_gas_products,
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
                                                  total_group_use = "Total_Group_Use",
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
  
  if (isFALSE(include_non_energy_uses)){
    
    to_return <- .tidy_iea_df %>%
      dplyr::filter(.data[[product_without_origin]] %in% c(list_primary_coal_products, list_primary_oil_products, list_primary_gas_products)) %>%
      dplyr::filter(
        ! (stringr::str_detect(.data[[flow]], exports) | stringr::str_detect(.data[[flow]], losses))
      ) %>%
      dplyr::filter(matnames %in% primary_production_mats) %>%
      dplyr::filter(! .data[[flow]] %in% list_non_energy_flows) %>%
      dplyr::mutate(
        "{product.group}" := dplyr::case_when(
          .data[[product_without_origin]] %in% c(list_primary_oil_products, list_primary_gas_products) ~ "Oil and gas products",
          .data[[product_without_origin]] %in% list_primary_coal_products ~ "Coal products"
        ),
        "{energy.stage}" := "Primary",
        "{e_dot}" := abs(.data[[e_dot]])
      ) %>%
      dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product.group]],
                      .data[[energy.stage]], .data[[unit]]) %>%
      dplyr::summarise(
        "{total_group_use}" := sum(.data[[e_dot]])
      )
    
    return(to_return)
    
  } else if (isTRUE(include_non_energy_uses)){
    
    to_return <- .tidy_iea_df %>%
      dplyr::filter(.data[[product_without_origin]] %in% c(list_primary_coal_products, list_primary_oil_products, list_primary_gas_products)) %>%
      dplyr::filter(
        ! (stringr::str_detect(.data[[flow]], exports) | stringr::str_detect(.data[[flow]], losses))
      ) %>%
      dplyr::filter(matnames %in% primary_production_mats) %>%
      dplyr::mutate(
        "{product.group}" := dplyr::case_when(
          .data[[product_without_origin]] %in% c(list_primary_oil_products, list_primary_gas_products) ~ "Oil and gas products",
          .data[[product_without_origin]] %in% list_primary_coal_products ~ "Coal products"
        ),
        "{energy.stage}" := "Primary",
        "{e_dot}" := abs(.data[[e_dot]])
      ) %>%
      dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product.group]],
                      .data[[energy.stage]], .data[[unit]]) %>%
      dplyr::summarise(
        "{total_group_use}" := sum(.data[[e_dot]])
      )
    
    return(to_return)
    
  } else {
    stop("The include_non_energy_uses argument must be either TRUE or FALSE.")
  }
}


# Calculates use of products by main group (coal vs oil&gas)
#' Title
#'
#' @param .tidy_iea_df
#' @param include_non_energy_uses
#' @param final_use_mats
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
#' @param total_group_use
#' @param energy.stage
#'
#' @return
#' @export
#'
#' @examples
calc_all_products_use_by_group <- function(.tidy_iea_df,
                                           include_non_energy_uses = FALSE,
                                           final_use_mats = c(IEATools::psut_cols$Y, IEATools::psut_cols$U_eiou),
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
                                           total_group_use = "Total_Group_Use",
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
  
  if (isFALSE(include_non_energy_uses)){
    
    to_return_1 <- .tidy_iea_df %>%
      dplyr::filter(.data[[product_without_origin]] %in% c(list_coal_products, list_oil_products, list_gas_products)) %>%
      dplyr::filter(
        ! (stringr::str_detect(.data[[flow]], exports) | stringr::str_detect(.data[[flow]], losses))
      ) %>%
      dplyr::filter(matnames %in% final_use_mats) %>%
      dplyr::filter(! .data[[flow]] %in% list_non_energy_flows) %>%
      dplyr::mutate(
        "{product.group}" := dplyr::case_when(
          .data[[product_without_origin]] %in% c(list_oil_products, list_gas_products) ~ "Oil and gas products",
          .data[[product_without_origin]] %in% list_coal_products ~ "Coal products"
        ),
        "{energy.stage}" := "Final (fuel)",
        "{e_dot}" := abs(.data[[e_dot]])
      ) %>%
      dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product.group]],
                      .data[[energy.stage]], .data[[unit]]) %>%
      dplyr::summarise(
        "{total_group_use}" := sum(.data[[e_dot]])
      )
    
    # Duplicating the data frame, but splitting oil and natural gas, and taking out coal products
    to_return_2 <- .tidy_iea_df %>%
      dplyr::filter(.data[[product_without_origin]] %in% c(list_coal_products, list_oil_products, list_gas_products)) %>%
      dplyr::filter(
        ! (stringr::str_detect(.data[[flow]], exports) | stringr::str_detect(.data[[flow]], losses))
      ) %>%
      dplyr::filter(matnames %in% final_use_mats) %>%
      dplyr::filter(! .data[[flow]] %in% list_non_energy_flows) %>%
      dplyr::mutate(
        "{product.group}" := dplyr::case_when(
          .data[[product_without_origin]] %in% list_oil_products ~ "Oil products",
          .data[[product_without_origin]] %in% list_gas_products ~ "Natural gas",
          .data[[product_without_origin]] %in% list_coal_products ~ "Coal products"
        ),
        "{energy.stage}" := "Final (fuel)",
        "{e_dot}" := abs(.data[[e_dot]])
      ) %>%
      dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product.group]],
                      .data[[energy.stage]], .data[[unit]]) %>%
      dplyr::summarise(
        "{total_group_use}" := sum(.data[[e_dot]])
      ) %>%
      dplyr::filter(.data[[product.group]] != "Coal products")
    
    # Binding both sets
    to_return <- dplyr::bind_rows(
      to_return_1,
      to_return_2
    )
    
    return(to_return)
    
  } else if (isTRUE(include_non_energy_uses)){
    
    to_return_1 <- .tidy_iea_df %>%
      dplyr::filter(.data[[product_without_origin]] %in% c(list_coal_products, list_oil_products, list_gas_products)) %>%
      dplyr::filter(
        ! (stringr::str_detect(.data[[flow]], exports) | stringr::str_detect(.data[[flow]], losses))
      ) %>%
      dplyr::filter(matnames %in% final_use_mats) %>%
      dplyr::mutate(
        "{product.group}" := dplyr::case_when(
          .data[[product_without_origin]] %in% c(list_oil_products, list_gas_products) ~ "Oil and gas products",
          .data[[product_without_origin]] %in% list_coal_products ~ "Coal products"
        ),
        "{energy.stage}" := "Final (fuel)",
        "{e_dot}" := abs(.data[[e_dot]])
      ) %>%
      dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product.group]],
                      .data[[energy.stage]], .data[[unit]]) %>%
      dplyr::summarise(
        "{total_group_use}" := sum(.data[[e_dot]])
      )
    
    # Duplicating the data frame, but splitting oil and natural gas, and taking out coal products
    to_return_2 <- .tidy_iea_df %>%
      dplyr::filter(.data[[product_without_origin]] %in% c(list_coal_products, list_oil_products, list_gas_products)) %>%
      dplyr::filter(
        ! (stringr::str_detect(.data[[flow]], exports) | stringr::str_detect(.data[[flow]], losses))
      ) %>%
      dplyr::filter(matnames %in% final_use_mats) %>%
      dplyr::mutate(
        "{product.group}" := dplyr::case_when(
          .data[[product_without_origin]] %in% list_oil_products ~ "Oil products",
          .data[[product_without_origin]] %in% list_gas_products ~ "Natural gas",
          .data[[product_without_origin]] %in% list_coal_products ~ "Coal products"
        ),
        "{energy.stage}" := "Final (fuel)",
        "{e_dot}" := abs(.data[[e_dot]])
      ) %>%
      dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product.group]],
                      .data[[energy.stage]], .data[[unit]]) %>%
      dplyr::summarise(
        "{total_group_use}" := sum(.data[[e_dot]])
      ) %>%
      dplyr::filter(.data[[product.group]] != "Coal products")
    
    # Binding both sets
    to_return <- dplyr::bind_rows(
      to_return_1,
      to_return_2
    )
    
    return(to_return)
    
  } else {
    stop("The include_non_energy_uses argument must be either TRUE or FALSE.")
  }
}


# Calculates total primary fossil fuel use
#' Title
#'
#' @param .tidy_iea_df
#' @param include_non_energy_uses
#' @param primary_production_mats
#' @param list_primary_oil_products
#' @param list_primary_coal_products
#' @param list_primary_gas_products
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
#' @param total_group_use
#' @param energy.stage
#'
#' @return
#' @export
#'
#' @examples
calc_primary_ff_supply <- function(.tidy_iea_df,
                                   include_non_energy_uses = FALSE,
                                   primary_production_mats = c(IEATools::psut_cols$V),
                                   list_primary_oil_products = IEATools::primary_oil_products,
                                   list_primary_coal_products = IEATools::primary_coal_products,
                                   list_primary_gas_products = IEATools::primary_gas_products,
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
                                   total_group_use = "Total_Group_Use",
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
  
  if (isFALSE(include_non_energy_uses)){
    
    to_return <- .tidy_iea_df %>%
      dplyr::filter(.data[[product_without_origin]] %in% c(list_primary_coal_products, list_primary_oil_products, list_primary_gas_products)) %>%
      dplyr::filter(
        ! (stringr::str_detect(.data[[flow]], exports) | stringr::str_detect(.data[[flow]], losses))
      ) %>%
      dplyr::filter(matnames %in% primary_production_mats) %>%
      dplyr::filter(! .data[[flow]] %in% list_non_energy_flows) %>%
      dplyr::mutate(
        "{product.group}" := "All fossil fuels",
        "{energy.stage}" := "Primary",
        "{e_dot}" := abs(.data[[e_dot]])
      ) %>%
      dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product.group]],
                      .data[[energy.stage]], .data[[unit]]) %>%
      dplyr::summarise(
        "{total_group_use}" := sum(.data[[e_dot]])
      )
    
    return(to_return)
    
    
    
  } else if (isTRUE(include_non_energy_uses)){
    
    to_return <- .tidy_iea_df %>%
      dplyr::filter(.data[[product_without_origin]] %in% c(list_primary_coal_products, list_primary_oil_products, list_primary_gas_products)) %>%
      dplyr::filter(
        ! (stringr::str_detect(.data[[flow]], exports) | stringr::str_detect(.data[[flow]], losses))
      ) %>%
      dplyr::filter(matnames %in% primary_production_mats) %>%
      dplyr::mutate(
        "{product.group}" := "All fossil fuels",
        "{energy.stage}" := "Primary",
        "{e_dot}" := abs(.data[[e_dot]])
      ) %>%
      dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product.group]],
                      .data[[energy.stage]], .data[[unit]]) %>%
      dplyr::summarise(
        "{total_group_use}" := sum(.data[[e_dot]])
      )
    
    return(to_return)
    
    
  } else {
    stop("The include_non_energy_uses argument must be either TRUE or FALSE.")
  }
}

# Calculates total fossil fuel use
#' Title
#'
#' @param .tidy_iea_df
#' @param include_non_energy_uses
#' @param final_use_mats
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
#' @param total_group_use
#' @param energy.stage
#'
#' @return
#' @export
#'
#' @examples
calc_ff_use <- function(.tidy_iea_df,
                        include_non_energy_uses = FALSE,
                        final_use_mats = c(IEATools::psut_cols$Y, IEATools::psut_cols$U_eiou),
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
                        total_group_use = "Total_Group_Use",
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
  
  if (isFALSE(include_non_energy_uses)){
    
    to_return <- .tidy_iea_df %>%
      dplyr::filter(.data[[product_without_origin]] %in% c(list_coal_products, list_oil_products, list_gas_products)) %>%
      dplyr::filter(
        ! (stringr::str_detect(.data[[flow]], exports) | stringr::str_detect(.data[[flow]], losses))
      ) %>%
      dplyr::filter(matnames %in% final_use_mats) %>%
      dplyr::filter(! .data[[flow]] %in% list_non_energy_flows) %>%
      dplyr::mutate(
        "{product.group}" := "All fossil fuels",
        "{energy.stage}" := "Final (fuel)",
        "{e_dot}" := abs(.data[[e_dot]])
      ) %>%
      dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product.group]],
                      .data[[energy.stage]], .data[[unit]]) %>%
      dplyr::summarise(
        "{total_group_use}" := sum(.data[[e_dot]])
      )
    
    return(to_return)
    
  } else if (isTRUE(include_non_energy_uses)){
    
    to_return <- .tidy_iea_df %>%
      dplyr::filter(.data[[product_without_origin]] %in% c(list_coal_products, list_oil_products, list_gas_products)) %>%
      dplyr::filter(
        ! (stringr::str_detect(.data[[flow]], exports) | stringr::str_detect(.data[[flow]], losses))
      ) %>%
      dplyr::filter(matnames %in% final_use_mats) %>%
      dplyr::mutate(
        "{product.group}" := "All fossil fuels",
        "{energy.stage}" := "Final (fuel)",
        "{e_dot}" := abs(.data[[e_dot]])
      ) %>%
      dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product.group]],
                      .data[[energy.stage]], .data[[unit]]) %>%
      dplyr::summarise(
        "{total_group_use}" := sum(.data[[e_dot]])
      )
    
    return(to_return)
    
  } else {
    stop("The include_non_energy_uses argument must be either TRUE or FALSE.")
  }
}
