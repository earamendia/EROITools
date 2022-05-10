
#' Calculates total energy use by product
#' 
#' This function calculates, for each country, year, method, the total energy use by product (for fossil fuels),
#' where the matrices containing energy use flows can be specified in the `total_use_mats` argument. 
#' By default, these matrices are U_EIOU and Y, and excludes energy flows used as feedstock. 
#' Exports and losses are also excluded from the total energy use. See details for more explanations.
#' 
#' The function can work both on a single country Energy Conversion Chain of Domestic Technology Assumption type,
#' or with a multi-regional Energy Conversion Chain for instance using the Global Market Assumption. The input data frame
#' will have to be slightly adapted in this case (for an example see the tests related to the function),
#' and the energy use by product will be returned as the pair (Origin_Country, Product).
#' Non-energy use flows can be included or excluded from the calculations using the `include_non_energy_uses` boolean.
#' 
#' @param .tidy_iea_df Tidy data frame for which to calculate total energy use by product
#' @param include_non_energy_uses A boolean defining whether non-energy uses should be included
#'                                in the calculation of total energy uses. Default is FALSE.
#' @param total_use_mats A list describing from which matrices should total final energy uses be calculated.
#'                       Default is `c(IEATools::psut_cols$Y, IEATools::psut_cols$U_eiou)`.
#' @param list_oil_products A list of oil products.
#'                          Default is `IEATools::oil_and_oil_products`.
#' @param list_coal_products A list of coal products.
#'                           Default is `IEATools::coal_and_coal_products`.
#' @param list_gas_products A list of natural gas products.
#'                          Default is `IEATools::primary_gas_products`.
#' @param list_non_energy_flows A list containing the names of non-energy flows in IEA data.
#'                              Default is `IEATools::non_energy_flows`.
#' @param exports A string identifying Exports flows.
#'                Default is `IEATools::interface_industries$exports`.
#' @param losses A string identifying Losses flows.
#'               Default is `IEATools::tfc_compare_flows$losses`.
#' @param country,method,energy_type,last_stage,year,product,unit,flow,e_dot See `IEATools::iea_cols`.
#' @param matnames The column name of the column having matrices names.
#'                 Default is `IEATools::mat_meta_cols$matnames`.
#' @param total_product_use Column name containing total energy use by product.
#'                          Default is "Total_Product_Use".
#' @param product_without_origin Column name containing the name of the product excluding the country of origin.
#'                               Helpful for doing calculations with Global Market Assumption.
#'                               Default is "product_without_origin".
#'
#' @return A data frame containing the values of total energy use by product.
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
        "{e_dot}" := abs(.data[[e_dot]])
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


#' Calculates primary energy supply for the coal products and oil and gas products group
#' 
#' The function calculates primary energy supply for the coal products and oil and gas products group. It does not disaggregate
#' the oil and gas products group because they are jointly extracted, so it is not robust to separate them in EROI calculations,
#' at least from the IEA data we use here.
#'
#' @param .tidy_iea_df A tidy iea data frame for which the primary energy supply needs to be calculated.
#' @param primary_production_mats A list containing the names of matrices containing primary production flows.
#' @param list_primary_oil_products A list containing the names of primary oil products.
#' @param list_primary_coal_products A list containing the names of primary coal products.
#' @param list_primary_gas_products A list containing the names of primary gas products.
#' @param country,method,energy_type,last_stage,year,product,unit,flow,e_dot See `IEATools::iea_cols`.
#' @param matnames The column name of the column having matrices names.
#'                 Default is `IEATools::mat_meta_cols$matnames`.
#' @param product.group The column name of the column defining the fossil fuel group.
#'                      Default is "Product.Group".
#' @param energy.stage The column name of the column defining the energy stage.
#'                      Default is "Energy.stage".
#' @param total_group_supply Column name containing total energy supply by product.
#'                           Default is "Total_Group_Supply".
#' @param product_without_origin Column name containing the name of the product excluding the country of origin.
#'                               Helpful for doing calculations with Global Market Assumption.
#'                               Default is "product_without_origin".
#'
#' @return A tidy data frame returning the total primary energy supply by fossil fuel group.
#' @export
#'
#' @examples
calc_primary_products_supply_by_group <- function(.tidy_iea_df,
                                                  primary_production_mats = c(IEATools::psut_cols$V),
                                                  list_primary_oil_products = IEATools::primary_oil_products,
                                                  list_primary_coal_products = IEATools::primary_coal_products,
                                                  list_primary_gas_products = IEATools::primary_gas_products,
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
                                                  total_group_supply = "Total_Group_Supply",
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
  
  # Calculating primary energy supply by product group:
  to_return <- .tidy_iea_df %>%
    dplyr::filter(.data[[product_without_origin]] %in% c(list_primary_coal_products, list_primary_oil_products, list_primary_gas_products)) %>%
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
    dplyr::summarise("{total_group_supply}" := sum(.data[[e_dot]]))
  
  return(to_return)
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
