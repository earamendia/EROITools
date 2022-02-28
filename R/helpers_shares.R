

# Calculates shares of each product in the primary fossil fuel supply by group, by default using the V matrix
# Where the groups are: "All fossil fuels", "Oil and gas products", and "Coal products".

#' Title
#'
#' @param .tidy_iea_df
#' @param include_non_energy_uses
#' @param primary_production_mats
#' @param list_primary_oil_products
#' @param list_primary_coal_products
#' @param list_primary_gas_products
#' @param product.group
#' @param total_product_use
#' @param total_group_use
#' @param non_energy_uses
#' @param share
#' @param energy.stage
#' @param country
#' @param method
#' @param energy_type
#' @param last_stage
#' @param year
#' @param unit
#' @param product
#' @param boolean_non_energy_uses
#'
#' @return
#' @export
#'
#' @examples
calc_share_primary_ff_supply_by_product_by_group <- function(.tidy_iea_df,
                                                             include_non_energy_uses = FALSE,
                                                             primary_production_mats = c(IEATools::psut_cols$V),
                                                             list_primary_oil_products = IEATools::primary_oil_products,
                                                             list_primary_coal_products = IEATools::primary_coal_products,
                                                             list_primary_gas_products = IEATools::primary_gas_products,
                                                             product.group = "Product.Group",
                                                             total_product_use = "Total_Product_Use",
                                                             total_group_use = "Total_Group_Use",
                                                             non_energy_uses = "Non_Energy_Uses",
                                                             share = "Share",
                                                             energy.stage = "Energy.stage",
                                                             country = IEATools::iea_cols$country,
                                                             method = IEATools::iea_cols$method,
                                                             energy_type = IEATools::iea_cols$energy_type,
                                                             last_stage = IEATools::iea_cols$last_stage,
                                                             year = IEATools::iea_cols$year,
                                                             unit = IEATools::iea_cols$unit,
                                                             product = IEATools::iea_cols$product,
                                                             boolean_non_energy_uses = "Boolean_Non_Energy_Uses",
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
  
  
  if (! (isTRUE(include_non_energy_uses) | isFALSE(include_non_energy_uses))){
    stop("The include_non_energy_uses argument must be either TRUE or FALSE.")
  }
  
  
  ### Building up production by product, for each fossil fuel group ###
  # FIRST AND SECOND STEPS COULD BE MERGED WITH A TIDYR::EXPAND_GRID AND APPROPRIATE FILTER AFTER.
  # BUT IT WOULD DEMAND A PROPER TESTING STRATEGY...!
  
  # First by group
  supply_ff_by_product_by_ff_group <- calc_total_use_by_product(.tidy_iea_df,
                                                                include_non_energy_uses = include_non_energy_uses,
                                                                total_use_mats = primary_production_mats) %>%
    dplyr::filter(
      .data[[product_without_origin]] %in% c(list_primary_oil_products, list_primary_coal_products, list_primary_gas_products)
    ) %>%
    dplyr::filter(
      .data[[product_without_origin]] %in% c(list_primary_oil_products, list_primary_coal_products, list_primary_gas_products)
    ) %>%
    dplyr::mutate(
      "{product.group}" := dplyr::case_when(
        .data[[product_without_origin]] %in% c(list_primary_oil_products, list_primary_gas_products) ~ "Oil and gas products",
        .data[[product_without_origin]] %in% list_primary_coal_products ~ "Coal products"
      ),
      "{energy.stage}" := "Primary"
    )
  
  # Second for all fossil fuels together
  supply_ff_by_product_all_ffs <- calc_total_use_by_product(.tidy_iea_df,
                                                            include_non_energy_uses = include_non_energy_uses,
                                                            total_use_mats = primary_production_mats) %>%
    dplyr::filter(
      .data[[product_without_origin]] %in% c(list_primary_oil_products, list_primary_coal_products, list_primary_gas_products)
    ) %>%
    dplyr::mutate(
      "{product.group}" := "All fossil fuels",
      "{energy.stage}" := "Primary"
    )
  
  # Third, binding these
  supply_ff_by_product <- dplyr::bind_rows(
    supply_ff_by_product_by_ff_group,
    supply_ff_by_product_all_ffs
  )
  
  
  ### Last, calculating the shares ###
  
  share_primary_ff_supply_by_product_by_group <- calc_primary_products_supply_by_group(.tidy_iea_df,
                                                                                       include_non_energy_uses = include_non_energy_uses,
                                                                                       primary_production_mats = primary_production_mats) %>%
    # Ideally, this here should be merged back within calc_primary_products_supply_by_group() function, too.
    dplyr::bind_rows(
      calc_primary_ff_supply(.tidy_iea_df,
                             include_non_energy_uses = include_non_energy_uses,
                             primary_production_mats = primary_production_mats)
    ) %>% 
    dplyr::left_join(supply_ff_by_product, by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {unit}, {product.group}, {energy.stage})) %>%
    dplyr::mutate(
      "{share}" := .data[[total_product_use]] / .data[[total_group_use]],
      "{boolean_non_energy_uses}" := include_non_energy_uses,
      "{non_energy_uses}" := dplyr::case_when(
        .data[[boolean_non_energy_uses]] == TRUE ~ "Included",
        .data[[boolean_non_energy_uses]] == FALSE ~ "Excluded"
      )
    ) %>%
    dplyr::select(-.data[[boolean_non_energy_uses]])
  
  return(share_primary_ff_supply_by_product_by_group)
}



# Calculates shares of each product in all the fossil fuel consumption by group

#' Title
#'
#' @param .tidy_iea_df
#' @param include_non_energy_uses
#' @param final_use_mats
#' @param list_oil_products
#' @param list_coal_products
#' @param list_gas_products
#' @param product.group
#' @param total_product_use
#' @param total_group_use
#' @param non_energy_uses
#' @param share
#' @param country
#' @param method
#' @param energy_type
#' @param last_stage
#' @param year
#' @param unit
#' @param product
#' @param boolean_non_energy_uses
#' @param energy.stage
#'
#' @return
#' @export
#'
#' @examples
calc_share_ff_use_by_product_by_group <- function(.tidy_iea_df,
                                                  include_non_energy_uses = FALSE,
                                                  final_use_mats = c(IEATools::psut_cols$Y, IEATools::psut_cols$U_eiou),
                                                  list_oil_products = IEATools::oil_and_oil_products,
                                                  list_coal_products = IEATools::coal_and_coal_products,
                                                  list_gas_products = IEATools::primary_gas_products,
                                                  product.group = "Product.Group",
                                                  total_product_use = "Total_Product_Use",
                                                  total_group_use = "Total_Group_Use",
                                                  non_energy_uses = "Non_Energy_Uses",
                                                  share = "Share",
                                                  country = IEATools::iea_cols$country,
                                                  method = IEATools::iea_cols$method,
                                                  energy_type = IEATools::iea_cols$energy_type,
                                                  last_stage = IEATools::iea_cols$last_stage,
                                                  year = IEATools::iea_cols$year,
                                                  unit = IEATools::iea_cols$unit,
                                                  product = IEATools::iea_cols$product,
                                                  boolean_non_energy_uses = "Boolean_Non_Energy_Uses",
                                                  energy.stage = "Energy.stage",
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
  
  if (! (isTRUE(include_non_energy_uses) | isFALSE(include_non_energy_uses))){
    stop("The include_non_energy_uses argument must be either TRUE or FALSE.")
  }
  
  ### Calculate the total use of each fossil fuel product, and binds its product groups in each observation. ###
  use_ff_by_product <- calc_total_use_by_product(.tidy_iea_df,
                                                 include_non_energy_uses = include_non_energy_uses,
                                                 total_use_mats = final_use_mats) %>%
    # First, oil and gas products, and coal products:
    dplyr::mutate(
      "{product.group}" := dplyr::case_when(
        .data[[product_without_origin]] %in% c(list_oil_products, list_gas_products) ~ "Oil and gas products",
        .data[[product_without_origin]] %in% list_coal_products ~ "Coal products"
      ),
      "{energy.stage}" := "Final (fuel)"
    ) %>%
    # Now, we bind the rows corresponding to duplicated information, but this time splitting Oil products and Natural gas:
    dplyr::bind_rows(
      calc_total_use_by_product(.tidy_iea_df,
                                include_non_energy_uses = include_non_energy_uses,
                                total_use_mats = final_use_mats) %>%
        dplyr::mutate(
          "{product.group}" := dplyr::case_when(
            .data[[product_without_origin]] %in% list_oil_products ~ "Oil products",
            .data[[product_without_origin]] %in% list_coal_products ~ "Coal products",
            .data[[product_without_origin]] %in% list_gas_products ~ "Natural gas"
          ),
          "{energy.stage}" := "Final (fuel)"
        ) %>%
        dplyr::filter(.data[[product.group]] != "Coal products")
    ) %>% 
    # Third, same information, but this time for "All fossil fuels"
    dplyr::bind_rows(
      calc_total_use_by_product(.tidy_iea_df,
                                include_non_energy_uses = include_non_energy_uses,
                                total_use_mats = final_use_mats) %>%
        dplyr::mutate(
          "{product.group}" := "All fossil fuels",
          "{energy.stage}" := "Final (fuel)"
        )
    )
  
  ### Now, calculates the share of each product contribution to total use of fossil fuels of each product group ###
  # Ideally, relocate calc_ff_use() within calc_all_products_use_by_group() function
  share_ff_use_by_product_by_group <- calc_all_products_use_by_group(.tidy_iea_df,
                                                                     include_non_energy_uses = include_non_energy_uses,
                                                                     final_use_mats = final_use_mats) %>%
    dplyr::bind_rows(
      share_ff_use_by_product <- calc_ff_use(.tidy_iea_df,
                                             include_non_energy_uses = include_non_energy_uses,
                                             final_use_mats = final_use_mats)
    ) %>% 
    dplyr::left_join(use_ff_by_product, by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {unit}, {product.group}, {energy.stage})) %>%
    dplyr::mutate(
      "{share}" := .data[[total_product_use]] / .data[[total_group_use]],
      "{boolean_non_energy_uses}" := include_non_energy_uses,
      "{non_energy_uses}" := dplyr::case_when(
        .data[[boolean_non_energy_uses]] == TRUE ~ "Included",
        .data[[boolean_non_energy_uses]] == FALSE ~ "Excluded"
      )
    ) %>%
    dplyr::select(-.data[[boolean_non_energy_uses]])
  
  return(share_ff_use_by_product_by_group)
}


#' Calculates the share of electricity supplied by each fossil fuel within each fossil fuel group
#' 
#' This function calculates the share of electricity supplied by each fossil fuel within each fossil fuel group,
#' for the two following fossil fuel groups "Oil and gas products", and "All fossil fuels". So it gives the share supplied
#' by respectively Coal products, Oil products, and Natural gas, in each of those groups. The shares then add up to unity necessarily.
#' 
#' The plan is then to update this function so it also does the same for the "Coal products", "Oil products", and "Natural gas".
#' This would be trivial (respectively 1 for Coal products, Oil products, and Natural gas), but it would be very convenient.
#'
#' @param .tidy_iea_df
#' @param supply_mats_list
#' @param matnames
#' @param product.group
#' @param share
#' @param energy.stage
#' @param country
#' @param method
#' @param energy_type
#' @param last_stage
#' @param year
#' @param unit
#' @param product
#' @param e.dot
#'
#' @return
#' @export
#'
#' @examples
calc_shares_elec_by_ff_group <- function(.tidy_iea_df,
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
  
  # Shares for the "All fossil fuels" group
  shares_electricity_by_ff <- .tidy_iea_df %>%
    dplyr::filter(matnames %in% supply_mats_list) %>%
    dplyr::filter(.data[[product_without_origin]] %in% c("Electricity [from Coal products]", "Electricity [from Oil products]", "Electricity [from Natural gas]")) %>%
    dplyr::select(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]],
                  .data[[unit]], .data[[product]], .data[[e.dot]]) %>%
    dplyr::mutate(
      "{product.group}" := "All fossil fuels",
      "{energy.stage}" := "Final (electricity)"
    ) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[unit]],
                    .data[[product.group]], .data[[energy.stage]], .data[[product]]) %>%
    dplyr::summarise(
      "{e.dot}" := sum(.data[[e.dot]])
    ) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[unit]],
                    .data[[product.group]], .data[[energy.stage]]) %>%
    dplyr::mutate(
      "{share}" := .data[[e.dot]] / sum(.data[[e.dot]])
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data[[unit]], -.data[[e.dot]])
  
  # Shares for the "Oil and gas products" group
  shares_electricity_by_ff_group <- .tidy_iea_df %>%
    dplyr::filter(matnames %in% supply_mats_list) %>%
    dplyr::filter(.data[[product_without_origin]] %in% c("Electricity [from Coal products]", "Electricity [from Oil products]", "Electricity [from Natural gas]")) %>%
    dplyr::select(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]],
                  .data[[unit]], .data[[product]], .data[[e.dot]]) %>%
    dplyr::mutate(
      "{product.group}" := dplyr::case_when(
        stringr::str_detect(.data[[product]], "Electricity \\[from Coal products\\]") ~ "Coal products",
        stringr::str_detect(.data[[product]], "Electricity \\[from Oil products\\]") ~ "Oil and gas products",
        stringr::str_detect(.data[[product]], "Electricity \\[from Natural gas\\]") ~ "Oil and gas products",
      ),
      "{energy.stage}" := "Final (electricity)"
    ) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[unit]],
                    .data[[product.group]], .data[[energy.stage]], .data[[product]]) %>%
    dplyr::summarise(
      "{e.dot}" := sum(.data[[e.dot]])
    ) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[unit]],
                    .data[[product.group]], .data[[energy.stage]]) %>%
    dplyr::mutate(
      "{share}" := .data[[e.dot]] / sum(.data[[e.dot]])
    ) %>%
    dplyr::filter(.data[[product.group]] != "Coal products") %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data[[unit]], -.data[[e.dot]])
  
  # Shares for the "Coal products", "Oil products", and "Natural gas" groups
  shares_electricity_by_ff_group_2 <- .tidy_iea_df %>%
    dplyr::filter(matnames %in% supply_mats_list) %>%
    dplyr::filter(.data[[product_without_origin]] %in% c("Electricity [from Coal products]", "Electricity [from Oil products]", "Electricity [from Natural gas]")) %>%
    dplyr::select(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product]]) %>%
    dplyr::mutate(
      "{product.group}" := stringr::str_extract(.data[[product]], "\\[.*\\]") %>%
        stringr::str_remove("\\[from ") %>%
        stringr::str_remove("\\]"),
      "{energy.stage}" := "Final (electricity)",
      "{share}" := 1,
      "{country}" := dplyr::case_when(
        stringr::str_detect(.data[[product]], "\\{.*\\}_") ~ stringr::str_extract(.data[[product]], "\\{.*\\}") %>%
          stringr::str_remove("\\{") %>% stringr::str_remove("\\}"),
        TRUE ~ .data[[country]]
      )
    ) %>%
    dplyr::distinct()
  
  
  shares_to_return <- dplyr::bind_rows(
    shares_electricity_by_ff,
    shares_electricity_by_ff_group,
    shares_electricity_by_ff_group_2
  )
  
  return(shares_to_return)
}


#' Calculates the share of heat supplied by each fossil fuel within each fossil fuel group
#' 
#' This function calculates the share of heat supplied by each fossil fuel within each fossil fuel group,
#' for the two following fossil fuel groups "Oil and gas products", and "All fossil fuels". So it gives the share supplied
#' by respectively Coal products, Oil products, and Natural gas, in each of those groups. The shares then add up to unity necessarily.
#' 
#' The plan is then to update this function so it also does the same for the "Coal products", "Oil products", and "Natural gas".
#' This would be trivial (respectively 1 for Coal products, Oil products, and Natural gas), but it would be very convenient.
#' 
#' @param .tidy_iea_df
#' @param supply_mats_list
#' @param matnames
#' @param product.group
#' @param share
#' @param energy.stage
#' @param country
#' @param method
#' @param energy_type
#' @param last_stage
#' @param year
#' @param unit
#' @param product
#' @param e.dot
#'
#' @return
#' @export
#'
#' @examples
calc_shares_heat_by_ff_group <- function(.tidy_iea_df,
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
  
  # Shares for the "All fossil fuels" group
  shares_heat_by_ff <- .tidy_iea_df %>%
    dplyr::filter(matnames %in% supply_mats_list) %>%
    dplyr::filter(.data[[product_without_origin]] %in% c("Heat [from Coal products]", "Heat [from Oil products]", "Heat [from Natural gas]")) %>%
    dplyr::select(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]],
                  .data[[unit]], .data[[product]], .data[[e.dot]]) %>%
    dplyr::mutate(
      "{product.group}" := "All fossil fuels",
      "{energy.stage}" := "Final (heat)"
    ) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[unit]],
                    .data[[product.group]], .data[[energy.stage]], .data[[product]]) %>%
    dplyr::summarise(
      "{e.dot}" := sum(.data[[e.dot]])
    ) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[unit]],
                    .data[[product.group]], .data[[energy.stage]]) %>%
    dplyr::mutate(
      "{share}" := .data[[e.dot]] / sum(.data[[e.dot]])
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data[[unit]], -.data[[e.dot]])
  
  # Shares for the "Oil and gas products" group
  shares_heat_by_ff_group <- .tidy_iea_df %>%
    dplyr::filter(matnames %in% supply_mats_list) %>%
    dplyr::filter(.data[[product_without_origin]] %in% c("Heat [from Coal products]", "Heat [from Oil products]", "Heat [from Natural gas]")) %>%
    dplyr::select(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]],
                  .data[[unit]], .data[[product]], .data[[e.dot]]) %>%
    dplyr::mutate(
      "{product.group}" := dplyr::case_when(
        stringr::str_detect(.data[[product]], "Heat \\[from Coal products\\]") ~ "Coal products",
        stringr::str_detect(.data[[product]], "Heat \\[from Oil products\\]") ~ "Oil and gas products",
        stringr::str_detect(.data[[product]], "Heat \\[from Natural gas\\]") ~ "Oil and gas products",
      ),
      "{energy.stage}" := "Final (heat)"
    ) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[unit]],
                    .data[[product.group]], .data[[energy.stage]], .data[[product]]) %>%
    dplyr::summarise(
      "{e.dot}" := sum(.data[[e.dot]])
    ) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[unit]],
                    .data[[product.group]], .data[[energy.stage]]) %>%
    dplyr::mutate(
      "{share}" := .data[[e.dot]] / sum(.data[[e.dot]])
    ) %>%
    dplyr::filter(.data[[product.group]] != "Coal products") %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data[[unit]], -.data[[e.dot]])
  
  # Shares for the "Coal products", "Oil products", and "Natural gas" groups
  shares_heat_by_ff_group_2 <- .tidy_iea_df %>%
    dplyr::filter(matnames %in% supply_mats_list) %>%
    dplyr::filter(.data[[product_without_origin]] %in% c("Heat [from Coal products]", "Heat [from Oil products]", "Heat [from Natural gas]")) %>%
    dplyr::select(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product]]) %>%
    dplyr::mutate(
      "{product.group}" := stringr::str_extract(.data[[product]], "\\[.*\\]") %>%
        stringr::str_remove("\\[from ") %>%
        stringr::str_remove("\\]"),
      "{energy.stage}" := "Final (heat)",
      "{share}" := 1,
      "{country}" := dplyr::case_when(
        stringr::str_detect(.data[[product]], "\\{.*\\}_") ~ stringr::str_extract(.data[[product]], "\\{.*\\}") %>%
          stringr::str_remove("\\{") %>% stringr::str_remove("\\}"),
        TRUE ~ .data[[country]]
      )
    ) %>%
    dplyr::distinct()
  
  # Binding shares before returning
  shares_to_return <- dplyr::bind_rows(
    shares_heat_by_ff,
    shares_heat_by_ff_group,
    shares_heat_by_ff_group_2
  )
  
  return(shares_to_return)
}


#' Title
#'
#' @param .tidy_iea_df
#' @param include_non_energy_uses
#' @param final_use_mats
#' @param list_oil_products
#' @param list_coal_products
#' @param list_gas_products
#' @param product.group
#' @param total_product_use
#' @param total_group_use
#' @param non_energy_uses
#' @param share
#' @param country
#' @param method
#' @param energy_type
#' @param last_stage
#' @param year
#' @param unit
#' @param e_dot
#' @param product
#' @param boolean_non_energy_uses
#' @param energy.stage
#'
#' @return
#' @export
#'
#' @examples
calc_shares_ff_by_group_inc_elec_heat <- function(.tidy_iea_df,
                                                  include_non_energy_uses = FALSE,
                                                  final_use_mats = c(IEATools::psut_cols$Y, IEATools::psut_cols$U_eiou),
                                                  list_oil_products = IEATools::oil_and_oil_products,
                                                  list_coal_products = IEATools::coal_and_coal_products,
                                                  list_gas_products = IEATools::primary_gas_products,
                                                  product.group = "Product.Group",
                                                  total_product_use = "Total_Product_Use",
                                                  total_group_use = "Total_Group_Use",
                                                  non_energy_uses = "Non_Energy_Uses",
                                                  share = "Share",
                                                  country = IEATools::iea_cols$country,
                                                  method = IEATools::iea_cols$method,
                                                  energy_type = IEATools::iea_cols$energy_type,
                                                  last_stage = IEATools::iea_cols$last_stage,
                                                  year = IEATools::iea_cols$year,
                                                  unit = IEATools::iea_cols$unit,
                                                  e_dot = IEATools::iea_cols$e_dot,
                                                  product = IEATools::iea_cols$product,
                                                  boolean_non_energy_uses = "Boolean_Non_Energy_Uses",
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
  
  
  if (! (isTRUE(include_non_energy_uses) | isFALSE(include_non_energy_uses))){
    stop("The include_non_energy_uses argument must be either TRUE or FALSE.")
  }
  
  # For all fossil fuels together
  use_ff_by_product_1 <- calc_total_use_by_product(.tidy_iea_df,
                                                   include_non_energy_uses = include_non_energy_uses,
                                                   total_use_mats = final_use_mats) %>%
    dplyr::mutate(
      "{product.group}" := "All fossil fuels"
    )
  
  # For "Oil and gas product", and "Coal products" groups
  use_ff_by_product_2 <- calc_total_use_by_product(.tidy_iea_df,
                                                   include_non_energy_uses = include_non_energy_uses,
                                                   total_use_mats = final_use_mats) %>%
    dplyr::mutate(
      "{product.group}" := dplyr::case_when(
        .data[[product_without_origin]] %in% c(list_oil_products, list_gas_products) ~ "Oil and gas products",
        .data[[product_without_origin]] %in% list_coal_products ~ "Coal products"
      )
    )
  
  # For "Oil products" and "Natural gas" groups
  use_ff_by_product_3 <- calc_total_use_by_product(.tidy_iea_df,
                                                   include_non_energy_uses = include_non_energy_uses,
                                                   total_use_mats = final_use_mats) %>%
    dplyr::mutate(
      "{product.group}" := dplyr::case_when(
        .data[[product_without_origin]] %in% list_oil_products ~ "Oil products",
        .data[[product_without_origin]] %in% list_gas_products ~ "Natural gas",
        .data[[product_without_origin]] %in% list_coal_products ~ "Coal products"
      )
    ) %>%
    dplyr::filter(.data[[product.group]] != "Coal products")
  
  # Figure out how much "Electricity [from ...]" is used by each fossil fuel group:
  share_elec_from_ff_by_ff_group_1 <- calc_share_elec_supply_by_ff_group(.tidy_iea_df) %>%
    dplyr::filter(.data[[product.group]] %in% c("Coal products", "Oil products", "Natural gas")) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[unit]], .data[[energy.stage]]) %>%
    dplyr::mutate(
      "{share}" := .data[[share]] / sum(.data[[share]])
    ) %>%
    dplyr::rename(
      Origin.Product.Group = Product.Group
    )
  
  share_elec_from_ff_by_ff_group_2 <- calc_share_elec_supply_by_ff_group(.tidy_iea_df) %>%
    dplyr::filter(.data[[product.group]] %in% c("Oil products", "Natural gas")) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[unit]], .data[[energy.stage]]) %>%
    dplyr::mutate(
      "{share}" := .data[[share]] / sum(.data[[share]])
    ) %>%
    dplyr::rename(
      Origin.Product.Group = Product.Group
    )
  
  use_elec_by_ff_group_1 <- calc_fec_from_ff_as_elec_by_group(.tidy_iea_df) %>%
    dplyr::filter(.data[[product.group]] == "All fossil fuels") %>%
    dplyr::left_join(share_elec_from_ff_by_ff_group_1,
                     by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {unit}, {energy.stage})) %>%
    dplyr::mutate(
      "{e_dot}" := .data[[e_dot]] * .data[[share]],
      "{product}" := stringr::str_c(.data[[product]], " [from ", .data[["Origin.Product.Group"]], "]")
    ) %>%
    dplyr::select(-.data[[share]], -.data[["Origin.Product.Group"]]) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product]], .data[[product.group]]) %>%
    dplyr::summarise(
      Total_Product_Use = sum(.data[[e_dot]])
    ) %>%
    dplyr::mutate(
      "{unit}" := "ktoe"
    )
  
  use_elec_by_ff_group_2 <- calc_fec_from_ff_as_elec_by_group(.tidy_iea_df) %>%
    dplyr::filter(.data[[product.group]] == "Oil and gas products") %>%
    dplyr::left_join(share_elec_from_ff_by_ff_group_2,
                     by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {unit}, {energy.stage})) %>%
    dplyr::mutate(
      "{e_dot}" := .data[[e_dot]] * .data[[share]],
      "{product}" := stringr::str_c(.data[[product]], " [from ", .data[["Origin.Product.Group"]], "]")
    ) %>%
    dplyr::select(-.data[[share]], -.data[["Origin.Product.Group"]]) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product]], .data[[product.group]]) %>%
    dplyr::summarise(
      Total_Product_Use = sum(.data[[e_dot]])
    ) %>%
    dplyr::mutate(
      "{unit}" := "ktoe"
    )
  
  use_elec_by_ff_group_3 <- calc_fec_from_ff_as_elec_by_group(.tidy_iea_df) %>%
    dplyr::filter(.data[[product.group]] %in% c("Oil products", "Coal products", "Natural gas")) %>%
    dplyr::mutate(
      "{product}" := stringr::str_c(.data[[product]], " [from ", .data[[product.group]], "]")
    ) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product]], .data[[product.group]]) %>%
    dplyr::summarise(
      Total_Product_Use = sum(.data[[e_dot]])
    ) %>%
    dplyr::mutate(
      "{unit}" := "ktoe"
    )
  
  # Binding rows
  use_elec_by_ff_group <- dplyr::bind_rows(use_elec_by_ff_group_1, use_elec_by_ff_group_2, use_elec_by_ff_group_3)
  
  
  # Figure out how much "Heat [from ...]" is used by each fossil fuel group:
  share_heat_from_ff_by_ff_group_1 <- calc_share_heat_supply_by_ff_group(.tidy_iea_df) %>%
    dplyr::filter(.data[[product.group]] %in% c("Coal products", "Oil products", "Natural gas")) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[unit]], .data[[energy.stage]]) %>%
    dplyr::mutate(
      "{share}" := .data[[share]] / sum(.data[[share]])
    ) %>%
    dplyr::rename(
      Origin.Product.Group = Product.Group
    )
  
  share_heat_from_ff_by_ff_group_2 <- calc_share_heat_supply_by_ff_group(.tidy_iea_df) %>%
    dplyr::filter(.data[[product.group]] %in% c("Oil products", "Natural gas")) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[unit]], .data[[energy.stage]]) %>%
    dplyr::mutate(
      "{share}" := .data[[share]] / sum(.data[[share]])
    ) %>%
    dplyr::rename(
      Origin.Product.Group = Product.Group
    )
  
  use_heat_by_ff_group_1 <- calc_fec_from_ff_as_heat_by_group(.tidy_iea_df) %>%
    dplyr::filter(.data[[product.group]] == "All fossil fuels") %>%
    dplyr::left_join(share_heat_from_ff_by_ff_group_1,
                     by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {unit}, {energy.stage})) %>%
    dplyr::mutate(
      "{e_dot}" := .data[[e_dot]] * .data[[share]],
      "{product}" := stringr::str_c(.data[[product]], " [from ", .data[["Origin.Product.Group"]], "]")
    ) %>%
    dplyr::select(-.data[[share]], -.data[["Origin.Product.Group"]]) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product]], .data[[product.group]]) %>%
    dplyr::summarise(
      Total_Product_Use = sum(.data[[e_dot]])
    ) %>%
    dplyr::mutate(
      "{unit}" := "ktoe"
    )
  
  use_heat_by_ff_group_2 <- calc_fec_from_ff_as_heat_by_group(.tidy_iea_df) %>%
    dplyr::filter(.data[[product.group]] == "Oil and gas products") %>%
    dplyr::left_join(share_heat_from_ff_by_ff_group_2,
                     by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {unit}, {energy.stage})) %>%
    dplyr::mutate(
      "{e_dot}" := .data[[e_dot]] * .data[[share]],
      "{product}" := stringr::str_c(.data[[product]], " [from ", .data[["Origin.Product.Group"]], "]")
    ) %>%
    dplyr::select(-.data[[share]], -.data[["Origin.Product.Group"]]) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product]], .data[[product.group]]) %>%
    dplyr::summarise(
      Total_Product_Use = sum(.data[[e_dot]])
    ) %>%
    dplyr::mutate(
      "{unit}" := "ktoe"
    )
  
  use_heat_by_ff_group_3 <- calc_fec_from_ff_as_heat_by_group(.tidy_iea_df) %>%
    dplyr::filter(.data[[product.group]] %in% c("Oil products", "Coal products", "Natural gas")) %>%
    dplyr::mutate(
      "{product}" := stringr::str_c(.data[[product]], " [from ", .data[[product.group]], "]")
    ) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product]], .data[[product.group]]) %>%
    dplyr::summarise(
      Total_Product_Use = sum(.data[[e_dot]])
    ) %>%
    dplyr::mutate(
      "{unit}" := "ktoe"
    )
  
  # Binding rows
  use_heat_by_ff_group <- dplyr::bind_rows(use_heat_by_ff_group_1, use_heat_by_ff_group_2, use_heat_by_ff_group_3)
  
  # Now, gather everything:
  use_ff_by_product_inc_elec_heat <- dplyr::bind_rows(
    use_ff_by_product_1,
    use_ff_by_product_2,
    use_ff_by_product_3,
    use_elec_by_ff_group,
    use_heat_by_ff_group
  )
  
  
  # And summarise/calculates shares of everything:
  share_ff_use_by_product <- use_ff_by_product_inc_elec_heat %>%
    dplyr::mutate(
      "{energy.stage}" := "Final (fuel+elec+heat)"
    ) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product.group]], .data[[energy.stage]]) %>%
    dplyr::mutate(
      Total_Group_Use = sum(Total_Product_Use)
    ) %>%
    dplyr::mutate(
      "{share}" := .data[["Total_Product_Use"]] / .data[["Total_Group_Use"]],
      "{boolean_non_energy_uses}" := include_non_energy_uses,
      "{non_energy_uses}" := dplyr::case_when(
        .data[[boolean_non_energy_uses]] == TRUE ~ "Included",
        .data[[boolean_non_energy_uses]] == FALSE ~ "Excluded"
      )
    ) %>%
    dplyr::select(-.data[[boolean_non_energy_uses]])
  
  # Returning values:
  return(share_ff_use_by_product)
}
