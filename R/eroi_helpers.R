
# Calculates shares of each product in the total primary fossil fuel consumption, so only one group "All fossil fuels"
# And using the V matrix, so we're talking here of SUPPLY shares.
# calc_share_primary_ff_use_by_product <- function(.tidy_iea_df,
#                                                  include_non_energy_uses = FALSE,
#                                                  primary_production_mats = c(IEATools::psut_cols$V),
#                                                  list_primary_oil_products = IEATools::primary_oil_products,
#                                                  list_primary_coal_products = IEATools::primary_coal_products,
#                                                  list_primary_gas_products = IEATools::primary_gas_products,
#                                                  product.group = "Product.Group",
#                                                  total_product_use = "Total_Product_Use",
#                                                  total_group_use = "Total_Group_Use",
#                                                  non_energy_uses = "Non_Energy_Uses",
#                                                  share = "Share",
#                                                  country = IEATools::iea_cols$country,
#                                                  method = IEATools::iea_cols$method,
#                                                  energy_type = IEATools::iea_cols$energy_type,
#                                                  last_stage = IEATools::iea_cols$last_stage,
#                                                  year = IEATools::iea_cols$year,
#                                                  unit = IEATools::iea_cols$unit,
#                                                  product = IEATools::iea_cols$product,
#                                                  boolean_non_energy_uses = "Boolean_Non_Energy_Uses",
#                                                  energy.stage = "Energy.stage",
#                                                  product_without_origin = "product_without_origin"
# ){
#   
#   ### Preparing the .tidy_iea_df so that it has a new "product_without_origin" column,
#   # which will be equal to "product" when we are not using a MR-PSUT framework
#   cols_to_check <- c(product_without_origin = NA_character_)
#   
#   .tidy_iea_df <- .tidy_iea_df %>%
#     tibble::add_column(!!!cols_to_check[!names(cols_to_check) %in% names(.)]) %>%
#     dplyr::mutate(
#       "{product_without_origin}" := dplyr::case_when(
#         is.na(.data[[product_without_origin]]) ~ .data[[product]],
#         TRUE ~ .data[[product_without_origin]]
#       )
#     )
#   
#   if (! (isTRUE(include_non_energy_uses) | isFALSE(include_non_energy_uses))){
#     stop("The include_non_energy_uses argument must be either TRUE or FALSE.")
#   }
#   
#   use_ff_by_product <- calc_total_use_by_product(.tidy_iea_df,
#                                                  include_non_energy_uses = include_non_energy_uses,
#                                                  total_use_mats = primary_production_mats) %>%
#     dplyr::filter(
#       .data[[product_without_origin]] %in% c(list_primary_oil_products, list_primary_coal_products, list_primary_gas_products)
#     ) %>%
#     dplyr::mutate(
#       "{product.group}" := "All fossil fuels",
#       "{energy.stage}" := "Primary"
#     )
#   
#   share_primary_ff_use_by_product <- calc_primary_ff_supply(.tidy_iea_df,
#                                                          include_non_energy_uses = include_non_energy_uses,
#                                                          primary_production_mats = primary_production_mats) %>%
#     dplyr::left_join(use_ff_by_product, by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {unit}, {product.group}, {energy.stage})) %>%
#     dplyr::mutate(
#       "{share}" := .data[[total_product_use]] / .data[[total_group_use]],
#       "{boolean_non_energy_uses}" := include_non_energy_uses,
#       "{non_energy_uses}" := dplyr::case_when(
#         .data[[boolean_non_energy_uses]] == TRUE ~ "Included",
#         .data[[boolean_non_energy_uses]] == FALSE ~ "Excluded"
#       )
#     ) %>%
#     dplyr::select(-.data[[boolean_non_energy_uses]])
#   
#   return(share_primary_ff_use_by_product)
# }



# Calculates shares of each product in the total fossil fuel consumption
# calc_share_ff_use_by_product <- function(.tidy_iea_df,
#                                          include_non_energy_uses = FALSE,
#                                          final_use_mats = c(IEATools::psut_cols$Y, IEATools::psut_cols$U_eiou),
#                                          list_oil_products = IEATools::oil_and_oil_products,
#                                          list_coal_products = IEATools::coal_and_coal_products,
#                                          list_gas_products = IEATools::primary_gas_products,
#                                          product.group = "Product.Group",
#                                          total_product_use = "Total_Product_Use",
#                                          total_group_use = "Total_Group_Use",
#                                          non_energy_uses = "Non_Energy_Uses",
#                                          share = "Share",
#                                          country = IEATools::iea_cols$country,
#                                          method = IEATools::iea_cols$method,
#                                          energy_type = IEATools::iea_cols$energy_type,
#                                          last_stage = IEATools::iea_cols$last_stage,
#                                          year = IEATools::iea_cols$year,
#                                          unit = IEATools::iea_cols$unit,
#                                          product = IEATools::iea_cols$product,
#                                          boolean_non_energy_uses = "Boolean_Non_Energy_Uses",
#                                          energy.stage = "Energy.stage",
#                                          product_without_origin = "product_without_origin"
# ){
#   
#   ### Preparing the .tidy_iea_df so that it has a new "product_without_origin" column,
#   # which will be equal to "product" when we are not using a MR-PSUT framework
#   cols_to_check <- c(product_without_origin = NA_character_)
#   
#   .tidy_iea_df <- .tidy_iea_df %>%
#     tibble::add_column(!!!cols_to_check[!names(cols_to_check) %in% names(.)]) %>%
#     dplyr::mutate(
#       "{product_without_origin}" := dplyr::case_when(
#         is.na(.data[[product_without_origin]]) ~ .data[[product]],
#         TRUE ~ .data[[product_without_origin]]
#       )
#     )
#   
#   
#   if (! (isTRUE(include_non_energy_uses) | isFALSE(include_non_energy_uses))){
#     stop("The include_non_energy_uses argument must be either TRUE or FALSE.")
#   }
#   
#   use_ff_by_product <- calc_total_use_by_product(.tidy_iea_df,
#                                                  include_non_energy_uses = include_non_energy_uses,
#                                                  total_use_mats = final_use_mats) %>%
#     dplyr::mutate(
#       "{product.group}" := "All fossil fuels",
#       "{energy.stage}" := "Final (fuel)"
#     )
#   
#   share_ff_use_by_product <- calc_ff_use(.tidy_iea_df,
#                                          include_non_energy_uses = include_non_energy_uses,
#                                          final_use_mats = final_use_mats) %>%
#     dplyr::left_join(use_ff_by_product, by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {unit}, {product.group}, {energy.stage})) %>%
#     dplyr::mutate(
#       "{share}" := .data[[total_product_use]] / .data[[total_group_use]],
#       "{boolean_non_energy_uses}" := include_non_energy_uses,
#       "{non_energy_uses}" := dplyr::case_when(
#         .data[[boolean_non_energy_uses]] == TRUE ~ "Included",
#         .data[[boolean_non_energy_uses]] == FALSE ~ "Excluded"
#       )
#     ) %>%
#     dplyr::select(-.data[[boolean_non_energy_uses]])
#   
#   return(share_ff_use_by_product)
# }


