
# average_FU_efficiencies <- tidy_shares_df %>% 
#   dplyr::ungroup() %>% 
#   # Those joins that fail are primary energy products
#   # which are systematically ascribed to non-energy uses in the PFU database.
#   # very careful here is this changes in the future in the PFU database....!
#   dplyr::inner_join(.tidy_efficiencies_df %>% 
#                       dplyr::rename("{product_without_origin}" := .data[[product]]), 
#                     by = c({country}, {method}, {energy_type}, {year}, {product_without_origin})) %>% 
#   dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[energy.stage]], .data[[year]], .data[[product.group]]) %>% 
#   dplyr::summarise(
#   "{aggregated_efficiency}" := sum(.data[[share]] * .data[[average_efficiency]]) / sum(.data[[share]])
#   )
# 
# 
# 
# print(tidy_shares_df)
# 
# print(.tidy_efficiencies_df)
