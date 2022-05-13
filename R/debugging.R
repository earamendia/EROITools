

# aggregated_useful_erois <- tidy_shares_df %>%
#   dplyr::inner_join(.tidy_erois_df %>% 
#                       dplyr::select(-.data[[country]]), by = c({method}, {energy_type}, {last_stage}, {year}, {product})) %>%
#   print()
#   
#   
#   dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]],
#                   .data[[eroi.method]], .data[[type]], .data[[boundary]], .data[[non_energy_uses]], .data[[product.group]], .data[[energy.stage]]) %>%
#   dplyr::summarise(
#     Group.eroi.inversed = sum(.data[[share]] * (1/.data[[useful_stage_eroi]])) / sum(.data[[share]])
#   ) %>% 
#   dplyr::mutate(
#     "{group.eroi}" := 1 / Group.eroi.inversed
#   ) %>% 
#   dplyr::select(-Group.eroi.inversed)