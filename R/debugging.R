# 
# # Working out primary energy supply, and final energy consumption by fossil fuel group (including energy / heat coming grom fossil fuels)
# total_output_per_group <- dplyr::bind_rows(
#   calc_primary_products_supply_by_group(.tidy_iea_df,
#                                         total_group_supply = total_group_output),
#   calc_fec_from_ff_by_group(.tidy_iea_df,
#                             include_non_energy_uses = include_non_energy_uses) %>%
#     dplyr::rename(
#       "{total_group_output}" := .data[[e_dot]]
#     )
# ) %>%
#   print()
# 
# # Now calculating the ratio of indirect energy by output, both at the primary and final stage, for each group
# indirect_energy_per_output_primary_final <- .tidy_indirect_energy %>%
#   dplyr::inner_join(
#     total_output_per_group,
#     by = c({country}, {year}, {product.group}, {energy.stage})
#   ) %>%
#   dplyr::mutate(
#     ratio_indirect_energy_per_output = .data[[indirect_energy_ktoe]] / .data[[total_group_output]]
#   ) %>%
#   dplyr::select(-.data[[indirect_energy_ktoe]], -.data[[method]], -.data[[energy_type]],
#                 -.data[[last_stage]], -.data[[unit]], -.data[[total_group_output]]) %>%
#   print()
# 
# 
# # Calculating the ratio of indirect energy by output, now at the final stage, for each group
# # This is needed because w^e need the ratio for each of Final (fuel), Final (elec), Final (heat), and Final (fuel+elec+heat)
# indirect_energy_per_output_useful <- .tidy_summarised_erois_df %>%
#   dplyr::filter(! .data[[energy.stage]] == "Primary") %>%
#   dplyr::filter(.data[[type]] == "Gross") %>%
#   dplyr::filter(.data[[boundary]] == "Feedstock") %>%
#   dplyr::mutate(
#     "{product.group}" := stringr::str_c(.data[[product.group]], stringr::str_extract(.data[[energy.stage]], " \\(.*\\)")),
#     "{energy.stage}" := stringr::str_remove(.data[[energy.stage]], " \\(.*\\)")
#   ) %>%
#   tidyr::pivot_wider(names_from = .data[[energy.stage]], values_from = Group.eroi) %>%
#   dplyr::mutate(
#     Final_to_useful_efficiency = Useful / Final
#   ) %>%dplyr::select(-Final, -Useful) %>%
#   dplyr::mutate(
#     "{energy.stage}" := stringr::str_c("Useful", stringr::str_extract(Product.Group, " \\(.*\\)")),
#     "{product.group}" := stringr::str_remove(.data[[product.group]], " \\(.*\\)")
#   ) %>%
#   dplyr::inner_join(
#     indirect_energy_per_output_primary_final %>% dplyr::filter(stringr::str_detect(.data[[energy.stage]], "Final")) %>% dplyr::select(-.data[[energy.stage]]),
#     by = c({country}, {year}, {product.group})
#   ) %>%
#   dplyr::mutate(
#     ratio_indirect_energy_per_output = ratio_indirect_energy_per_output / Final_to_useful_efficiency
#   ) %>%
#   dplyr::ungroup() %>%
#   dplyr::select(-.data[[method]], -.data[[energy_type]], -.data[[last_stage]], -Eroi.method, -.data[[type]], -.data[[boundary]], -Non_Energy_Uses, -Final_to_useful_efficiency)
# 
# 
# # Expand, so that the final stage is now available for final (fuel), final (elec), and final (heat)
# # With the same values.
# indirect_energy_per_output_expanded <- indirect_energy_per_output_primary_final %>%
#   dplyr::filter(.data[[energy.stage]] == "Primary") %>%
#   dplyr::bind_rows(
#     indirect_energy_per_output_primary_final %>%
#       dplyr::filter(.data[[energy.stage]] == "Final") %>%
#       dplyr::select(-.data[[energy.stage]]) %>%
#       tidyr::expand_grid(Energy.stage = c("Final (fuel)", "Final (electricity)", "Final (heat)", "Final (fuel+elec+heat)")),
#     indirect_energy_per_output_useful
#   )
# 
# # Now, adding indirect energy to EROIs:
# tidy_summarised_erois_with_idE <- .tidy_summarised_erois_df %>%
#   tidyr::expand_grid(
#     Indirect_Energy = c("Included", "Excluded")
#   ) %>%
#   dplyr::left_join(
#     indirect_energy_per_output_expanded,
#     by = c({country}, {year}, "Indirect_Energy", {product.group}, {energy.stage})
#   ) %>%
#   dplyr::mutate(
#     ratio_indirect_energy_per_output = tidyr::replace_na(.data[["ratio_indirect_energy_per_output"]], 0),
#     Group.eroi = 1/(1/Group.eroi + .data[["ratio_indirect_energy_per_output"]])
#   ) %>%
#   dplyr::select(-.data[["ratio_indirect_energy_per_output"]])

# just a test in the debugging file
