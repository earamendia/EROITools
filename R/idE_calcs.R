

add_indirect_energy_to_erois <- function(.tidy_summarised_erois_df,
                                         .tidy_indirect_energy,
                                         .tidy_iea_df,
                                         include_non_energy_uses = TRUE){
  
  # Working out primary energy supply, and final energy consumption by fossil fuel group (including energy / heat coming grom fossil fuels)
  total_output_per_group <- dplyr::bind_rows(
    calc_primary_products_supply_by_group(.tidy_iea_df),
    # calc_primary_ff_supply(.tidy_iea_df,
    #                        include_non_energy_uses = include_non_energy_uses),
    calc_fec_from_ff_by_group(.tidy_iea_df,
                              include_non_energy_uses = include_non_energy_uses) %>%
      dplyr::rename(Total_Group_Use = E.dot)
  )
  
  # Now calculating the ratio of indirect energy by output, both at the primary and final stage, for each group
  indirect_energy_per_output_primary_final <- .tidy_indirect_energy %>%
    dplyr::inner_join(
      total_output_per_group,
      by = c("Country", "Year", "Product.Group", "Energy.stage")
    ) %>%
    dplyr::mutate(
      ratio_indirect_energy_per_output = .data[["Indirect_Energy_ktoe"]] / .data[["Total_Group_Use"]]
    ) %>%
    dplyr::select(-.data[["Indirect_Energy_ktoe"]], -.data[["Method"]], -.data[["Energy.type"]],
                  -.data[["Last.stage"]], -.data[["Unit"]], -.data[["Total_Group_Use"]])
  
  
  # Calculating the ratio of indirect energy by output, now at the final stage, for each group
  indirect_energy_per_output_useful <- .tidy_summarised_erois_df %>%
    dplyr::filter(! Energy.stage == "Primary") %>%
    dplyr::filter(Type == "Gross") %>%
    dplyr::filter(Boundary == "Feedstock") %>%
    dplyr::mutate(
      Product.Group = stringr::str_c(Product.Group, stringr::str_extract(Energy.stage, " \\(.*\\)")),
      Energy.stage = stringr::str_remove(Energy.stage, " \\(.*\\)")
    ) %>%
    tidyr::pivot_wider(names_from = Energy.stage, values_from = Group.eroi) %>%
    dplyr::mutate(
      Final_to_useful_efficiency = Useful / Final
    ) %>%
    dplyr::select(-Final, -Useful) %>%
    dplyr::mutate(
      Energy.stage = stringr::str_c("Useful", stringr::str_extract(Product.Group, " \\(.*\\)")),
      Product.Group = stringr::str_remove(Product.Group, " \\(.*\\)")
    ) %>%
    dplyr::inner_join(
      indirect_energy_per_output_primary_final %>% dplyr::filter(stringr::str_detect(Energy.stage, "Final")) %>% dplyr::select(-Energy.stage),
      by = c("Country", "Year", "Product.Group")
    ) %>%
    dplyr::mutate(
      ratio_indirect_energy_per_output = ratio_indirect_energy_per_output / Final_to_useful_efficiency
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-Method, -Energy.type, -Last.stage, -Eroi.method, -Type, -Boundary, -Non_Energy_Uses, -Final_to_useful_efficiency)
  
  
  # Expand, so that the final stage is now available for final (fuel), final (elec), and final (heat)
  # With the same values.
  indirect_energy_per_output_expanded <- indirect_energy_per_output_primary_final %>%
    dplyr::filter(Energy.stage == "Primary") %>%
    dplyr::bind_rows(
      indirect_energy_per_output_primary_final %>%
        dplyr::filter(Energy.stage == "Final") %>%
        dplyr::select(-Energy.stage) %>%
        tidyr::expand_grid(Energy.stage = c("Final (fuel)", "Final (electricity)", "Final (heat)", "Final (fuel+elec+heat)")),
      indirect_energy_per_output_useful
    )
  
  # Now, adding indirect energy to EROIs:
  tidy_summarised_erois_with_idE <- .tidy_summarised_erois_df %>%
    tidyr::expand_grid(
      Indirect_Energy = c("Included", "Excluded")
    ) %>%
    dplyr::left_join(
      indirect_energy_per_output_expanded,
      by = c("Country", "Year", "Indirect_Energy", "Product.Group", "Energy.stage")
    ) %>%
    dplyr::mutate(
      ratio_indirect_energy_per_output = tidyr::replace_na(.data[["ratio_indirect_energy_per_output"]], 0),
      Group.eroi = 1/(1/Group.eroi + .data[["ratio_indirect_energy_per_output"]])
    ) %>%
    dplyr::select(-.data[["ratio_indirect_energy_per_output"]])
  
  return(tidy_summarised_erois_with_idE)
}
