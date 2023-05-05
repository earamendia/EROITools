
#' Adds indirect energy to the EROI values
#'
#' To add indirect energy, the function calculates the ratio of indirect energy to output per product group at the global level (see details), 
#' and then adds it to the energy inputs using the inverse of the EROI. The function uses the same ratio for each product group, 
#' independently of whether the energy stage is electricity, heat, fuel, or the combination (it does apply a different ratio at the primary, final and useful stage though).
#' 
#' The .tidy_iea_data frame provided as input should be a representation of the global Energy Conversion Chain, as the ratio of indirect energy to output (primary and final energy stages)
#' is calculated at the global level, so output per fossil fuel group needs to be calculated at the global level.
#'
#' @param .tidy_summarised_erois_df The aggregated EROI values to which indirect energy needs to be added
#' @param .tidy_indirect_energy A tidy data frame containing the indirect energy to be added, in ktoe.
#' @param .tidy_iea_df The `.tidy_iea_df` from which the output per product group is calculated.
#' @param country,year,method,energy_type,last_stage,e_dot,unit See `IEATools::iea_cols`.
#' @param product.group The name of the column containing the product group name.
#'                      Default is "Product.Group".
#' @param energy.stage The name of the column containing the energy stage for the calculation of the EROI.
#'                     Default is "Energy.stage".
#' @param group.eroi The name of the column containing the EROI values.
#'                   Default is "Group.eroi".
#' @param total_group_output The name of the column containing the total product group output.
#'                           Default is "Total_Group_Output".
#' @param indirect_energy_ktoe The name of the column containing the indirect energy data in the input indirect energy data frame.
#'                             Default is "Indirect_Energy_ktoe".
#' @param eroi.method The name of the column containing the eroi method.
#'                    Default is "Eroi.method".
#' @param type The name of the column containing the type of eroi calculated.
#'             Default is "Type".
#' @param boundary The name of the column containing the boundary for the eroi calculation.
#'                 Default is "Boundary".
#' @param final_to_useful_eff The name of the column containing the derived final-to-useful efficiencies.
#'                      Default is "Final_to_useful_efficiency".
#' @param ratio_indirect_energy_per_output The name of the column containing the ratio of indirect energy to product group output.
#'                                         Default is "ratio_indirect_energy_per_output".
#' @param final_to_useful_eff The name of the temporary column containing the calculated final-to-useful efficiencies.
#'                            Default is "Final_to_useful_efficiency".
#' @param include_non_energy_uses A boolean stating whether non-energy uses are included in the calculation of the output per product group.
#'
#' @return A data frame containing the EROIs with the indirect energy now included.
#' @export
add_indirect_energy_to_erois <- function(.tidy_summarised_erois_df,
                                         .tidy_indirect_energy,
                                         .tidy_iea_df,
                                         country = IEATools::iea_cols$country,
                                         year = IEATools::iea_cols$year,
                                         method = IEATools::iea_cols$method,
                                         energy_type = IEATools::iea_cols$energy_type,
                                         last_stage = IEATools::iea_cols$last_stage,
                                         e_dot = IEATools::iea_cols$e_dot,
                                         unit = IEATools::iea_cols$unit,
                                         product.group = "Product.Group",
                                         energy.stage = "Energy.stage",
                                         group.eroi = "Group.eroi",
                                         total_group_output = "Total_Group_Output",
                                         indirect_energy_ktoe = "Indirect_Energy_ktoe",
                                         eroi.method = "Eroi.method",
                                         type = "Type",
                                         boundary = "Boundary",
                                         final_to_useful_eff = "Final_to_useful_efficiency",
                                         ratio_indirect_energy_per_output = "ratio_indirect_energy_per_output",
                                         include_non_energy_uses = TRUE){

  # Working out primary energy supply, and final energy consumption by fossil fuel group (including energy / heat coming grom fossil fuels)
  total_output_per_group <- dplyr::bind_rows(
    calc_primary_products_supply_by_group(.tidy_iea_df,
                                          total_group_supply = total_group_output),
    calc_fec_from_ff_by_group(.tidy_iea_df,
                              include_non_energy_uses = include_non_energy_uses) %>%
      dplyr::rename(
        "{total_group_output}" := tidyselect::all_of(e_dot)
      )
  )

  # Now calculating the ratio of indirect energy by output, both at the primary and final stage, for each group
  indirect_energy_per_output_primary_final <- .tidy_indirect_energy %>%
    dplyr::inner_join(
      total_output_per_group,
      by = c({country}, {year}, {product.group}, {energy.stage}),
      relationship = "many-to-many"
    ) %>%
    dplyr::mutate(
      "{ratio_indirect_energy_per_output}" := .data[[indirect_energy_ktoe]] / .data[[total_group_output]]
    ) %>%
    dplyr::ungroup() %>% 
    dplyr::select(-tidyselect::all_of(c(country, indirect_energy_ktoe, method, energy_type, last_stage, unit, total_group_output)))

  # Calculating the ratio of indirect energy by output, now at the final stage, for each group
  # This is needed because we need the ratio for each of Final (fuel), Final (elec), Final (heat), and Final (fuel+elec+heat)
  indirect_energy_per_output_useful <- .tidy_summarised_erois_df %>%
    dplyr::filter(! .data[[energy.stage]] == "Primary") %>%
    dplyr::filter(.data[[type]] == "Gross") %>%
    dplyr::filter(.data[[boundary]] == "Feedstock") %>%
    dplyr::mutate(
      "{product.group}" := stringr::str_c(.data[[product.group]], stringr::str_extract(.data[[energy.stage]], " \\(.*\\)")),
      "{energy.stage}" := stringr::str_remove(.data[[energy.stage]], " \\(.*\\)")
    ) %>%
    tidyr::pivot_wider(names_from = tidyselect::all_of(energy.stage), values_from = tidyselect::all_of(group.eroi)) %>%
    dplyr::mutate(
      "{final_to_useful_eff}" := .data[["Useful"]] / .data[["Final"]]
    ) %>%
    dplyr::select(-tidyselect::all_of(c("Final", "Useful"))) %>% 
    dplyr::mutate(
      "{energy.stage}" := stringr::str_c("Useful", stringr::str_extract(.data[[product.group]], " \\(.*\\)")),
      "{product.group}" := stringr::str_remove(.data[[product.group]], " \\(.*\\)")
    ) %>%
    dplyr::inner_join(
      indirect_energy_per_output_primary_final %>% dplyr::filter(stringr::str_detect(.data[[energy.stage]], "Final")) %>% dplyr::select(-tidyselect::all_of(energy.stage)),
      by = c({year}, {product.group}),
      relationship = "many-to-many"
    ) %>%
    dplyr::mutate(
      "{ratio_indirect_energy_per_output}" := .data[[ratio_indirect_energy_per_output]] / .data[[final_to_useful_eff]]
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-tidyselect::all_of(c(method, energy_type, last_stage, eroi.method, type, boundary, "Non_Energy_Uses", final_to_useful_eff)))

  
  # List countries
  list_countries <- indirect_energy_per_output_useful %>%
    dplyr::ungroup() %>%
    dplyr::select(tidyselect::all_of(country)) %>%
    dplyr::distinct() %>%
    dplyr::pull()
  
  # Expand, so that the final stage is now available for final (fuel), final (elec), and final (heat)
  # With the same values.
  indirect_energy_per_output_expanded <- indirect_energy_per_output_primary_final %>%
    tidyr::expand_grid("{country}" := list_countries) %>% 
    dplyr::filter(.data[[energy.stage]] == "Primary") %>%
    dplyr::bind_rows(
      indirect_energy_per_output_primary_final %>%
        tidyr::expand_grid("{country}" := list_countries) %>% 
        dplyr::filter(.data[[energy.stage]] == "Final") %>%
        dplyr::select(-tidyselect::all_of(energy.stage)) %>% 
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
      by = c({country}, {year}, "Indirect_Energy", {product.group}, {energy.stage}),
      relationship = "many-to-many"
    ) %>%
    dplyr::mutate(
      Indirect_Energy = dplyr::case_when(
        Indirect_Energy == "Included" & is.na(.data[[ratio_indirect_energy_per_output]]) ~ "Inclusion not possible",
        TRUE ~ Indirect_Energy
      )
    ) %>% 
    dplyr::mutate(
      "{ratio_indirect_energy_per_output}" := tidyr::replace_na(.data[[ratio_indirect_energy_per_output]], 0),
      "{group.eroi}" := 1/(1/.data[[group.eroi]] + .data[[ratio_indirect_energy_per_output]])
    ) %>%
    dplyr::select(-tidyselect::all_of(ratio_indirect_energy_per_output))

  return(tidy_summarised_erois_with_idE)
}



#' Adds indirect energy to useful stage EROI values with given breakdown
#'
#' To add indirect energy, the function calculates the ratio of indirect energy to output per product group, 
#' and then adds it to the energy inputs using the inverse of the EROI. The function starts by calculating the ratio of
#' indirect energy to final energy output, and then uses the average efficiency, either by end-use or final demand sector depending on the breakdown,
#' to determine the ratio indirect energy input to useful energy output. The average efficiency is determined as the ratio of useful stage EROI to final stage EROI.
#' 
#' The .tidy_iea_df provided as input should be a representation of the global Energy Conversion Chain, as the ratio of indirect energy to output (primary and final energy stages)
#' is calculated at the global level, so output per fossil fuel group needs to be calculated at the global level.
#'
#' @param .tidy_aggregated_erois_by_df The aggregated useful stage EROI values, with breakdown, to which indirect energy needs to be added
#' @param .tidy_indirect_energy A tidy data frame containing the indirect energy to be added, in ktoe.
#' @param .tidy_iea_df The `.tidy_iea_df` from which the output per product group is calculated.
#' @param aggregation_category The name of the column standing for the breakdown of EROIs. Default is "EU_category".
#' @param country,year,method,energy_type,last_stage,e_dot,unit See `IEATools::iea_cols`.
#' @param product.group The name of the column containing the product group name.
#'                      Default is "Product.Group".
#' @param energy.stage The name of the column containing the energy stage for the calculation of the EROI.
#'                     Default is "Energy.stage".
#' @param group.eroi The name of the column containing the EROI values.
#'                   Default is "Group.eroi".
#' @param total_group_output The name of the column containing the total product group output.
#'                           Default is "Total_Group_Output".
#' @param indirect_energy_ktoe The name of the column containing the indirect energy data in the input indirect energy data frame.
#'                             Default is "Indirect_Energy_ktoe".
#' @param eroi.method The name of the column containing the eroi method.
#'                    Default is "Eroi.method".
#' @param type The name of the column containing the type of eroi calculated.
#'             Default is "Type".
#' @param boundary The name of the column containing the boundary for the eroi calculation.
#'                 Default is "Boundary".
#' @param final_to_useful_eff The name of the column containing the derived final-to-useful efficiencies.
#'                      Default is "Final_to_useful_efficiency".
#' @param ratio_indirect_energy_per_output The name of the column containing the ratio of indirect energy to product group output.
#'                                         Default is "ratio_indirect_energy_per_output".
#' @param final_to_useful_eff The name of the temporary column containing the calculated final-to-useful efficiencies.
#'                            Default is "Final_to_useful_efficiency".
#' @param include_non_energy_uses A boolean stating whether non-energy uses are included in the calculation of the output per product group.
#'
#' @return A data frame containing the EROIs with considered breakdown, with the indirect energy now included.
#' @export
add_indirect_energy_useful_erois_by <- function(.tidy_aggregated_erois_by_df,
                                                .tidy_indirect_energy,
                                                .tidy_iea_df,
                                                aggregation_category = "EU_category",
                                                country = IEATools::iea_cols$country,
                                                year = IEATools::iea_cols$year,
                                                method = IEATools::iea_cols$method,
                                                energy_type = IEATools::iea_cols$energy_type,
                                                last_stage = IEATools::iea_cols$last_stage,
                                                e_dot = IEATools::iea_cols$e_dot,
                                                unit = IEATools::iea_cols$unit,
                                                product.group = "Product.Group",
                                                energy.stage = "Energy.stage",
                                                group.eroi = "Group.eroi",
                                                total_group_output = "Total_Group_Output",
                                                indirect_energy_ktoe = "Indirect_Energy_ktoe",
                                                eroi.method = "Eroi.method",
                                                type = "Type",
                                                boundary = "Boundary",
                                                final_to_useful_eff = "Final_to_useful_efficiency",
                                                ratio_indirect_energy_per_output = "ratio_indirect_energy_per_output",
                                                include_non_energy_uses = TRUE){
  
  # Working out primary energy supply, and final energy consumption by fossil fuel group (including energy / heat coming grom fossil fuels)
  total_output_per_group <- dplyr::bind_rows(
    EROITools::calc_primary_products_supply_by_group(.tidy_iea_df,
                                                     total_group_supply = total_group_output),
    EROITools::calc_fec_from_ff_by_group(.tidy_iea_df,
                                         include_non_energy_uses = include_non_energy_uses) %>%
      dplyr::rename(
        "{total_group_output}" := tidyselect::all_of(e_dot)
      )
  )
  
  # Now calculating the ratio of indirect energy by output, both at the primary and final stage, for each group
  indirect_energy_per_output_primary_final <- .tidy_indirect_energy %>%
    dplyr::inner_join(
      total_output_per_group,
      by = c({country}, {year}, {product.group}, {energy.stage}),
      relationship = "many-to-many"
    ) %>%
    dplyr::mutate(
      "{ratio_indirect_energy_per_output}" := .data[[indirect_energy_ktoe]] / .data[[total_group_output]]
    ) %>%
    dplyr::ungroup() %>% 
    dplyr::select(-tidyselect::all_of(c(country, indirect_energy_ktoe, method, energy_type, last_stage, unit, total_group_output)))
  
  
  # Calculating the ratio of indirect energy by output, now at the final stage, for each group
  # This is needed because we need the ratio for each of Final (fuel), Final (elec), Final (heat), and Final (fuel+elec+heat)
  indirect_energy_per_output_useful <- .tidy_aggregated_erois_by_df %>%
    dplyr::filter(! .data[[energy.stage]] == "Primary") %>%
    dplyr::filter(.data[[type]] == "Gross") %>%
    dplyr::filter(.data[[boundary]] == "Feedstock") %>%
    dplyr::mutate(
      "{product.group}" := stringr::str_c(.data[[product.group]], stringr::str_extract(.data[[energy.stage]], " \\(.*\\)")),
      "{energy.stage}" := stringr::str_remove(.data[[energy.stage]], " \\(.*\\)")
    ) %>%
    tidyr::pivot_wider(names_from = tidyselect::all_of(energy.stage), values_from = tidyselect::all_of(group.eroi)) %>%
    dplyr::mutate(
      "{final_to_useful_eff}" := .data[["Useful"]] / .data[["Final"]]
    ) %>%
    dplyr::select(-tidyselect::all_of(c("Final", "Useful"))) %>% 
    dplyr::mutate(
      "{energy.stage}" := stringr::str_c("Useful", stringr::str_extract(.data[[product.group]], " \\(.*\\)")),
      "{product.group}" := stringr::str_remove(.data[[product.group]], " \\(.*\\)")
    ) %>%
    dplyr::inner_join(
      indirect_energy_per_output_primary_final %>% dplyr::filter(stringr::str_detect(.data[[energy.stage]], "Final")) %>% dplyr::select(-tidyselect::all_of(energy.stage)),
      by = c({year}, {product.group}),
      relationship = "many-to-many"
    ) %>%
    dplyr::mutate(
      "{ratio_indirect_energy_per_output}" := .data[[ratio_indirect_energy_per_output]] / .data[[final_to_useful_eff]]
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-tidyselect::all_of(c(method, energy_type, eroi.method, type, boundary, final_to_useful_eff)))
  
  # Now, adding indirect energy to EROIs:
  tidy_aggregated_erois_by_with_idE <- .tidy_aggregated_erois_by_df %>% 
    dplyr::filter(stringr::str_detect(.data[[energy.stage]], "Useful")) %>% 
    tidyr::expand_grid(
      Indirect_Energy = c("Included", "Excluded")
    ) %>% 
    dplyr::left_join(
      indirect_energy_per_output_useful,
      by = c({country}, {year}, "Indirect_Energy", {product.group}, {energy.stage}, {aggregation_category}),
      relationship = "many-to-many"
    ) %>% 
    dplyr::mutate(
      Indirect_Energy = dplyr::case_when(
        Indirect_Energy == "Included" & is.na(.data[[ratio_indirect_energy_per_output]]) ~ "Inclusion not possible",
        TRUE ~ Indirect_Energy
      )
    ) %>% 
    dplyr::mutate(
      "{ratio_indirect_energy_per_output}" := tidyr::replace_na(.data[[ratio_indirect_energy_per_output]], 0),
      "{group.eroi}" := 1/(1/.data[[group.eroi]] + .data[[ratio_indirect_energy_per_output]])
    ) %>% 
    dplyr::select(-tidyselect::all_of(ratio_indirect_energy_per_output))
  
  return(tidy_aggregated_erois_by_with_idE)
}

