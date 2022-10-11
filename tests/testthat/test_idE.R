
test_that("add_indirect_energy_to_erois works as intended",{
  
  # Path to dummy AB data
  A_B_path <- system.file("extdata/A_B_data_full_2018_format_stat_diffs_stock_changes.csv", package = "EROITools")
  
  # Loading data
  tidy_AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df() %>%
    IEATools::specify_all() %>%
    ECCTools::specify_elect_heat_renewables() %>%
    ECCTools::specify_elect_heat_fossil_fuels() %>%
    ECCTools::specify_elect_heat_nuclear() %>%
    ECCTools::specify_other_elec_heat_production() %>%
    ECCTools::specify_elect_heat_markets() %>%
    IEATools::add_psut_matnames() %>%
    ECCTools::stat_diffs_to_balancing() %>%
    ECCTools::stock_changes_to_balancing()
  
  # Calculating total use of each product
  tidy_AB_dta <- tidy_AB_data %>%
    ECCTools::transform_to_dta(requirement_matrices_list = c("U_feed"),
                               select_dta_observations = FALSE)
  
  
  tidy_io_AB_dta <- tidy_AB_dta %>% 
    IEATools::prep_psut() %>% 
    Recca::calc_io_mats(method_q_calculation = "sum_R_V_cols")
  
  tidy_AB_erois_dta <- tidy_io_AB_dta %>% 
    Recca::calc_E_EIOU() %>% 
    Recca::calc_erois() %>% 
    EROITools::extract_tidy_product_erois() %>% 
    dplyr::mutate(
      Eroi.method = "DTA"
    ) %>% 
    dplyr::relocate(.data[["Eroi.method"]], .after = Year)
  
  
  # Primary stage EROIs:
  primary_erois <- aggregate_primary_stage_erois(
    .tidy_erois_df = tidy_AB_erois_dta,
    .tidy_iea_df = tidy_AB_dta,
    eroi_calc_method = "dta"
  )
  
  # Final stage EROIs:
  final_erois <- aggregate_final_stage_erois(
    .tidy_erois_df = tidy_AB_erois_dta,
    .tidy_iea_df = tidy_AB_dta,
    eroi_calc_method = "dta"
  )
  
  # Pushing to tidy useful stage EROIs:
  length_to_use <- tidy_AB_erois_dta %>% 
    dplyr::select(Country, Method, Energy.type, Year, Product) %>% 
    dplyr::distinct() %>% 
    nrow()
  
  tidy_FU_efficiencies_dta <- tidy_AB_erois_dta %>% 
    dplyr::select(Country, Method, Energy.type, Year, Product) %>% 
    dplyr::distinct() %>% 
    dplyr::mutate(
      Average_Efficiency_Global = seq(0.15, 1, 0.85/(length_to_use-1))
    )
  
  tidy_useful_erois_dta <- tidy_AB_erois_dta %>% 
    dplyr::left_join(tidy_FU_efficiencies_dta,
                     by = c("Country", "Method", "Energy.type", "Year", "Product")) %>%
    dplyr::mutate(
      Useful_Stage_EROI = Average_Efficiency_Global * EROI
    ) %>% 
    dplyr::filter(! is.na(Useful_Stage_EROI))
  
  # Useful stage EROIs:
  useful_erois <- aggregate_useful_stage_erois(
    .tidy_erois_df = tidy_useful_erois_dta,
    .tidy_iea_df = tidy_AB_dta,
    eroi_calc_method = "dta"
  )
  
  # All EROIs in single data frame:
  all_erois <- dplyr::bind_rows(
    primary_erois,
    final_erois,
    useful_erois
  )
  
  prepare_idE_df_1 <- all_erois %>% 
    dplyr::filter(Energy.stage == "Primary" | stringr::str_detect(Energy.stage, "Final")) %>% 
    dplyr::mutate(
      Energy.stage = dplyr::case_when(
        stringr::str_detect(Energy.stage, "Final") ~ "Final",
        TRUE ~ Energy.stage
      )
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(Country, Year, Product.Group, Energy.stage) %>% 
    dplyr::mutate(
      Indirect_Energy = "Included"
    ) %>% 
    dplyr::distinct()
  
  n_rows <- prepare_idE_df_1 %>% 
    nrow()
  
  prepare_idE_df_2 <- prepare_idE_df_1 %>% 
    dplyr::mutate(
      Indirect_Energy_ktoe = seq(0, 1000, 1000/(n_rows - 1))
    )
  
  res_idE <- add_indirect_energy_to_erois(
    .tidy_summarised_erois_df = all_erois %>% dplyr::filter(Country == "A"),
    .tidy_indirect_energy = prepare_idE_df_2 %>% dplyr::filter(Country == "A"),
    .tidy_iea_df = tidy_AB_dta %>% dplyr::filter(Country == "A"),
  )
  
  # And finally testing the values obtained:
  
  # First, primary energy stage:
  res_idE %>%
    dplyr::filter(Country == "A", Type == "Gross", Boundary == "All", Product.Group == "All fossil fuels", Energy.stage == "Primary", Indirect_Energy == "Included") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(40.52669, tolerance = 1e-3)

  res_idE %>% 
    dplyr::filter(Country == "A", Type == "Gross", Boundary == "All", Product.Group == "Oil and gas products", Energy.stage == "Primary", Indirect_Energy == "Included") %>% 
    magrittr::extract2("Group.eroi") %>% 
    expect_equal(23.06059, tolerance = 1e-3)
  
  res_idE %>% 
    dplyr::filter(Country == "A", Type == "Gross", Boundary == "All", Product.Group == "Coal products", Energy.stage == "Primary", Indirect_Energy == "Included") %>% 
    magrittr::extract2("Group.eroi") %>% 
    expect_equal(36.18344, tolerance = 1e-3)
  
  # Second, final energy stage:
  res_idE %>% 
    dplyr::filter(Country == "A", Type == "Gross", Boundary == "All", Product.Group == "All fossil fuels", Energy.stage == "Final (fuel)", Indirect_Energy == "Included") %>% 
    magrittr::extract2("Group.eroi") %>% 
    expect_equal(7.144977, tolerance = 1e-3)
  
  res_idE %>% 
    dplyr::filter(Country == "A", Type == "Gross", Boundary == "All", Product.Group == "All fossil fuels", Energy.stage == "Final (electricity)", Indirect_Energy == "Included") %>% 
    magrittr::extract2("Group.eroi") %>% 
    expect_equal(15.30191, tolerance = 1e-3)
  
  res_idE %>% 
    dplyr::filter(Country == "A", Type == "Gross", Boundary == "All", Product.Group == "All fossil fuels", Energy.stage == "Final (heat)", Indirect_Energy == "Included") %>% 
    magrittr::extract2("Group.eroi") %>% 
    expect_equal(15.30191, tolerance = 1e-3)
  
  res_idE %>% 
    dplyr::filter(Country == "A", Type == "Gross", Boundary == "All", Product.Group == "All fossil fuels", Energy.stage == "Final (fuel+elec+heat)", Indirect_Energy == "Included") %>% 
    magrittr::extract2("Group.eroi") %>% 
    expect_equal(8.911203, tolerance = 1e-3)
  
  res_idE %>% 
    dplyr::filter(Country == "A", Type == "Gross", Boundary == "All", Product.Group == "Natural gas", Energy.stage == "Final (fuel)", Indirect_Energy == "Included") %>% 
    magrittr::extract2("Group.eroi") %>% 
    expect_equal(2.874926, tolerance = 1e-3)
  
  res_idE %>% 
    dplyr::filter(Country == "A", Type == "Gross", Boundary == "All", Product.Group == "Natural gas", Energy.stage == "Final (electricity)", Indirect_Energy == "Included") %>% 
    magrittr::extract2("Group.eroi") %>% 
    expect_equal(2.738458, tolerance = 1e-3)
  
  # Third, useful energy stage:
  res_idE %>% 
    dplyr::filter(Country == "A", Type == "Gross", Boundary == "All", Product.Group == "All fossil fuels", Energy.stage == "Useful (fuel)", Indirect_Energy == "Included") %>% 
    magrittr::extract2("Group.eroi") %>% 
    expect_equal(2.260515, tolerance = 1e-2)
  
  res_idE %>% 
    dplyr::filter(Country == "A", Type == "Gross", Boundary == "All", Product.Group == "All fossil fuels", Energy.stage == "Useful (electricity)", Indirect_Energy == "Included") %>% 
    magrittr::extract2("Group.eroi") %>% 
    expect_equal(4.799236, tolerance = 1e-2)
  
  res_idE %>% 
    dplyr::filter(Country == "A", Type == "Gross", Boundary == "All", Product.Group == "Natural gas", Energy.stage == "Useful (fuel)", Indirect_Energy == "Included") %>% 
    magrittr::extract2("Group.eroi") %>% 
    expect_equal(1.609235, tolerance = 1e-3)
  
})




test_that("add_indirect_energy_useful_erois_by works as intended",{
  
  # Path to dummy AB data
  A_B_path <- system.file("extdata/A_B_data_full_2018_format_stat_diffs_stock_changes.csv", package = "EROITools")
  
  # Loading data
  tidy_AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df() %>%
    IEATools::specify_all() %>%
    ECCTools::specify_elect_heat_renewables() %>%
    ECCTools::specify_elect_heat_fossil_fuels() %>%
    ECCTools::specify_elect_heat_nuclear() %>%
    ECCTools::specify_other_elec_heat_production() %>%
    ECCTools::specify_elect_heat_markets() %>%
    IEATools::add_psut_matnames() %>%
    ECCTools::stat_diffs_to_balancing() %>%
    ECCTools::stock_changes_to_balancing()
  
  # Calculating total use of each product
  tidy_AB_dta <- tidy_AB_data %>%
    ECCTools::transform_to_dta(requirement_matrices_list = c("U_feed"),
                               select_dta_observations = FALSE)
  
  
  tidy_io_AB_dta <- tidy_AB_dta %>% 
    IEATools::prep_psut() %>% 
    Recca::calc_io_mats(method_q_calculation = "sum_R_V_cols")
  
  tidy_AB_erois_dta <- tidy_io_AB_dta %>% 
    Recca::calc_E_EIOU() %>% 
    Recca::calc_erois() %>% 
    EROITools::extract_tidy_product_erois() %>% 
    dplyr::mutate(
      Eroi.method = "DTA"
    ) %>% 
    dplyr::relocate(.data[["Eroi.method"]], .after = Year)
  
  
  # List of breakdown categories
  list_breakdown_categories <- c("C1", "C2", "C3", "C4", "C5")
  
  # Final stage EROIs:
  final_erois <- aggregate_final_stage_erois(
    .tidy_erois_df = tidy_AB_erois_dta,
    .tidy_iea_df = tidy_AB_dta,
    eroi_calc_method = "dta"
  ) %>% 
    tidyr::expand_grid(
      Categories = list_breakdown_categories
    )
  
  # Pushing to tidy useful stage EROIs:
  length_to_use <- tidy_AB_erois_dta %>%
    dplyr::select(Country, Method, Energy.type, Year, Product) %>% 
    dplyr::distinct() %>% 
    nrow()
  
  tidy_FU_efficiencies_dta <- tidy_AB_erois_dta %>% 
    dplyr::select(Country, Method, Energy.type, Year, Product) %>% 
    dplyr::distinct() %>% 
    dplyr::mutate(
      Average_Efficiency_Global = seq(0.15, 1, 0.85/(length_to_use-1))
    )
  
  tidy_useful_erois_dta <- tidy_AB_erois_dta %>% 
    dplyr::left_join(tidy_FU_efficiencies_dta,
                     by = c("Country", "Method", "Energy.type", "Year", "Product")) %>%
    dplyr::mutate(
      Useful_Stage_EROI = Average_Efficiency_Global * EROI
    ) %>% 
    dplyr::filter(! is.na(Useful_Stage_EROI))
  
  # Useful stage EROIs:
  useful_erois <- aggregate_useful_stage_erois(
    .tidy_erois_df = tidy_useful_erois_dta,
    .tidy_iea_df = tidy_AB_dta,
    eroi_calc_method = "dta"
  ) %>% 
    tidyr::expand_grid(
      Categories = list_breakdown_categories
    ) %>% 
    dplyr::mutate(
      Group.eroi = dplyr::case_when(
        Categories == "C1" ~ Group.eroi/2,
        Categories == "C2" ~ Group.eroi/3,
        Categories == "C3" ~ Group.eroi/4,
        Categories == "C4" ~ Group.eroi/5,
        TRUE ~ Group.eroi
      )
    )
  
  # All EROIs in single data frame:
  final_useful_erois <- dplyr::bind_rows(
    final_erois,
    useful_erois
  ) %>% 
    dplyr::select(-Last.stage, -Non_Energy_Uses)
  
  prepare_idE_df_1 <- final_useful_erois %>% 
    dplyr::filter(stringr::str_detect(Energy.stage, "Final")) %>% 
    dplyr::mutate(
      Energy.stage = dplyr::case_when(
        stringr::str_detect(Energy.stage, "Final") ~ "Final",
        TRUE ~ Energy.stage
      )
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(Country, Year, Product.Group, Energy.stage) %>% 
    dplyr::mutate(
      Indirect_Energy = "Included"
    ) %>% 
    dplyr::distinct()
  
  n_rows <- prepare_idE_df_1 %>% 
    nrow()
  
  prepare_idE_df_2 <- prepare_idE_df_1 %>% 
    dplyr::mutate(
      Indirect_Energy_ktoe = seq(0, 1000, 1000/(n_rows - 1))
    )
  
  res_idE <- add_indirect_energy_useful_erois_by(
    .tidy_aggregated_erois_by_df = final_useful_erois %>% dplyr::filter(Country == "A"),
    .tidy_indirect_energy = prepare_idE_df_2 %>% dplyr::filter(Country == "A"),
    .tidy_iea_df = tidy_AB_dta %>% dplyr::filter(Country == "A"),
    aggregation_category = "Categories"
  )
  
  # And finally testing the values obtained:
  # First, useful energy stage:
  res_idE %>% 
    dplyr::filter(Country == "A", Type == "Gross", Boundary == "All", Product.Group == "All fossil fuels", Energy.stage == "Useful (fuel)", Indirect_Energy == "Included", Categories == "C1") %>% 
    magrittr::extract2("Group.eroi") %>% 
    expect_equal(1.40662, tolerance = 0.01)
  
  res_idE %>% 
    dplyr::filter(Country == "A", Type == "Gross", Boundary == "All", Product.Group == "All fossil fuels", Energy.stage == "Useful (electricity)", Indirect_Energy == "Included", Categories == "C1") %>% 
    magrittr::extract2("Group.eroi") %>% 
    expect_equal(4.142056, tolerance = 1e-2)
  
  res_idE %>% 
    dplyr::filter(Country == "A", Type == "Gross", Boundary == "All", Product.Group == "Coal products", Energy.stage == "Useful (fuel)", Indirect_Energy == "Included", Categories == "C1") %>% 
    magrittr::extract2("Group.eroi") %>% 
    expect_equal(0.3653858, tolerance = 1e-2)
  
  res_idE %>% 
    dplyr::filter(Country == "A", Type == "Gross", Boundary == "All", Product.Group == "Coal products", Energy.stage == "Useful (electricity)", Indirect_Energy == "Included", Categories == "C5") %>% 
    magrittr::extract2("Group.eroi") %>% 
    expect_equal(4.70873, tolerance = 1e-2)
  
  res_idE %>% 
    dplyr::filter(Country == "A", Type == "Gross", Boundary == "All", Product.Group == "Oil and gas products", Energy.stage == "Useful (fuel)", Indirect_Energy == "Included", Categories == "C2") %>% 
    magrittr::extract2("Group.eroi") %>% 
    expect_equal(1.068693, tolerance = 1e-2)
  
  res_idE %>% 
    dplyr::filter(Country == "A", Type == "Gross", Boundary == "All", Product.Group == "Oil and gas products", Energy.stage == "Useful (electricity)", Indirect_Energy == "Included", Categories == "C3") %>% 
    magrittr::extract2("Group.eroi") %>% 
    expect_equal(0.7637625, tolerance = 1e-2)
})

