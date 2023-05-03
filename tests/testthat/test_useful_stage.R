
test_that("push_to_useful_erois works",{
  
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
    IEATools::add_psut_matnames(R_includes_all_exogenous_flows = FALSE) %>%
    ECCTools::stat_diffs_to_balancing() %>%
    ECCTools::stock_changes_to_balancing()
  
  
  # FIRST, WE TEST THE DTA APPROACH
  
  # Calculating total use of each product
  tidy_AB_dta <- tidy_AB_data %>%
    ECCTools::transform_to_dta(requirement_matrices_list = c("U_feed"),
                               select_dta_observations = FALSE)
  
  # Calculating IO matrices
  tidy_io_AB_dta <- tidy_AB_data %>%
    IEATools::prep_psut() %>%
    Recca::calc_io_mats(method_q_calculation = "sum_R_V_cols")
  
  # Calculating tidy IO EROIs
  tidy_AB_erois_dta <- tidy_io_AB_dta %>%
    Recca::calc_E_EIOU() %>%
    Recca::calc_erois() %>%
    EROITools::extract_tidy_product_erois() %>%
    dplyr::mutate(
      Eroi.method = "DTA"
    ) %>%
    dplyr::relocate(tidyselect::all_of("Eroi.method"), .after = Year)
  
  # Pushing to tidy useful stage EROIs
  length_to_use <- tidy_AB_erois_dta %>% 
    dplyr::select(Country, Method, Energy.type, Year, Product) %>% 
    dplyr::distinct() %>% 
    nrow()
  
  tidy_FU_efficiencies_dta <- tidy_AB_erois_dta %>% 
    dplyr::select(Country, Method, Energy.type, Year, Product) %>% 
    dplyr::distinct() %>% 
    dplyr::mutate(
      Average_Efficiency_Col = seq(0.15, 1, 0.85/(length_to_use-1))
    )
  
  # First, manually
  tidy_useful_erois_dta <- tidy_AB_erois_dta %>% 
    dplyr::left_join(tidy_FU_efficiencies_dta,
                     by = c("Country", "Method", "Energy.type", "Year", "Product")) %>%
    dplyr::mutate(
      Useful_Stage_EROI = Average_Efficiency_Col * EROI
    ) %>% 
    dplyr::filter(! is.na(Useful_Stage_EROI))
  
  # Second, with the new function:
  tidy_useful_erois_dta_with_function <- push_to_useful_erois(
    .tidy_io_erois = tidy_AB_erois_dta,
    tidy_FU_efficiencies = tidy_FU_efficiencies_dta,
    eroi_calc_method = "dta"
  )
  
  # Test they are the same
  expect_true(all(tidy_useful_erois_dta == tidy_useful_erois_dta_with_function))
  
  # Check couple of values:
  tidy_useful_erois_dta_with_function %>% 
    dplyr::filter(Country == "A", Product == "Electricity [from Natural gas]", Type == "Gross", Boundary == "All") %>% 
    magrittr::extract2("Useful_Stage_EROI") %>% 
    expect_equal(6.9495, tolerance = 1e-3)
  tidy_useful_erois_dta_with_function %>% 
    dplyr::filter(Country == "A", Product == "Electricity [from Natural gas]", Type == "Gross", Boundary == "Feedstock") %>% 
    magrittr::extract2("Useful_Stage_EROI") %>% 
    expect_equal(7.274943, tolerance = 1e-3)
  
  # Check what happens if two end uses:
  tidy_FU_efficiencies_dta_end_uses <- tidy_FU_efficiencies_dta %>% 
    tidyr::expand_grid(End_Use = c("EU1", "EU2", "EU3"))
  
  tidy_useful_erois_dta_end_uses <- push_to_useful_erois(
    .tidy_io_erois = tidy_AB_erois_dta,
    tidy_FU_efficiencies = tidy_FU_efficiencies_dta_end_uses,
    eroi_calc_method = "dta"
  )
  
  expect_equal(tidy_useful_erois_dta_with_function %>% nrow() * 3,
               tidy_useful_erois_dta_end_uses %>% nrow())
  
  tidy_useful_erois_dta_end_uses %>% 
    dplyr::filter(Country == "A", Product == "Blast furnace gas", Type == "Gross", Boundary == "All", End_Use == "EU1") %>% 
    magrittr::extract2("Useful_Stage_EROI") %>% 
    expect_equal(1.342899763, tolerance = 1e-3)
  tidy_useful_erois_dta_end_uses %>% 
    dplyr::filter(Country == "A", Product == "Electricity [from Natural gas]", Type == "Gross", Boundary == "Feedstock", End_Use == "EU1") %>% 
    magrittr::extract2("Useful_Stage_EROI") %>% 
    expect_equal(7.274943, tolerance = 1e-3)
  tidy_useful_erois_dta_end_uses %>% 
    dplyr::filter(Country == "A", Product == "Electricity [from Natural gas]", Type == "Gross", Boundary == "Feedstock", End_Use == "EU2") %>% 
    magrittr::extract2("Useful_Stage_EROI") %>% 
    expect_equal(7.274943, tolerance = 1e-3)
  tidy_useful_erois_dta_end_uses %>% 
    dplyr::filter(Country == "A", Product == "Electricity [from Natural gas]", Type == "Gross", Boundary == "Feedstock", End_Use == "EU3") %>% 
    magrittr::extract2("Useful_Stage_EROI") %>% 
    expect_equal(7.274943, tolerance = 1e-3)
  
  
  # SECOND, WE TEST THE GMA APPROACH
  
  tidy_AB_data_gma <- tidy_AB_data %>%
    ECCTools::transform_to_gma()
  
  # Calculates IO mats - GMA
  tidy_io_AB_gma <- tidy_AB_data_gma %>% 
    IEATools::prep_psut() %>% 
    Recca::calc_io_mats(method_q_calculation = "sum_R_V_cols")
  
  # Calculates tidy IO erois - GMA
  tidy_AB_erois_gma <- tidy_io_AB_gma %>% 
    Recca::calc_E_EIOU() %>% 
    Recca::calc_erois() %>% 
    EROITools::extract_tidy_product_erois() %>% 
    dplyr::mutate(
      Eroi.method = "DTA"
    ) %>% 
    dplyr::relocate(tidyselect::all_of("Eroi.method"), .after = Year)
  
  # Prepare GMA data frame for shares calculations
  # Pushing to tidy useful stage EROIs
  length_to_use <- tidy_AB_erois_gma %>% 
    dplyr::select(Country, Method, Energy.type, Year, Product) %>% 
    dplyr::distinct() %>% 
    nrow()
  
  tidy_FU_efficiencies_gma <- tidy_AB_erois_gma %>% 
    dplyr::mutate(
      Country = stringr::str_extract(Product, "\\{.*\\}") %>% 
        stringr::str_remove("\\{") %>% stringr::str_remove("\\}"),
      Product = stringr::str_remove(Product, "\\{.*\\}_")
    ) %>% 
    dplyr::select(Country, Method, Energy.type, Year, Product) %>% 
    dplyr::distinct() %>% 
    dplyr::mutate(
      Average_Efficiency_Col = seq(0.15, 1, 0.85/(length_to_use-1))
    )
  
  # First, manually:
  tidy_io_erois_adapted_gma <- tidy_AB_erois_gma %>% 
    dplyr::mutate(product_without_origin = stringr::str_remove(Product, "\\{.*\\}_")) %>% 
    dplyr::select(-Country)
  
  # Calculates tidy_national_useful_stage_erois
  tidy_useful_erois_gma <- tidy_FU_efficiencies_gma %>%
    dplyr::rename(product_without_origin = Product) %>% 
    dplyr::left_join(
      tidy_io_erois_adapted_gma,
      by = c("Method", "Energy.type", "Year", "product_without_origin"),
      relationship = "many-to-many"
    ) %>% 
    dplyr::mutate(
      Useful_Stage_EROI = Average_Efficiency_Col * EROI
    ) %>%
    dplyr::select(-product_without_origin) %>% 
    dplyr::filter(! is.na(Useful_Stage_EROI))
  
  # Second, with the new function:
  tidy_useful_erois_gma_with_function <- push_to_useful_erois(
    .tidy_io_erois = tidy_AB_erois_gma,
    tidy_FU_efficiencies = tidy_FU_efficiencies_gma,
    eroi_calc_method = "gma"
  )
  
  # Expect the same results
  expect_true(all(tidy_useful_erois_gma == tidy_useful_erois_gma_with_function))
  
  # Check couple of values:
  tidy_useful_erois_gma_with_function %>% 
    dplyr::filter(Country == "A", Product == "{A}_Blast furnace gas", Type == "Gross", Boundary == "All") %>% 
    magrittr::extract2("Useful_Stage_EROI") %>% 
    expect_equal(0.4350455, tolerance = 1e-4)
  tidy_useful_erois_gma_with_function %>% 
    dplyr::filter(Country == "A", Product == "{B}_Blast furnace gas", Type == "Gross", Boundary == "All") %>% 
    magrittr::extract2("Useful_Stage_EROI") %>% 
    expect_equal(0.2136091, tolerance = 1e-4)
  tidy_useful_erois_gma_with_function %>% 
    dplyr::filter(Country == "B", Product == "{A}_Blast furnace gas", Type == "Gross", Boundary == "All") %>% 
    magrittr::extract2("Useful_Stage_EROI") %>% 
    expect_equal(1.873112, tolerance = 1e-4)
  tidy_useful_erois_gma_with_function %>% 
    dplyr::filter(Country == "B", Product == "{B}_Blast furnace gas", Type == "Gross", Boundary == "All") %>% 
    magrittr::extract2("Useful_Stage_EROI") %>% 
    expect_equal(0.919706, tolerance = 1e-4)
  
  # With end uses:
  tidy_FU_efficiencies_gma_end_uses <- tidy_FU_efficiencies_gma %>% 
    tidyr::expand_grid(End_Use = c("EU1", "EU2", "EU3"))
  
  tidy_useful_erois_gma_end_uses <- push_to_useful_erois(
    .tidy_io_erois = tidy_AB_erois_gma,
    tidy_FU_efficiencies = tidy_FU_efficiencies_gma_end_uses,
    eroi_calc_method = "gma"
  )
  
  # Check couple of values:
  tidy_useful_erois_gma_end_uses %>% 
    dplyr::filter(Country == "A", Product == "{A}_Blast furnace gas", Type == "Gross", Boundary == "All", End_Use == "EU1") %>% 
    magrittr::extract2("Useful_Stage_EROI") %>% 
    expect_equal(0.4350455, tolerance = 1e-4)
  tidy_useful_erois_gma_end_uses %>% 
    dplyr::filter(Country == "A", Product == "{B}_Blast furnace gas", Type == "Gross", Boundary == "All", End_Use == "EU2") %>% 
    magrittr::extract2("Useful_Stage_EROI") %>% 
    expect_equal(0.2136091, tolerance = 1e-4)
  tidy_useful_erois_gma_end_uses %>% 
    dplyr::filter(Country == "B", Product == "{A}_Blast furnace gas", Type == "Gross", Boundary == "All", End_Use == "EU3") %>% 
    magrittr::extract2("Useful_Stage_EROI") %>% 
    expect_equal(1.873112, tolerance = 1e-4)
  tidy_useful_erois_gma_end_uses %>% 
    dplyr::filter(Country == "B", Product == "{B}_Blast furnace gas", Type == "Gross", Boundary == "All", End_Use == "EU1") %>% 
    magrittr::extract2("Useful_Stage_EROI") %>% 
    expect_equal(0.919706, tolerance = 1e-4)
})






test_that("calc_avg_efficiency_by_ff_group works",{
  
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
    IEATools::add_psut_matnames(R_includes_all_exogenous_flows = FALSE) %>%
    ECCTools::stat_diffs_to_balancing() %>%
    ECCTools::stock_changes_to_balancing()
  
  
  # FIRST, WE TEST THE DTA APPROACH
  
  tidy_AB_dta <- tidy_AB_data %>%
    ECCTools::transform_to_dta(requirement_matrices_list = c("U_feed"),
                               select_dta_observations = FALSE)
  
  # Building efficiencies data frame:
  length_to_use <- tidy_AB_data %>% 
    dplyr::select(Country, Method, Energy.type, Year, Product) %>% 
    dplyr::distinct() %>% 
    nrow()
  
  tidy_FU_efficiencies <- tidy_AB_data %>% 
    dplyr::select(Country, Method, Energy.type, Year, Product) %>% 
    dplyr::distinct() %>% 
    dplyr::mutate(
      Average_Efficiency_Col = seq(0.15, 1, 0.85/(length_to_use-1))
    )
  
  # Calculating aggregated efficiencies
  aggregated_efficiencies_dta <- calc_avg_efficiency_by_ff_group(
    .tidy_efficiencies_df = tidy_FU_efficiencies,
    .tidy_iea_df = tidy_AB_dta,
    calc_method = "dta"
  )
  
  # Testing:
  aggregated_efficiencies_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Coal products", Energy.stage == "Useful (fuel)") %>% 
    magrittr::extract2("Aggregated_Efficiency") %>% 
    expect_equal(0.1825334, tolerance = 1e-4)
  aggregated_efficiencies_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil products", Energy.stage == "Useful (fuel)") %>% 
    magrittr::extract2("Aggregated_Efficiency") %>% 
    expect_equal(0.2419002, tolerance = 1e-4)
  aggregated_efficiencies_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Natural gas", Energy.stage == "Useful (fuel)") %>% 
    magrittr::extract2("Aggregated_Efficiency") %>% 
    expect_equal(0.2787879, tolerance = 1e-4)
  aggregated_efficiencies_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products", Energy.stage == "Useful (fuel)") %>% 
    magrittr::extract2("Aggregated_Efficiency") %>% 
    expect_equal(0.2473364, tolerance = 1e-4)
  aggregated_efficiencies_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Energy.stage == "Useful (fuel)") %>% 
    magrittr::extract2("Aggregated_Efficiency") %>% 
    expect_equal(0.2365364, tolerance = 1e-4)
  aggregated_efficiencies_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Coal products", Energy.stage == "Useful (electricity)") %>% 
    magrittr::extract2("Aggregated_Efficiency") %>% 
    expect_equal(0.5363636, tolerance = 1e-4)
  aggregated_efficiencies_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil products", Energy.stage == "Useful (electricity)") %>% 
    nrow() %>% 
    expect_equal(0)
  aggregated_efficiencies_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Natural gas", Energy.stage == "Useful (electricity)") %>% 
    magrittr::extract2("Aggregated_Efficiency") %>% 
    expect_equal(0.5621212, tolerance = 1e-4)
  aggregated_efficiencies_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products", Energy.stage == "Useful (electricity)") %>% 
    magrittr::extract2("Aggregated_Efficiency") %>% 
    expect_equal(0.5621212, tolerance = 1e-4)
  aggregated_efficiencies_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Energy.stage == "Useful (electricity)") %>% 
    magrittr::extract2("Aggregated_Efficiency") %>% 
    expect_equal(0.5410463, tolerance = 1e-4)
  aggregated_efficiencies_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Coal products", Energy.stage == "Useful (heat)") %>%
    magrittr::extract2("Aggregated_Efficiency") %>%
    expect_equal(0.5878788, tolerance = 1e-4)
  aggregated_efficiencies_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Oil products", Energy.stage == "Useful (heat)") %>%
    nrow() %>% 
    expect_equal(0)
  aggregated_efficiencies_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Natural gas", Energy.stage == "Useful (heat)") %>%
    magrittr::extract2("Aggregated_Efficiency") %>%
    expect_equal(0.6136364, tolerance = 1e-4)
  aggregated_efficiencies_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products", Energy.stage == "Useful (heat)") %>%
    magrittr::extract2("Aggregated_Efficiency") %>%
    expect_equal(0.6136364, tolerance = 1e-4)
  aggregated_efficiencies_dta %>%
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Energy.stage == "Useful (heat)") %>%
    magrittr::extract2("Aggregated_Efficiency") %>%
    expect_equal(0.5925614, tolerance = 1e-4)
  aggregated_efficiencies_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Coal products", Energy.stage == "Useful (fuel+elec+heat)") %>%
    magrittr::extract2("Aggregated_Efficiency") %>%
    expect_equal(0.4485493, tolerance = 1e-4)
  aggregated_efficiencies_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Oil products", Energy.stage == "Useful (fuel+elec+heat)") %>%
    magrittr::extract2("Aggregated_Efficiency") %>%
    expect_equal(0.2419002, tolerance = 1e-4)
  aggregated_efficiencies_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Natural gas", Energy.stage == "Useful (fuel+elec+heat)") %>%
    magrittr::extract2("Aggregated_Efficiency") %>%
    expect_equal(0.4130826, tolerance = 1e-4)
  aggregated_efficiencies_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products", Energy.stage == "Useful (fuel+elec+heat)") %>%
    magrittr::extract2("Aggregated_Efficiency") %>%
    expect_equal(0.283935, tolerance = 1e-4)
  aggregated_efficiencies_dta %>%
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Energy.stage == "Useful (fuel+elec+heat)") %>%
    magrittr::extract2("Aggregated_Efficiency") %>%
    expect_equal(0.3514432, tolerance = 1e-4)
  
  
  # SECOND, WE TEST THE GMA APPROACH
  
  tidy_AB_data_gma <- tidy_AB_data %>%
    ECCTools::transform_to_gma()
  
  # Prepare GMA data frame for shares calculations
  # Pushing to tidy useful stage EROIs
  # length_to_use <- tidy_AB_data %>% 
  #   dplyr::select(Country, Method, Energy.type, Year, Product) %>% 
  #   dplyr::distinct() %>% 
  #   nrow()
  # 
  # tidy_FU_efficiencies_gma <- tidy_AB_data %>% 
  #   dplyr::select(Country, Method, Energy.type, Year, Product) %>% 
  #   dplyr::distinct() %>% 
  #   dplyr::mutate(
  #     Average_Efficiency_Col = seq(0.15, 1, 0.85/(length_to_use-1))
  #   ) %>% 
  #   print()
  
  # Adapting ECC for shares calculations:
  tidy_AB_data_gma_prepared <- prepare_gma_for_shares(tidy_AB_data_gma)
  
  # Calculating aggregated efficiencies
  aggregated_efficiencies_gma <- calc_avg_efficiency_by_ff_group(
    .tidy_efficiencies_df = tidy_FU_efficiencies,
    .tidy_iea_df = tidy_AB_data_gma_prepared,
    calc_method = "gma"
  )
  
  # Testing:
  aggregated_efficiencies_gma %>% 
    dplyr::filter(Country == "A", Product.Group == "Coal products", Energy.stage == "Useful (fuel)") %>% 
    magrittr::extract2("Aggregated_Efficiency") %>% 
    expect_equal(0.1825334, tolerance = 1e-4)
  aggregated_efficiencies_gma %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil products", Energy.stage == "Useful (fuel)") %>% 
    magrittr::extract2("Aggregated_Efficiency") %>% 
    expect_equal(0.2419002, tolerance = 1e-4)
  aggregated_efficiencies_gma %>% 
    dplyr::filter(Country == "A", Product.Group == "Natural gas", Energy.stage == "Useful (fuel)") %>% 
    magrittr::extract2("Aggregated_Efficiency") %>% 
    expect_equal(0.2787879, tolerance = 1e-4)
  aggregated_efficiencies_gma %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products", Energy.stage == "Useful (fuel)") %>% 
    magrittr::extract2("Aggregated_Efficiency") %>% 
    expect_equal(0.2473364, tolerance = 1e-4)
  aggregated_efficiencies_gma %>% 
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Energy.stage == "Useful (fuel)") %>% 
    magrittr::extract2("Aggregated_Efficiency") %>% 
    expect_equal(0.2365364, tolerance = 1e-4)
  aggregated_efficiencies_gma %>% 
    dplyr::filter(Country == "A", Product.Group == "Coal products", Energy.stage == "Useful (electricity)") %>% 
    magrittr::extract2("Aggregated_Efficiency") %>% 
    expect_equal(0.5363636, tolerance = 1e-4)
  aggregated_efficiencies_gma %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil products", Energy.stage == "Useful (electricity)") %>% 
    nrow() %>% 
    expect_equal(0)
  aggregated_efficiencies_gma %>% 
    dplyr::filter(Country == "A", Product.Group == "Natural gas", Energy.stage == "Useful (electricity)") %>% 
    magrittr::extract2("Aggregated_Efficiency") %>% 
    expect_equal(0.5621212, tolerance = 1e-4)
  aggregated_efficiencies_gma %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products", Energy.stage == "Useful (electricity)") %>% 
    magrittr::extract2("Aggregated_Efficiency") %>% 
    expect_equal(0.5621212, tolerance = 1e-4)
  aggregated_efficiencies_gma %>% 
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Energy.stage == "Useful (electricity)") %>% 
    magrittr::extract2("Aggregated_Efficiency") %>% 
    expect_equal(0.5410463, tolerance = 1e-4)
  aggregated_efficiencies_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Coal products", Energy.stage == "Useful (heat)") %>%
    magrittr::extract2("Aggregated_Efficiency") %>%
    expect_equal(0.5878788, tolerance = 1e-4)
  aggregated_efficiencies_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Oil products", Energy.stage == "Useful (heat)") %>%
    nrow() %>% 
    expect_equal(0)
  aggregated_efficiencies_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Natural gas", Energy.stage == "Useful (heat)") %>%
    magrittr::extract2("Aggregated_Efficiency") %>%
    expect_equal(0.6136364, tolerance = 1e-4)
  aggregated_efficiencies_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products", Energy.stage == "Useful (heat)") %>%
    magrittr::extract2("Aggregated_Efficiency") %>%
    expect_equal(0.6136364, tolerance = 1e-4)
  aggregated_efficiencies_gma %>%
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Energy.stage == "Useful (heat)") %>%
    magrittr::extract2("Aggregated_Efficiency") %>%
    expect_equal(0.5925614, tolerance = 1e-4)
  aggregated_efficiencies_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Coal products", Energy.stage == "Useful (fuel+elec+heat)") %>%
    magrittr::extract2("Aggregated_Efficiency") %>%
    expect_equal(0.4485493, tolerance = 1e-4)
  aggregated_efficiencies_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Oil products", Energy.stage == "Useful (fuel+elec+heat)") %>%
    magrittr::extract2("Aggregated_Efficiency") %>%
    expect_equal(0.2419002, tolerance = 1e-4)
  aggregated_efficiencies_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Natural gas", Energy.stage == "Useful (fuel+elec+heat)") %>%
    magrittr::extract2("Aggregated_Efficiency") %>%
    expect_equal(0.4130826, tolerance = 1e-4)
  aggregated_efficiencies_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products", Energy.stage == "Useful (fuel+elec+heat)") %>%
    magrittr::extract2("Aggregated_Efficiency") %>%
    expect_equal(0.283935, tolerance = 1e-4)
  aggregated_efficiencies_gma %>%
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Energy.stage == "Useful (fuel+elec+heat)") %>%
    magrittr::extract2("Aggregated_Efficiency") %>%
    expect_equal(0.3514432, tolerance = 1e-4)
})

