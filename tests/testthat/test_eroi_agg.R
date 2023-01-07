
test_that("aggregate_primary_stage_erois works",{

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


  # FIRST, WE TEST THE DTA APPROACH

  # Calculating total use of each product
  tidy_AB_dta <- tidy_AB_data %>%
    ECCTools::transform_to_dta(requirement_matrices_list = c("U_feed"),
                               select_dta_observations = FALSE)


  tidy_io_AB_dta <- tidy_AB_data %>%
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

  res_dta <- aggregate_primary_stage_erois(
    .tidy_erois_df = tidy_AB_erois_dta,
    .tidy_iea_df = tidy_AB_dta,
    eroi_calc_method = "dta"
  )

  # Testing
  res_dta %>%
    dplyr::filter(Country == "B") %>% nrow() %>%
    expect_equal(0)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Coal products", Type == "Gross", Boundary == "Feedstock") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(100)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products", Type == "Gross", Boundary == "Feedstock") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(34.97828, tolerance = 1e-4)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Type == "Gross", Boundary == "Feedstock") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(42.89933, tolerance = 1e-4)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Coal products", Type == "Gross", Boundary == "All") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(92.54219032, tolerance = 1e-4)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products", Type == "Gross", Boundary == "All") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(33.27742, tolerance = 1e-4)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Type == "Gross", Boundary == "All") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(40.6743, tolerance = 1e-4)


  # SECOND, WE TEST THE GMA APPROACH

  tidy_AB_data_gma <- tidy_AB_data %>%
    ECCTools::transform_to_gma()

  tidy_io_AB_gma <- tidy_AB_data_gma %>%
    IEATools::prep_psut() %>%
    Recca::calc_io_mats(method_q_calculation = "sum_R_V_cols")

  tidy_AB_erois_gma <- tidy_io_AB_gma %>%
    Recca::calc_E_EIOU() %>%
    Recca::calc_erois() %>%
    EROITools::extract_tidy_product_erois() %>%
    dplyr::mutate(
      Eroi.method = "DTA"
    ) %>%
    # dplyr::relocate(.data[["Eroi.method"]], .after = Year)
    dplyr::relocate(tidyselect::all_of("Eroi.method"), .after = Year)
  
  tidy_AB_data_gma_prepared <- tidy_AB_data_gma %>%
    prepare_gma_for_shares()

  res_gma <- aggregate_primary_stage_erois(
    .tidy_erois_df = tidy_AB_erois_gma,
    .tidy_iea_df = tidy_AB_data_gma_prepared,
    eroi_calc_method = "gma"
  )

  # Testing
  res_gma %>%
    dplyr::filter(Country == "B") %>% nrow() %>%
    expect_equal(0)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Coal products", Type == "Gross", Boundary == "Feedstock") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(100)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products", Type == "Gross", Boundary == "Feedstock") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(34.97828, tolerance = 1e-4)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Type == "Gross", Boundary == "Feedstock") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(42.89933, tolerance = 1e-4)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Coal products", Type == "Gross", Boundary == "All") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(88.4716400, tolerance = 1e-4)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products", Type == "Gross", Boundary == "All") %>%
    magrittr::extract2("Group.eroi") %>%
  expect_equal(33.09121, tolerance = 1e-4)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Type == "Gross", Boundary == "All") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(40.24497, tolerance = 1e-4)
})




test_that("aggregate_final_stage_erois works",{

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


  # FIRST, WE TEST THE DTA APPROACH

  # Calculating total use of each product
  tidy_AB_dta <- tidy_AB_data %>%
    ECCTools::transform_to_dta(requirement_matrices_list = c("U_feed"),
                               select_dta_observations = FALSE)


  tidy_io_AB_dta <- tidy_AB_data %>%
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

  res_dta <- aggregate_final_stage_erois(
    .tidy_erois_df = tidy_AB_erois_dta,
    .tidy_iea_df = tidy_AB_dta,
    eroi_calc_method = "dta"
  )

  # Testing
  res_dta %>%
    dplyr::filter(is.na(Non_Energy_Uses)) %>% nrow() %>%
    expect_equal(0)

  # Coal products:
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Coal products", Energy.stage == "Final (fuel)", Type == "Gross", Boundary == "All") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(10.08925, tolerance = 1e-4)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Coal products", Energy.stage == "Final (fuel)", Type == "Gross", Boundary == "Feedstock") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(10.82847, tolerance = 1e-4)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Coal products", Energy.stage == "Final (electricity)", Type == "Gross", Boundary == "All") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(30.87759814, tolerance = 1e-4)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Coal products", Energy.stage == "Final (electricity)", Type == "Gross", Boundary == "Feedstock") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(32.52233141, tolerance = 1e-4)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Coal products", Energy.stage == "Final (heat)", Type == "Gross", Boundary == "All") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(30.87759814, tolerance = 1e-4)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Coal products", Energy.stage == "Final (heat)", Type == "Gross", Boundary == "Feedstock") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(32.52233141, tolerance = 1e-4)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Coal products", Energy.stage == "Final (fuel+elec+heat)", Type == "Gross", Boundary == "All") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(20.25395, tolerance = 1e-4)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Coal products", Energy.stage == "Final (fuel+elec+heat)", Type == "Gross", Boundary == "Feedstock") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(21.53792, tolerance = 1e-4)
  # Oil products
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Oil products", Energy.stage == "Final (fuel)", Type == "Gross", Boundary == "All") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(9.069165, tolerance = 1e-4)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Oil products", Energy.stage == "Final (electricity)", Type == "Gross", Boundary == "Feedstock") %>%
    nrow() %>%
    expect_equal(0)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Oil products", Energy.stage == "Final (heat)", Type == "Gross", Boundary == "All") %>%
    nrow() %>%
    expect_equal(0)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Oil products", Energy.stage == "Final (fuel+elec+heat)", Type == "Gross", Boundary == "All") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(9.069165, tolerance = 1e-4)
  # Natural gas
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Natural gas", Energy.stage == "Final (fuel)", Type == "Gross", Boundary == "All") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(31.86586777, tolerance = 1e-4)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Natural gas", Energy.stage == "Final (electricity)", Type == "Gross", Boundary == "Feedstock") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(21.46704485, tolerance = 1e-4)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Natural gas", Energy.stage == "Final (heat)", Type == "Gross", Boundary == "All") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(20.52142131, tolerance = 1e-4)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Natural gas", Energy.stage == "Final (fuel+elec+heat)", Type == "Gross", Boundary == "Feedstock") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(26.54106, tolerance = 1e-4)
  # Oil and gas products
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products", Energy.stage == "Final (fuel)", Type == "Gross", Boundary == "All") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(10.13798, tolerance = 1e-4)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products", Energy.stage == "Final (electricity)", Type == "Gross", Boundary == "Feedstock") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(21.46704485, tolerance = 1e-4)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products", Energy.stage == "Final (heat)", Type == "Gross", Boundary == "All") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(20.52142131, tolerance = 1e-4)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products", Energy.stage == "Final (fuel+elec+heat)", Type == "Gross", Boundary == "Feedstock") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(11.56725, tolerance = 1e-4)
  # All fossil fuels
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Energy.stage == "Final (fuel)", Type == "Gross", Boundary == "Feedstock") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(10.89826, tolerance = 1e-4)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Energy.stage == "Final (electricity)", Type == "Gross", Boundary == "All") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(28.28256, tolerance = 1e-4)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Energy.stage == "Final (heat)", Type == "Gross", Boundary == "All") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(28.28256, tolerance = 1e-4)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Energy.stage == "Final (fuel+elec+heat)", Type == "Gross", Boundary == "Feedstock") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(14.27844, tolerance = 1e-4)
  # Couple of checks Country B:
  res_dta %>%
    dplyr::filter(Country == "B", Product.Group == "All fossil fuels", Energy.stage == "Final (fuel)", Type == "Gross", Boundary == "All") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(3.813338, tolerance = 1e-4)
  res_dta %>%
    dplyr::filter(Country == "B", Product.Group == "Natural gas", Energy.stage == "Final (fuel)", Type == "Gross", Boundary == "Feedstock") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(Inf, tolerance = 1e-4)


  # SECOND, WE TEST THE GMA APPROACH

  tidy_AB_data_gma <- tidy_AB_data %>%
    ECCTools::transform_to_gma()

  tidy_io_AB_gma <- tidy_AB_data_gma %>%
    IEATools::prep_psut() %>%
    Recca::calc_io_mats(method_q_calculation = "sum_R_V_cols")

  tidy_AB_erois_gma <- tidy_io_AB_gma %>%
    Recca::calc_E_EIOU() %>%
    Recca::calc_erois() %>%
    EROITools::extract_tidy_product_erois() %>%
    dplyr::mutate(
      Eroi.method = "DTA"
    ) %>%
    dplyr::relocate(.data[["Eroi.method"]], .after = Year)

  tidy_AB_data_gma_prepared <- tidy_AB_data_gma %>%
    prepare_gma_for_shares()

  res_gma <- aggregate_final_stage_erois(
    .tidy_erois_df = tidy_AB_erois_gma,
    .tidy_iea_df = tidy_AB_data_gma_prepared,
    eroi_calc_method = "gma"
  )


  # Coal products:
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Coal products", Energy.stage == "Final (fuel)", Type == "Gross", Boundary == "All") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(2.9828, tolerance = 1e-4)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Coal products", Energy.stage == "Final (electricity)", Type == "Gross", Boundary == "Feedstock") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(26.6218962, tolerance = 1e-4)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Coal products", Energy.stage == "Final (heat)", Type == "Gross", Boundary == "All") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(24.1777691, tolerance = 1e-4)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Coal products", Energy.stage == "Final (fuel+elec+heat)", Type == "Gross", Boundary == "All") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(8.607592, tolerance = 1e-4)


  # Oil products
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Oil products", Energy.stage == "Final (fuel)", Type == "Gross", Boundary == "All") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(9.0404289, tolerance = 1e-4)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Oil products", Energy.stage == "Final (electricity)", Type == "Gross", Boundary == "Feedstock") %>%
    nrow() %>%
    expect_equal(0)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Oil products", Energy.stage == "Final (heat)", Type == "Gross", Boundary == "All") %>%
    nrow() %>%
    expect_equal(0)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Oil products", Energy.stage == "Final (fuel+elec+heat)", Type == "Gross", Boundary == "All") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(9.0404289, tolerance = 1e-4)
  # Natural gas
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Natural gas", Energy.stage == "Final (fuel)", Type == "Gross", Boundary == "All") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(31.6920549, tolerance = 1e-4)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Natural gas", Energy.stage == "Final (electricity)", Type == "Gross", Boundary == "Feedstock") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(21.4670449, tolerance = 1e-4)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Natural gas", Energy.stage == "Final (heat)", Type == "Gross", Boundary == "All") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(20.3968628, tolerance = 1e-4)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Natural gas", Energy.stage == "Final (fuel+elec+heat)", Type == "Gross", Boundary == "Feedstock") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(26.54106, tolerance = 1e-4)
  # Oil and gas products
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products", Energy.stage == "Final (fuel)", Type == "Gross", Boundary == "All") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(10.10477, tolerance = 1e-4)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products", Energy.stage == "Final (electricity)", Type == "Gross", Boundary == "Feedstock") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(21.4670449, tolerance = 1e-4)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products", Energy.stage == "Final (heat)", Type == "Gross", Boundary == "All") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(20.3968628, tolerance = 1e-4)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products", Energy.stage == "Final (fuel+elec+heat)", Type == "Gross", Boundary == "Feedstock") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(11.56725, tolerance = 1e-4)
  # All fossil fuels
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Energy.stage == "Final (fuel)", Type == "Gross", Boundary == "Feedstock") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(8.079494, tolerance = 1e-4)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Energy.stage == "Final (electricity)", Type == "Gross", Boundary == "All") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(23.38949, tolerance = 1e-4)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Energy.stage == "Final (heat)", Type == "Gross", Boundary == "All") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(23.38949, tolerance = 1e-4)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Energy.stage == "Final (fuel+elec+heat)", Type == "Gross", Boundary == "Feedstock") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(10.84909, tolerance = 1e-4)


  # Couple of checks Country B:
  res_gma %>%
    dplyr::filter(Country == "B", Product.Group == "All fossil fuels", Energy.stage == "Final (fuel)", Type == "Gross", Boundary == "All") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(3.306948, tolerance = 1e-4)
  res_gma %>%
    dplyr::filter(Country == "B", Product.Group == "Natural gas", Energy.stage == "Final (fuel)", Type == "Gross", Boundary == "Feedstock") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(33.5426180, tolerance = 1e-4)
})



test_that("aggregate_useful_stage_erois works",{

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
    dplyr::relocate(.data[["Eroi.method"]], .after = Year)

  # Pushing to tidy useful stage EROIs
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

  # Calculating aggregated EROIs:
  res_dta <- aggregate_useful_stage_erois(
    .tidy_erois_df = tidy_useful_erois_dta,
    .tidy_iea_df = tidy_AB_dta,
    eroi_calc_method = "dta"
  )

  # Testing:
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Coal products", Energy.stage == "Useful (fuel)", Boundary == "All", Type == "Gross") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(1.542919, tolerance = 1e-4)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Oil products", Energy.stage == "Useful (fuel)", Boundary == "All", Type == "Gross") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(4.658814, tolerance = 1e-4)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products", Energy.stage == "Useful (fuel)", Boundary == "All", Type == "Gross") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(5.227826, tolerance = 1e-4)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Natural gas", Energy.stage == "Useful (fuel)", Boundary == "Feedstock", Type == "Gross") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(18.75901967, tolerance = 1e-4)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Energy.stage == "Useful (fuel)", Boundary == "Feedstock", Type == "Gross") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(4.019568, tolerance = 1e-4)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Coal products", Energy.stage == "Useful (electricity)", Boundary == "Feedstock", Type == "Gross") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(9.99760558, tolerance = 1e-4)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Oil products", Energy.stage == "Useful (electricity)", Boundary == "All", Type == "Gross") %>%
    nrow() %>% expect_equal(0)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products", Energy.stage == "Useful (electricity)", Boundary == "All", Type == "Gross") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(6.95448166, tolerance = 1e-4)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Natural gas", Energy.stage == "Useful (electricity)", Boundary == "All", Type == "Gross") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(6.95448166, tolerance = 1e-4)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Energy.stage == "Useful (electricity)", Boundary == "Feedstock", Type == "Gross") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(9.360662, tolerance = 1e-4)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Coal products", Energy.stage == "Useful (heat)", Boundary == "All", Type == "Gross") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(12.408219993, tolerance = 1e-4)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Oil products", Energy.stage == "Useful (heat)", Boundary == "All", Type == "Gross") %>%
    nrow() %>%
    expect_equal(0)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products", Energy.stage == "Useful (heat)", Boundary == "All", Type == "Gross") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(8.89261590, tolerance = 1e-4)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Natural gas", Energy.stage == "Useful (heat)", Boundary == "Feedstock", Type == "Gross") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(9.30238610, tolerance = 1e-4)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Energy.stage == "Useful (heat)", Boundary == "All", Type == "Gross") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(11.57614, tolerance = 1e-4)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Coal products", Energy.stage == "Useful (fuel+elec+heat)", Boundary == "All", Type == "Gross") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(4.124755, tolerance = 1e-4)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Oil products", Energy.stage == "Useful (fuel+elec+heat)", Boundary == "All", Type == "Gross") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(4.658814, tolerance = 1e-4)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products", Energy.stage == "Useful (fuel+elec+heat)", Boundary == "Feedstock", Type == "Gross") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(5.785075, tolerance = 1e-4)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "Natural gas", Energy.stage == "Useful (fuel+elec+heat)", Boundary == "All", Type == "Gross") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(10.37702, tolerance = 1e-4)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Energy.stage == "Useful (fuel+elec+heat)", Boundary == "All", Type == "Gross") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(4.786732, tolerance = 1e-4)

  # Couple of checks Country B:
  res_dta %>%
    dplyr::filter(Country == "B", Product.Group == "All fossil fuels", Energy.stage == "Useful (fuel)", Type == "Gross", Boundary == "All") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(2.85176, tolerance = 1e-4)
  res_dta %>%
    dplyr::filter(Country == "B", Product.Group == "Natural gas", Energy.stage == "Useful (fuel)", Type == "Gross", Boundary == "Feedstock") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(Inf, tolerance = 1e-4)


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
    dplyr::relocate(.data[["Eroi.method"]], .after = Year)

  # Prepare GMA data frame for shares calculations
  tidy_AB_data_gma_prepared <- tidy_AB_data_gma %>%
    prepare_gma_for_shares()

  # Pushing to tidy useful stage EROIs
  length_to_use <- tidy_AB_erois_gma %>%
    dplyr::select(Country, Method, Energy.type, Year, Product) %>%
    dplyr::distinct() %>%
    nrow()

  tidy_FU_efficiencies_gma <- tidy_AB_erois_gma %>%
    dplyr::select(Country, Method, Energy.type, Year, Product) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      Average_Efficiency_Global = seq(0.15, 1, 0.85/(length_to_use-1))
    )

  tidy_useful_erois_gma <- tidy_AB_erois_gma %>%
    dplyr::left_join(tidy_FU_efficiencies_gma,
                     by = c("Country", "Method", "Energy.type", "Year", "Product")) %>%
    dplyr::select(-Country) %>%
    tidyr::expand_grid(Country = c("A", "B")) %>%
    dplyr::mutate(
      Useful_Stage_EROI = Average_Efficiency_Global * EROI
    ) %>%
    dplyr::filter(! is.na(Useful_Stage_EROI))


  # Calculating aggregated EROIs:
  res_gma <- aggregate_useful_stage_erois(
    .tidy_erois_df = tidy_useful_erois_gma,
    .tidy_iea_df = tidy_AB_data_gma_prepared,
    eroi_calc_method = "gma"
  )


  # Testing:
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Coal products", Energy.stage == "Useful (fuel)", Boundary == "All", Type == "Gross") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(0.5107176, tolerance = 1e-4)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Oil products", Energy.stage == "Useful (fuel)", Boundary == "All", Type == "Gross") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(5.054887, tolerance = 1e-4)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products", Energy.stage == "Useful (fuel)", Boundary == "Feedstock", Type == "Gross") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(6.12621, tolerance = 1e-4)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Natural gas", Energy.stage == "Useful (fuel)", Boundary == "All", Type == "Gross") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(19.3453585, tolerance = 1e-4)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Energy.stage == "Useful (fuel)", Boundary == "Feedstock", Type == "Gross") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(2.42007, tolerance = 1e-4)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Coal products", Energy.stage == "Useful (electricity)", Boundary == "All", Type == "Gross") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(7.9081453, tolerance = 1e-4)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Oil products", Energy.stage == "Useful (electricity)", Boundary == "All", Type == "Gross") %>%
    nrow() %>% expect_equal(0)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products", Energy.stage == "Useful (electricity)", Boundary == "All", Type == "Gross") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(7.3938628, tolerance = 1e-4)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Natural gas", Energy.stage == "Useful (electricity)", Boundary == "All", Type == "Gross") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(7.3938628, tolerance = 1e-4)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Energy.stage == "Useful (electricity)", Boundary == "All", Type == "Gross") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(7.809392, tolerance = 1e-4)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Coal products", Energy.stage == "Useful (heat)", Boundary == "Feedstock", Type == "Gross") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(11.5361550, tolerance = 1e-4)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Oil products", Energy.stage == "Useful (heat)", Boundary == "All", Type == "Gross") %>%
    nrow() %>%
    expect_equal(0)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products", Energy.stage == "Useful (heat)", Boundary == "All", Type == "Gross") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(9.5610295, tolerance = 1e-4)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Natural gas", Energy.stage == "Useful (heat)", Boundary == "All", Type == "Gross") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(9.5610295, tolerance = 1e-4)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Energy.stage == "Useful (heat)", Boundary == "Feedstock", Type == "Gross") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(11.237, tolerance = 1e-4)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Coal products", Energy.stage == "Useful (fuel+elec+heat)", Boundary == "All", Type == "Gross") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(1.691032, tolerance = 1e-4)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Oil products", Energy.stage == "Useful (fuel+elec+heat)", Boundary == "All", Type == "Gross") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(5.054887, tolerance = 1e-4)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products", Energy.stage == "Useful (fuel+elec+heat)", Boundary == "All", Type == "Gross") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(5.835724, tolerance = 1e-4)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "Natural gas", Energy.stage == "Useful (fuel+elec+heat)", Boundary == "Feedstock", Type == "Gross") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(11.71006, tolerance = 1e-4)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Energy.stage == "Useful (fuel+elec+heat)", Boundary == "All", Type == "Gross") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(2.910402, tolerance = 1e-4)

  # Couple of checks Country B:
  res_gma %>%
    dplyr::filter(Country == "B", Product.Group == "All fossil fuels", Energy.stage == "Useful (fuel)", Type == "Gross", Boundary == "All") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(2.642866, tolerance = 1e-4)
  res_gma %>%
    dplyr::filter(Country == "B", Product.Group == "Natural gas", Energy.stage == "Useful (fuel)", Type == "Gross", Boundary == "All") %>%
    magrittr::extract2("Group.eroi") %>%
    expect_equal(19.3453585, tolerance = 1e-4)
})

