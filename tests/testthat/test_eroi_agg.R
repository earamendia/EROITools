
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
    dplyr::relocate(.data[["Eroi.method"]], .after = Year)
  
  tidy_AB_data_gma_prepared <- tidy_AB_data_gma %>% 
    dplyr::mutate(
      Country = stringr::str_extract(Flow, "\\{.*\\}") %>% 
        stringr::str_remove("\\{") %>% 
        stringr::str_remove("\\}"),
      Flow = stringr::str_remove(Flow, "\\{.*\\}_"),
      product_without_origin = stringr::str_remove(Product, "\\{.*\\}_"),
    )
  
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
  
  
  
  
  
  
})







test_that("aggregate_useful_stage_erois works",{
  
  
  
  
  
  
})