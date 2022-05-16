
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
    IEATools::add_psut_matnames() %>%
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
  
  tidy_useful_erois_dta_with_function <- push_to_useful_erois(
    .tidy_io_erois = tidy_AB_erois_dta,
    tidy_FU_efficiencies = tidy_FU_efficiencies_dta,
    eroi_calc_method = "dta"
  )
  
  
  
  
  
  
  
  
  
  
  
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
  # tidy_AB_data_gma_prepared <- tidy_AB_data_gma %>% 
  #   prepare_gma_for_shares()
  
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
    dplyr::mutate(
      Useful_Stage_EROI = Average_Efficiency_Global * EROI
    ) %>% 
    dplyr::filter(! is.na(Useful_Stage_EROI))
  
  tidy_useful_erois_gma_with_function <- push_to_useful_erois(
    .tidy_io_erois = tidy_AB_erois_gma,
    tidy_FU_efficiencies = tidy_FU_efficiencies_gma,
    eroi_calc_method = "gma"
  )
})