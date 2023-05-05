

test_that("calc_fec_from_ff_as_elec_by_group",{

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
  
  res_dta <- tidy_AB_dta %>%
    calc_fec_from_ff_as_elec_by_group()
  
  # Testing
  res_dta %>% 
    dplyr::filter(! matnames %in% c("Y", "U_EIOU")) %>% 
    nrow() %>% 
    expect_equal(0)
  
  # Country A, Iron and steel:
  res_dta %>% 
    dplyr::filter(Country == "A", Flow == "Iron and steel", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(2920)
  res_dta %>% 
    dplyr::filter(Country == "A", Flow == "Iron and steel", Product.Group == "Natural gas") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(530.9091, tolerance = 1e-4)
  res_dta %>% 
    dplyr::filter(Country == "A", Flow == "Iron and steel", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(530.9091, tolerance = 1e-4)
  res_dta %>% 
    dplyr::filter(Country == "A", Flow == "Iron and steel", Product.Group == "Coal products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(2389.091, tolerance = 1e-4)
  res_dta %>% 
    dplyr::filter(Country == "A", Flow == "Iron and steel", Product.Group == "Oil products") %>% 
    nrow() %>% 
    expect_equal(0)
  # Country A, Coal mines
  res_dta %>% 
    dplyr::filter(Country == "A", Flow == "Coal mines", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(20)
  res_dta %>% 
    dplyr::filter(Country == "A", Flow == "Coal mines", Product.Group == "Natural gas") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(3.636364, tolerance = 1e-4)
  res_dta %>% 
    dplyr::filter(Country == "A", Flow == "Coal mines", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(3.636364, tolerance = 1e-4)
  res_dta %>% 
    dplyr::filter(Country == "A", Flow == "Coal mines", Product.Group == "Coal products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(16.36364, tolerance = 1e-4)
  res_dta %>% 
    dplyr::filter(Country == "A", Flow == "Coal mines", Product.Group == "Oil products") %>% 
    nrow() %>% 
    expect_equal(0)
  # Country B, Iron and steel
  res_dta %>% 
    dplyr::filter(Country == "B", Flow == "Iron and steel", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(300)
  res_dta %>% 
    dplyr::filter(Country == "B", Flow == "Iron and steel", Product.Group == "Natural gas") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(264.7059, tolerance = 1e-4)
  res_dta %>% 
    dplyr::filter(Country == "B", Flow == "Iron and steel", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(264.7059, tolerance = 1e-4)
  res_dta %>% 
    dplyr::filter(Country == "B", Flow == "Iron and steel", Product.Group == "Coal products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(35.29412, tolerance = 1e-4)
  res_dta %>% 
    dplyr::filter(Country == "B", Flow == "Iron and steel", Product.Group == "Oil products") %>% 
    nrow() %>% 
    expect_equal(0)
  
  
  # SECOND, WE TEST THE GMA APPROACH
  
  tidy_AB_data_gma <- tidy_AB_data %>%
    ECCTools::transform_to_gma()
  
  tidy_AB_data_gma_prepared <- tidy_AB_data_gma %>% 
    prepare_gma_for_shares()
  
  res_gma <- tidy_AB_data_gma_prepared %>% 
    calc_fec_from_ff_as_elec_by_group()
  
  # Testing
  # Country A, Iron and steel:
  res_gma %>% 
    dplyr::filter(Country == "A", Flow == "Iron and steel", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(2920)
  res_gma %>% 
    dplyr::filter(Country == "A", Flow == "Iron and steel", Product.Group == "Natural gas") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(530.9091, tolerance = 1e-4)
  res_gma %>% 
    dplyr::filter(Country == "A", Flow == "Iron and steel", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(530.9091, tolerance = 1e-4)
  res_gma %>% 
    dplyr::filter(Country == "A", Flow == "Iron and steel", Product.Group == "Coal products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(2389.091, tolerance = 1e-4)
  res_gma %>% 
    dplyr::filter(Country == "A", Flow == "Iron and steel", Product.Group == "Oil products") %>% 
    nrow() %>% 
    expect_equal(0)
  # Country A, Coal mines
  res_gma %>% 
    dplyr::filter(Country == "A", Flow == "Coal mines", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(20)
  res_gma %>% 
    dplyr::filter(Country == "A", Flow == "Coal mines", Product.Group == "Natural gas") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(3.636364, tolerance = 1e-4)
  res_gma %>% 
    dplyr::filter(Country == "A", Flow == "Coal mines", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(3.636364, tolerance = 1e-4)
  res_gma %>% 
    dplyr::filter(Country == "A", Flow == "Coal mines", Product.Group == "Coal products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(16.36364, tolerance = 1e-4)
  res_gma %>% 
    dplyr::filter(Country == "A", Flow == "Coal mines", Product.Group == "Oil products") %>% 
    nrow() %>% 
    expect_equal(0)
  # Country B, Iron and steel
  res_gma %>% 
    dplyr::filter(Country == "B", Flow == "Iron and steel", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(300)
  res_gma %>% 
    dplyr::filter(Country == "B", Flow == "Iron and steel", Product.Group == "Natural gas") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(264.7059, tolerance = 1e-4)
  res_gma %>% 
    dplyr::filter(Country == "B", Flow == "Iron and steel", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(264.7059, tolerance = 1e-4)
  res_gma %>% 
    dplyr::filter(Country == "B", Flow == "Iron and steel", Product.Group == "Coal products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(35.29412, tolerance = 1e-4)
  res_gma %>% 
    dplyr::filter(Country == "B", Flow == "Iron and steel", Product.Group == "Oil products") %>% 
    nrow() %>% 
    expect_equal(0)
})


test_that("calc_share_elec_supply_by_ff_group",{

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
  
  res_dta <- tidy_AB_dta %>%
    calc_share_elec_supply_by_ff_group()
  
  # Testing
  res_dta %>% 
    dplyr::filter(Product.Group == "Oil products") %>% 
    nrow() %>% 
    expect_equal(0)
  res_dta %>% 
    dplyr::filter(Product.Group == "All fossil fuels") %>% 
    dplyr::ungroup() %>% dplyr::select(Share) %>% dplyr::pull() %>%
    expect_equal(c(1,1))
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Coal products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.8181818, tolerance = 1e-5)
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.1818182, tolerance = 1e-5)
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Natural gas") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.1818182, tolerance = 1e-5)
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "Coal products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.1176471, tolerance = 1e-5)
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.8823529, tolerance = 1e-5)
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "Natural gas") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.8823529, tolerance = 1e-5)
  
  
  # SECOND, WE TEST THE GMA APPROACH
  
  tidy_AB_data_gma <- tidy_AB_data %>%
    ECCTools::transform_to_gma()
  
  tidy_AB_data_gma_prepared <- tidy_AB_data_gma %>% 
    prepare_gma_for_shares()
  
  res_gma <- tidy_AB_data_gma_prepared %>% 
    calc_share_elec_supply_by_ff_group()
  
  # Testing
  res_gma %>% 
    dplyr::filter(Product.Group == "Oil products") %>% 
    nrow() %>% 
    expect_equal(0)
  res_gma %>% 
    dplyr::filter(Product.Group == "All fossil fuels") %>% 
    dplyr::ungroup() %>% dplyr::select(Share) %>% dplyr::pull() %>%
    expect_equal(c(1,1))
  res_gma %>% 
    dplyr::filter(Country == "A", Product.Group == "Coal products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.8181818, tolerance = 1e-5)
  res_gma %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.1818182, tolerance = 1e-5)
  res_gma %>% 
    dplyr::filter(Country == "A", Product.Group == "Natural gas") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.1818182, tolerance = 1e-5)
  res_gma %>% 
    dplyr::filter(Country == "B", Product.Group == "Coal products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.1176471, tolerance = 1e-5)
  res_gma %>% 
    dplyr::filter(Country == "B", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.8823529, tolerance = 1e-5)
  res_gma %>% 
    dplyr::filter(Country == "B", Product.Group == "Natural gas") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.8823529, tolerance = 1e-5)
})


test_that("calc_fec_from_ff_as_heat_by_group",{

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
  
  res_dta <- tidy_AB_dta %>%
    calc_fec_from_ff_as_heat_by_group()
  
  # Testing; first country A iron and steel
  res_dta %>% 
    dplyr::filter(Country == "A", Flow == "Iron and steel", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("E.dot") %>%
    expect_equal(40)
  res_dta %>% 
    dplyr::filter(Country == "A", Flow == "Iron and steel", Product.Group == "Oil products") %>% 
    nrow() %>% 
    expect_equal(0)
  res_dta %>% 
    dplyr::filter(Country == "A", Flow == "Iron and steel", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(7.272727, tolerance = 1e-5)
  res_dta %>% 
    dplyr::filter(Country == "A", Flow == "Iron and steel", Product.Group == "Natural gas") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(7.272727, tolerance = 1e-5)
  res_dta %>% 
    dplyr::filter(Country == "A", Flow == "Iron and steel", Product.Group == "Coal products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(32.72727, tolerance = 1e-5)
  # Second country A, oil refineries
  res_dta %>% 
    dplyr::filter(Country == "A", Flow == "Oil refineries", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(80)
  res_dta %>% 
    dplyr::filter(Country == "A", Flow == "Oil refineries", Product.Group == "Oil products") %>% 
    nrow() %>% 
    expect_equal(0)
  res_dta %>% 
    dplyr::filter(Country == "A", Flow == "Oil refineries", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(14.54545, tolerance = 1e-4)
  res_dta %>% 
    dplyr::filter(Country == "A", Flow == "Oil refineries", Product.Group == "Natural gas") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(14.54545, tolerance = 1e-4)
  res_dta %>% 
    dplyr::filter(Country == "A", Flow == "Oil refineries", Product.Group == "Coal products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(65.45455, tolerance = 1e-4)
  # Third country B, oil refineries
  res_dta %>% 
    dplyr::filter(Country == "B", Flow == "Oil refineries", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(60)
  res_dta %>% 
    dplyr::filter(Country == "B", Flow == "Oil refineries", Product.Group == "Oil products") %>% 
    nrow() %>% 
    expect_equal(0)
  res_dta %>% 
    dplyr::filter(Country == "B", Flow == "Oil refineries", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(52.94118, tolerance = 1e-5)
  res_dta %>% 
    dplyr::filter(Country == "B", Flow == "Oil refineries", Product.Group == "Natural gas") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(52.94118, tolerance = 1e-5)
  res_dta %>% 
    dplyr::filter(Country == "B", Flow == "Oil refineries", Product.Group == "Coal products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(7.058824, tolerance = 1e-5)
  
  
  # SECOND, WE TEST THE GMA APPROACH
  
  tidy_AB_data_gma <- tidy_AB_data %>%
    ECCTools::transform_to_gma()
  
  tidy_AB_data_gma_prepared <- tidy_AB_data_gma %>% 
    prepare_gma_for_shares()
  
  res_gma <- tidy_AB_data_gma_prepared %>% 
    calc_fec_from_ff_as_heat_by_group()
  
  # Testing; first country A iron and steel
  res_gma %>% 
    dplyr::filter(Country == "A", Flow == "Iron and steel", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(40)
  res_gma %>% 
    dplyr::filter(Country == "A", Flow == "Iron and steel", Product.Group == "Oil products") %>% 
    nrow() %>% 
    expect_equal(0)
  res_gma %>% 
    dplyr::filter(Country == "A", Flow == "Iron and steel", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(7.272727, tolerance = 1e-5)
  res_gma %>% 
    dplyr::filter(Country == "A", Flow == "Iron and steel", Product.Group == "Natural gas") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(7.272727, tolerance = 1e-5)
  res_gma %>% 
    dplyr::filter(Country == "A", Flow == "Iron and steel", Product.Group == "Coal products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(32.72727, tolerance = 1e-5)
  # Second country A, oil refineries
  res_gma %>% 
    dplyr::filter(Country == "A", Flow == "Oil refineries", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(80)
  res_gma %>% 
    dplyr::filter(Country == "A", Flow == "Oil refineries", Product.Group == "Oil products") %>% 
    nrow() %>% 
    expect_equal(0)
  res_gma %>% 
    dplyr::filter(Country == "A", Flow == "Oil refineries", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(14.54545, tolerance = 1e-4)
  res_gma %>% 
    dplyr::filter(Country == "A", Flow == "Oil refineries", Product.Group == "Natural gas") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(14.54545, tolerance = 1e4)
  res_gma %>% 
    dplyr::filter(Country == "A", Flow == "Oil refineries", Product.Group == "Coal products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(65.45455, tolerance = 1e-4)
  # Third country B, oil refineries
  res_gma %>% 
    dplyr::filter(Country == "B", Flow == "Oil refineries", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(60)
  res_gma %>% 
    dplyr::filter(Country == "B", Flow == "Oil refineries", Product.Group == "Oil products") %>% 
    nrow() %>% 
    expect_equal(0)
  res_gma %>% 
    dplyr::filter(Country == "B", Flow == "Oil refineries", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(52.94118, tolerance = 1e-5)
  res_gma %>% 
    dplyr::filter(Country == "B", Flow == "Oil refineries", Product.Group == "Natural gas") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(52.94118, tolerance = 1e-5)
  res_gma %>% 
    dplyr::filter(Country == "B", Flow == "Oil refineries", Product.Group == "Coal products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(7.058824, tolerance = 1e-5)
})


test_that("calc_share_heat_supply_by_ff_group",{

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
  
  res_dta <- tidy_AB_dta %>%
    calc_share_heat_supply_by_ff_group()

  # Country A
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.8)
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil products") %>% 
    nrow() %>% 
    expect_equal(0)
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.1454545, tolerance = 1e-5)
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Natural gas") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.1454545, tolerance = 1e-5)
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Coal products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.6545455, tolerance = 1e-5)
  
  # Country B
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.6)
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "Oil products") %>% 
    nrow() %>% 
    expect_equal(0)
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.5294118, tolerance = 1e-5)
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "Natural gas") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.5294118, tolerance = 1e-5)
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "Coal products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.07058824, tolerance = 1e-5)
  
  
  # SECOND, WE TEST THE GMA APPROACH
  
  tidy_AB_data_gma <- tidy_AB_data %>%
    ECCTools::transform_to_gma()
  
  tidy_AB_data_gma_prepared <- tidy_AB_data_gma %>% 
    prepare_gma_for_shares()
  
  res_gma <- tidy_AB_data_gma_prepared %>% 
    calc_share_heat_supply_by_ff_group()
  
  # Country A
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.8)
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil products") %>% 
    nrow() %>% 
    expect_equal(0)
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.1454545, tolerance = 1e-4)
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Natural gas") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.1454545, tolerance = 1e-4)
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Coal products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.6545455, tolerance = 1e-4)
  
  # Country B
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.6)
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "Oil products") %>% 
    nrow() %>% 
    expect_equal(0)
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.5294118, tolerance = 1e-5)
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "Natural gas") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.5294118, tolerance = 1e-5)
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "Coal products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.07058824, tolerance = 1e-5)
})




test_that("calc_fec_from_ff_by_group",{

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
  
  res_dta <- tidy_AB_dta %>%
    calc_fec_from_ff_by_group()

  # Note that heat coming out of oil refineries is defined as Heat [from Other processes]!!! so not coming from FF. Hence the values.
  # Country A:
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(9100)
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Coal products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(3731.81818, tolerance = 1e-4)
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Natural gas") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(1318.181818, tolerance = 1e-4)
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(1318.181818+4050, tolerance = 1e-4)
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(4050)

  # Country B:
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(5250)
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "Coal products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(750+152.9412, tolerance = 1e-4)
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "Natural gas") %>%
    magrittr::extract2("E.dot") %>% 
    expect_equal(2147.059, tolerance = 1e-4)
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(2200+2147.059, tolerance = 1e-4)
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "Oil products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(2200)
  
  # SECOND, WE TEST THE GMA APPROACH
  
  tidy_AB_data_gma <- tidy_AB_data %>%
    ECCTools::transform_to_gma()
  
  tidy_AB_data_gma_prepared <- tidy_AB_data_gma %>% 
    prepare_gma_for_shares()
  
  res_gma <- tidy_AB_data_gma_prepared %>% 
    calc_fec_from_ff_by_group()
  
  # Note that heat coming out of oil refineries is defined as Heat [from Other processes]!!! so not coming from FF. Hence the values.
  # Country A:
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(9100)
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Coal products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(3731.81818, tolerance = 1e-4)
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Natural gas") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(1318.181818, tolerance = 1e-4)
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(1318.181818+4050, tolerance = 1e-4)
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(4050)
  
  # Country B:
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(5250)
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "Coal products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(750+152.9412, tolerance = 1e-4)
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "Natural gas") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(2147.059, tolerance = 1e-4)
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(2200+2147.059, tolerance = 1e-4)
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "Oil products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(2200)
})



test_that("calc_fec_from_ff_as_fuel_by_group",{

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
  
  res_dta <- tidy_AB_dta %>%
    calc_fec_from_ff_as_fuel_by_group()
  
  # Country A:
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(5700)
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Coal products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(950)
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Natural gas") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(700)
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(4750)
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(4050)
  
  # Country B:
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(3950)
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "Coal products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(750)
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "Natural gas") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(1000)
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(3200)
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "Oil products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(2200)
  
  
  # Quick test for including non-energy uses
  res_dta_non_energy_uses <- tidy_AB_dta %>%
    calc_fec_from_ff_as_fuel_by_group(include_non_energy_uses = TRUE)
  
  # Country A:
  res_dta_non_energy_uses %>% 
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(5700)
  res_dta_non_energy_uses %>% 
    dplyr::filter(Country == "A", Product.Group == "Coal products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(950)
  res_dta_non_energy_uses %>% 
    dplyr::filter(Country == "A", Product.Group == "Natural gas") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(700)
  res_dta_non_energy_uses %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(4750)
  res_dta_non_energy_uses %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(4050)
  
  
  # SECOND, WE TEST THE GMA APPROACH
  
  tidy_AB_data_gma <- tidy_AB_data %>%
    ECCTools::transform_to_gma()
  
  tidy_AB_data_gma_prepared <- tidy_AB_data_gma %>% 
    prepare_gma_for_shares()
  
  res_gma <- tidy_AB_data_gma_prepared %>% 
    calc_fec_from_ff_as_fuel_by_group()
  
  # Country A:
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(5700)
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Coal products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(950)
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Natural gas") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(700)
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(4750)
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(4050)
  
  # Country B:
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(3950)
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "Coal products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(750)
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "Natural gas") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(1000)
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(3200)
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "Oil products") %>% 
    magrittr::extract2("E.dot") %>% 
    expect_equal(2200)
})




test_that("calc_share_elec_supply_by_ff_group works with losses",{
  
  # Path to dummy AB data
  A_B_path <- system.file("extdata/A_B_data_full_2018_format_stat_diffs_stock_changes.csv", package = "EROITools")
  
  # Loading data
  tidy_AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df(unit_val = "ktoe") %>%
    tibble::add_row(
      Country = "A",
      Method = "PCM",
      Energy.type = "E",
      Last.stage = "Final",
      Year = 2018,
      Ledger.side = "Supply",
      Flow.aggregation.point = "TFC compare",
      Flow = "Losses",
      Product = "Electricity",
      Unit = "ktoe",
      E.dot = -50
    ) %>%
    tibble::add_row(
      Country = "A",
      Method = "PCM",
      Energy.type = "E",
      Last.stage = "Final",
      Year = 2018,
      Ledger.side = "Supply",
      Flow.aggregation.point = "Transformation processes",
      Flow = "A balancing industry",
      Product = "Electricity",
      Unit = "ktoe",
      E.dot = 50
    ) %>%
    IEATools::specify_all() %>%
    ECCTools::specify_elect_heat_renewables() %>%
    ECCTools::specify_elect_heat_fossil_fuels() %>%
    ECCTools::specify_elect_heat_nuclear() %>%
    ECCTools::specify_other_elec_heat_production() %>%
    ECCTools::specify_elect_heat_markets() %>%
    IEATools::add_psut_matnames(R_includes_all_exogenous_flows = FALSE) %>%
    ECCTools::stat_diffs_to_balancing() %>%
    ECCTools::stock_changes_to_balancing() %>% 
    ECCTools::specify_losses_as_industry()
  
  
  # FIRST, WE TEST THE DTA APPROACH
  
  # Calculating total use of each product
  tidy_AB_dta <- tidy_AB_data %>%
    ECCTools::transform_to_dta(requirement_matrices_list = c("U_feed"),
                               select_dta_observations = FALSE)
  
  res_dta <- tidy_AB_dta %>%
    calc_share_elec_supply_by_ff_group()
  
  # Testing
  res_dta %>% 
    dplyr::filter(Product.Group == "Oil products") %>% 
    nrow() %>% 
    expect_equal(0)
  res_dta %>% 
    dplyr::filter(Product.Group == "All fossil fuels") %>% 
    dplyr::ungroup() %>% dplyr::select(Share) %>% dplyr::pull() %>%
    expect_equal(c(0.9846154,1), tolerance = 1e-5)
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Coal products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.8055944, tolerance = 1e-5)
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.179021, tolerance = 1e-5)
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Natural gas") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.179021, tolerance = 1e-5)
  # Useless because no change in country B's ECC
  # res_dta %>% 
  #   dplyr::filter(Country == "B", Product.Group == "Coal products") %>% 
  #   magrittr::extract2("Share") %>% 
  #   expect_equal(0.1176471, tolerance = 1e-5)
  # res_dta %>% 
  #   dplyr::filter(Country == "B", Product.Group == "Oil and gas products") %>% 
  #   magrittr::extract2("Share") %>% 
  #   expect_equal(0.8823529, tolerance = 1e-5)
  # res_dta %>% 
  #   dplyr::filter(Country == "B", Product.Group == "Natural gas") %>% 
  #   magrittr::extract2("Share") %>% 
  #   expect_equal(0.8823529, tolerance = 1e-5)
  
  # Only A because this is the country that changes
  # This time we add losses without increasing supply
  tidy_A_data_bis <- A_B_path %>%
    IEATools::load_tidy_iea_df(unit_val = "ktoe") %>%
    dplyr::filter(Country == "A") %>% 
    tibble::add_row(
      Country = "A",
      Method = "PCM",
      Energy.type = "E",
      Last.stage = "Final",
      Year = 2018,
      Ledger.side = "Supply",
      Flow.aggregation.point = "TFC compare",
      Flow = "Losses",
      Product = "Electricity",
      Unit = "ktoe",
      E.dot = -50
    ) %>%
    # reducing consumption as 50 goes to losses
    dplyr::mutate(
      E.dot = dplyr::case_when(
        (Flow == "Iron and steel" & Product == "Electricity") ~ 2870,
        TRUE ~ E.dot
      )
    ) %>% 
    IEATools::specify_all() %>%
    ECCTools::specify_elect_heat_renewables() %>%
    ECCTools::specify_elect_heat_fossil_fuels() %>%
    ECCTools::specify_elect_heat_nuclear() %>%
    ECCTools::specify_other_elec_heat_production() %>%
    ECCTools::specify_elect_heat_markets() %>%
    IEATools::add_psut_matnames(R_includes_all_exogenous_flows = FALSE) %>%
    ECCTools::stat_diffs_to_balancing() %>%
    ECCTools::stock_changes_to_balancing() %>% 
    ECCTools::specify_losses_as_industry()
  
  # Calcs
  tidy_A_dta_bis <- tidy_A_data_bis %>%
    ECCTools::transform_to_dta(requirement_matrices_list = c("U_feed"),
                               select_dta_observations = FALSE)
  
  res_dta_bis <- tidy_A_dta_bis %>%
    calc_share_elec_supply_by_ff_group()
  
  # Testing:
  res_dta_bis %>% 
    dplyr::filter(Product.Group == "Oil products") %>% 
    nrow() %>% 
    expect_equal(0)
  res_dta_bis %>% 
    dplyr::filter(Product.Group == "All fossil fuels") %>% 
    dplyr::ungroup() %>% dplyr::select(Share) %>% dplyr::pull() %>%
    expect_equal(1)
  res_dta_bis %>% 
    dplyr::filter(Country == "A", Product.Group == "Coal products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.8181818, tolerance = 1e-5)
  res_dta_bis %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.1818182, tolerance = 1e-5)
  res_dta_bis %>% 
    dplyr::filter(Country == "A", Product.Group == "Natural gas") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.1818182, tolerance = 1e-5)
  
  
  # SECOND, WE TEST THE GMA APPROACH
  # THE LOSSES APPROACH IS NOT YET IMPLEMENTED AT THE COUNTRY LEVEL!!!
  
  # tidy_AB_data_gma <- tidy_AB_data %>%
  #   ECCTools::transform_to_gma()
  # 
  # tidy_AB_data_gma_prepared <- tidy_AB_data_gma %>% 
  #   prepare_gma_for_shares()
  # 
  # res_gma <- tidy_AB_data_gma_prepared %>% 
  #   calc_share_elec_supply_by_ff_group()
  
  # Testing
  # res_gma %>% 
  #   dplyr::filter(Product.Group == "Oil products") %>% 
  #   nrow() %>% 
  #   expect_equal(0)
  # res_gma %>% 
  #   dplyr::filter(Product.Group == "All fossil fuels") %>% 
  #   dplyr::ungroup() %>% dplyr::select(Share) %>% dplyr::pull() %>%
  #   expect_equal(c(1,1))
  # res_gma %>% 
  #   dplyr::filter(Country == "A", Product.Group == "Coal products") %>% 
  #   magrittr::extract2("Share") %>% 
  #   expect_equal(0.8181818, tolerance = 1e-5)
  # res_gma %>% 
  #   dplyr::filter(Country == "A", Product.Group == "Oil and gas products") %>% 
  #   magrittr::extract2("Share") %>% 
  #   expect_equal(0.1818182, tolerance = 1e-5)
  # res_gma %>% 
  #   dplyr::filter(Country == "A", Product.Group == "Natural gas") %>% 
  #   magrittr::extract2("Share") %>% 
  #   expect_equal(0.1818182, tolerance = 1e-5)
  # res_gma %>% 
  #   dplyr::filter(Country == "B", Product.Group == "Coal products") %>% 
  #   magrittr::extract2("Share") %>% 
  #   expect_equal(0.1176471, tolerance = 1e-5)
  # res_gma %>% 
  #   dplyr::filter(Country == "B", Product.Group == "Oil and gas products") %>% 
  #   magrittr::extract2("Share") %>% 
  #   expect_equal(0.8823529, tolerance = 1e-5)
  # res_gma %>% 
  #   dplyr::filter(Country == "B", Product.Group == "Natural gas") %>% 
  #   magrittr::extract2("Share") %>% 
  #   expect_equal(0.8823529, tolerance = 1e-5)
})




test_that("calc_share_heat_supply_by_ff_group works with losses modelling",{
  
  # Path to dummy AB data
  A_B_path <- system.file("extdata/A_B_data_full_2018_format_stat_diffs_stock_changes.csv", package = "EROITools")
  
  # Loading data
  tidy_AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df(unit_val = "ktoe") %>%
    dplyr::filter(Country == "A") %>% 
    # Adding losses of Heat
    tibble::add_row(
      Country = "A",
      Method = "PCM",
      Energy.type = "E",
      Last.stage = "Final",
      Year = 2018,
      Ledger.side = "Supply",
      Flow.aggregation.point = "TFC compare",
      Flow = "Losses",
      Product = "Heat",
      Unit = "ktoe",
      E.dot = -10
    ) %>%
    dplyr::mutate(
      E.dot = dplyr::case_when(
        (Flow == "Iron and steel" & Product == "Heat") ~ 40,
        TRUE ~ E.dot
      )
    ) %>% 
    IEATools::specify_all() %>%
    ECCTools::specify_elect_heat_renewables() %>%
    ECCTools::specify_elect_heat_fossil_fuels() %>%
    ECCTools::specify_elect_heat_nuclear() %>%
    ECCTools::specify_other_elec_heat_production() %>%
    ECCTools::specify_elect_heat_markets() %>%
    IEATools::add_psut_matnames(R_includes_all_exogenous_flows = FALSE) %>%
    ECCTools::stat_diffs_to_balancing() %>%
    ECCTools::stock_changes_to_balancing() %>% 
    ECCTools::specify_losses_as_industry()
  
  
  # FIRST, WE TEST THE DTA APPROACH
  
  # Calculating total use of each product
  tidy_AB_dta <- tidy_AB_data %>%
    ECCTools::transform_to_dta(requirement_matrices_list = c("U_feed"),
                               select_dta_observations = FALSE)
  
  res_dta <- tidy_AB_dta %>%
    calc_share_heat_supply_by_ff_group()
  
  # Country A
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.8)
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil products") %>% 
    nrow() %>% 
    expect_equal(0)
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.1454545, tolerance = 1e-5)
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Natural gas") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.1454545, tolerance = 1e-5)
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Coal products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.6545455, tolerance = 1e-5)
  
  # Country B
  # res_dta %>% 
  #   dplyr::filter(Country == "B", Product.Group == "All fossil fuels") %>% 
  #   magrittr::extract2("Share") %>% 
  #   expect_equal(0.6)
  # res_dta %>% 
  #   dplyr::filter(Country == "B", Product.Group == "Oil products") %>% 
  #   nrow() %>% 
  #   expect_equal(0)
  # res_dta %>% 
  #   dplyr::filter(Country == "B", Product.Group == "Oil and gas products") %>% 
  #   magrittr::extract2("Share") %>% 
  #   expect_equal(0.5294118, tolerance = 1e-5)
  # res_dta %>% 
  #   dplyr::filter(Country == "B", Product.Group == "Natural gas") %>% 
  #   magrittr::extract2("Share") %>% 
  #   expect_equal(0.5294118, tolerance = 1e-5)
  # res_dta %>% 
  #   dplyr::filter(Country == "B", Product.Group == "Coal products") %>% 
  #   magrittr::extract2("Share") %>% 
  #   expect_equal(0.07058824, tolerance = 1e-5)
  
  
  # SECOND, WE TEST THE GMA APPROACH
  # NOTE THAT LOSSES MODELLING IS NOT IMPLEMENTED YET FOR COUNTRY LEVEL!!
  
  # tidy_AB_data_gma <- tidy_AB_data %>%
  #   ECCTools::transform_to_gma()
  # 
  # tidy_AB_data_gma_prepared <- tidy_AB_data_gma %>% 
  #   prepare_gma_for_shares()
  # 
  # res_gma <- tidy_AB_data_gma_prepared %>% 
  #   calc_share_heat_supply_by_ff_group()
  
  # Country A
  # res_dta %>% 
  #   dplyr::filter(Country == "A", Product.Group == "All fossil fuels") %>% 
  #   magrittr::extract2("Share") %>% 
  #   expect_equal(0.8)
  # res_dta %>% 
  #   dplyr::filter(Country == "A", Product.Group == "Oil products") %>% 
  #   nrow() %>% 
  #   expect_equal(0)
  # res_dta %>% 
  #   dplyr::filter(Country == "A", Product.Group == "Oil and gas products") %>% 
  #   magrittr::extract2("Share") %>% 
  #   expect_equal(0.1454545, tolerance = 1e-4)
  # res_dta %>% 
  #   dplyr::filter(Country == "A", Product.Group == "Natural gas") %>% 
  #   magrittr::extract2("Share") %>% 
  #   expect_equal(0.1454545, tolerance = 1e-4)
  # res_dta %>% 
  #   dplyr::filter(Country == "A", Product.Group == "Coal products") %>% 
  #   magrittr::extract2("Share") %>% 
  #   expect_equal(0.6545455, tolerance = 1e-4)
  
  # Country B
  # res_dta %>% 
  #   dplyr::filter(Country == "B", Product.Group == "All fossil fuels") %>% 
  #   magrittr::extract2("Share") %>% 
  #   expect_equal(0.6)
  # res_dta %>% 
  #   dplyr::filter(Country == "B", Product.Group == "Oil products") %>% 
  #   nrow() %>% 
  #   expect_equal(0)
  # res_dta %>% 
  #   dplyr::filter(Country == "B", Product.Group == "Oil and gas products") %>% 
  #   magrittr::extract2("Share") %>% 
  #   expect_equal(0.5294118, tolerance = 1e-5)
  # res_dta %>% 
  #   dplyr::filter(Country == "B", Product.Group == "Natural gas") %>% 
  #   magrittr::extract2("Share") %>% 
  #   expect_equal(0.5294118, tolerance = 1e-5)
  # res_dta %>% 
  #   dplyr::filter(Country == "B", Product.Group == "Coal products") %>% 
  #   magrittr::extract2("Share") %>% 
  #   expect_equal(0.07058824, tolerance = 1e-5)
})
