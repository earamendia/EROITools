

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
    IEATools::add_psut_matnames() %>%
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
    dplyr::mutate(
      Country = stringr::str_extract(Flow, "\\{.*\\}") %>% 
        stringr::str_remove("\\{") %>% 
        stringr::str_remove("\\}"),
      Flow = stringr::str_remove(Flow, "\\{.*\\}_"),
      product_without_origin = stringr::str_remove(Product, "\\{.*\\}_"),
    )
  
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
    IEATools::add_psut_matnames() %>%
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
    dplyr::mutate(
      Country = stringr::str_extract(Flow, "\\{.*\\}") %>% 
        stringr::str_remove("\\{") %>% 
        stringr::str_remove("\\}"),
      Flow = stringr::str_remove(Flow, "\\{.*\\}_"),
      product_without_origin = stringr::str_remove(Product, "\\{.*\\}_"),
    )
  
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
    IEATools::add_psut_matnames() %>%
    ECCTools::stat_diffs_to_balancing() %>%
    ECCTools::stock_changes_to_balancing()
  
  
  # FIRST, WE TEST THE DTA APPROACH
  
  # Calculating total use of each product
  tidy_AB_dta <- tidy_AB_data %>%
    ECCTools::transform_to_dta(requirement_matrices_list = c("U_feed"),
                               select_dta_observations = FALSE)
  
  res_dta <- tidy_AB_dta %>%
    calc_fec_from_ff_as_heat_by_group()
  
  # Testing
  
  
  
  
  
  
  
  
  
  
  
  
  # SECOND, WE TEST THE GMA APPROACH
  
  tidy_AB_data_gma <- tidy_AB_data %>%
    ECCTools::transform_to_gma()
  
  tidy_AB_data_gma_prepared <- tidy_AB_data_gma %>% 
    dplyr::mutate(
      Country = stringr::str_extract(Flow, "\\{.*\\}") %>% 
        stringr::str_remove("\\{") %>% 
        stringr::str_remove("\\}"),
      Flow = stringr::str_remove(Flow, "\\{.*\\}_"),
      product_without_origin = stringr::str_remove(Product, "\\{.*\\}_"),
    )
  
  res_gma <- tidy_AB_data_gma_prepared %>% 
    calc_fec_from_ff_as_heat_by_group()
  


})


test_that("calc_share_heat_supply_by_ff_group",{



})




# test_that("calc_fec_from_ff_by_group",{
# 
  # # Path to dummy AB data
  # A_B_path <- system.file("extdata/A_B_data_full_2018_format_stat_diffs_stock_changes.csv", package = "EROITools")
  # 
  # # Loading data
  # tidy_AB_data <- A_B_path %>%
  #   IEATools::load_tidy_iea_df() %>%
  #   IEATools::specify_all() %>%
  #   ECCTools::specify_elect_heat_renewables() %>%
  #   ECCTools::specify_elect_heat_fossil_fuels() %>%
  #   ECCTools::specify_elect_heat_nuclear() %>%
  #   ECCTools::specify_other_elec_heat_production() %>%
  #   ECCTools::specify_elect_heat_markets() %>%
  #   IEATools::add_psut_matnames() %>%
  #   ECCTools::stat_diffs_to_balancing() %>%
  #   ECCTools::stock_changes_to_balancing()
  # 
  # 
  # # FIRST, WE TEST THE DTA APPROACH
  # 
  # # Calculating total use of each product
  # tidy_AB_dta <- tidy_AB_data %>%
  #   ECCTools::transform_to_dta(requirement_matrices_list = c("U_feed"),
  #                              select_dta_observations = FALSE)
  # 
  # res_dta <- tidy_AB_dta %>%
  #   calc_fec_from_ff_by_group()
#   
#   
#   
#   
#   
#   
# 
# 
# })



# test_that("calc_fec_from_ff_as_fuel_by_group",{
#   
#   
#   
# })
