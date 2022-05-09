
test_that("calc_total_use_by_product works",{
  
  # Path to dummy AB data
  A_B_path <- system.file("extdata/A_B_data_full_2018_format_stat_diffs_stock_changes.csv", package = "EROITools")
  #"inst/extdata/A_B_data_full_2018_format_stat_diffs_stock_changes.csv"
  
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
    calc_total_use_by_product()
  
  # Testing the DTA approach
  # Country A
  res_dta %>% 
    dplyr::filter(Country == "A", Product == "Blast furnace gas") %>% 
    magrittr::extract2("Total_Product_Use") %>% 
    expect_equal(750)
  
  res_dta %>% 
    dplyr::filter(Country == "A", Product == "Coke oven coke") %>% 
    magrittr::extract2("Total_Product_Use") %>% 
    expect_equal(200)
  
  res_dta %>% 
    dplyr::filter(Country == "A", Product == "Kerosene type jet fuel excl. biofuels") %>% 
    magrittr::extract2("Total_Product_Use") %>% 
    expect_equal(1750)
  
  res_dta %>% 
    dplyr::filter(Country == "A", Product == "Motor gasoline excl. biofuels") %>% 
    magrittr::extract2("Total_Product_Use") %>% 
    expect_equal(2300)
  
  res_dta %>% 
    dplyr::filter(Country == "A", Product == "Natural gas") %>% 
    magrittr::extract2("Total_Product_Use") %>% 
    expect_equal(700)
  
  # Country B
  res_dta %>% 
    dplyr::filter(Country == "B", Product == "Blast furnace gas") %>% 
    magrittr::extract2("Total_Product_Use") %>% 
    expect_equal(650)
  
  res_dta %>% 
    dplyr::filter(Country == "B", Product == "Coke oven coke") %>% 
    magrittr::extract2("Total_Product_Use") %>% 
    expect_equal(100)
  
  res_dta %>% 
    dplyr::filter(Country == "B", Product == "Kerosene type jet fuel excl. biofuels") %>% 
    magrittr::extract2("Total_Product_Use") %>% 
    expect_equal(1000)
  
  res_dta %>% 
    dplyr::filter(Country == "B", Product == "Motor gasoline excl. biofuels") %>% 
    magrittr::extract2("Total_Product_Use") %>% 
    expect_equal(1200)
  
  res_dta %>% 
    dplyr::filter(Country == "B", Product == "Natural gas") %>% 
    magrittr::extract2("Total_Product_Use") %>% 
    expect_equal(1000)
  
  
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
    calc_total_use_by_product()
  
  # Actual tests below:
  # Country A
  res_gma %>% 
    dplyr::filter(Country == "A", Product == "{A}_Blast furnace gas") %>% 
    magrittr::extract2("Total_Product_Use") %>% 
    expect_equal(750)
  
  res_gma %>% 
    dplyr::filter(Country == "A", Product == "{A}_Coke oven coke") %>% 
    magrittr::extract2("Total_Product_Use") %>% 
    expect_equal(80)
  
  res_gma %>% 
    dplyr::filter(Country == "A", Product == "{B}_Coke oven coke") %>% 
    magrittr::extract2("Total_Product_Use") %>% 
    expect_equal(120)
  
  res_gma %>% 
    dplyr::filter(Country == "A", Product == "{A}_Kerosene type jet fuel excl. biofuels") %>% 
    magrittr::extract2("Total_Product_Use") %>% 
    expect_equal(1750)
  
  res_gma %>% 
    dplyr::filter(Country == "A", Product == "{A}_Motor gasoline excl. biofuels") %>% 
    magrittr::extract2("Total_Product_Use") %>% 
    expect_equal(2300)
  
  res_gma %>% 
    dplyr::filter(Country == "A", Product == "{A}_Natural gas") %>% 
    magrittr::extract2("Total_Product_Use") %>% 
    expect_equal(700)
  
  # Country B
  res_gma %>% 
    dplyr::filter(Country == "B", product_without_origin == "Blast furnace gas") %>% 
    magrittr::extract2("Total_Product_Use") %>% 
    expect_equal(650)
  
  res_gma %>% 
    dplyr::filter(Country == "B", product_without_origin == "Coke oven coke") %>% 
    magrittr::extract2("Total_Product_Use") %>% 
    expect_equal(100)
  
  res_gma %>% 
    dplyr::filter(Country == "B", product_without_origin == "Kerosene type jet fuel excl. biofuels") %>% 
    magrittr::extract2("Total_Product_Use") %>% 
    expect_equal(1000)
  
  res_gma %>% 
    dplyr::filter(Country == "B", Product == "{B}_Motor gasoline excl. biofuels") %>% 
    magrittr::extract2("Total_Product_Use") %>% 
    expect_equal(1200)
  
  res_gma %>% 
    dplyr::filter(Country == "B", Product == "{A}_Natural gas") %>% 
    magrittr::extract2("Total_Product_Use") %>% 
    expect_equal(1000)
})


# test_that("calc_primary_products_supply_by_group works",{
#   
#   
#   
#   
#   
#   
#   
# })
# 
# 
# 
# 
# test_that("calc_all_products_use_by_group works",{
#   
#   
#   
#   
#   
#   
#   
# })
# 
# test_that("calc_primary_ff_supply works",{
#   
#   
#   
#   
#   
#   
#   
# })
# 
# 
# test_that("calc_ff_use works",{
#   
#   
#   
#   
#   
#   
#   
# })

