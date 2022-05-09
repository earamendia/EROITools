
test_that("calc_total_use_by_product works",{
  
  # Path to dummy AB data
  A_B_path <- "inst/extdata/A_B_data_full_2018_format_stat_diffs_stock_changes.csv"
  #system.file("extdata/A_B_data_full_2018_format_stat_diffs_stock_changes.csv", package = "EROITools")
  
  # Loading data
  tidy_AB_data <- A_B_path %>% 
    IEATools::load_tidy_iea_df() %>% 
    #IEATools::fix_tidy_iea_df_balances() %>% 
    IEATools::specify_all() %>% 
    ECCTools::specify_elect_heat_renewables() %>% 
    ECCTools::specify_elect_heat_fossil_fuels() %>% 
    ECCTools::specify_elect_heat_nuclear() %>% 
    ECCTools::specify_other_elec_heat_production() %>% 
    ECCTools::specify_elect_heat_markets() %>% 
    IEATools::add_psut_matnames()

  
  # FIRST, WE TEST THE DTA APPROACH
  
  # Calculating total use of each product
  tidy_AB_dta <- tidy_AB_data %>% 
    # Don't use the stat_diffs_to_balancing and stock_changes_to_balancing functions for GMA, so locate them here
    ECCTools::stat_diffs_to_balancing() %>% 
    ECCTools::stock_changes_to_balancing() %>% 
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

