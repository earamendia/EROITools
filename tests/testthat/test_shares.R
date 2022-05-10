
# I think there is a big design issue with this calc_share_primary_ff_supply_by_product_by_group() function.
# test_that("calc_share_primary_ff_supply_by_product_by_group works",{
# 
# 
# })


test_that("calc_share_ff_use_by_product_by_group works",{

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
    calc_share_ff_use_by_product_by_group()
  
  # General tests:
  res_dta %>% 
    dplyr::filter(Non_Energy_Uses == "Included") %>% 
    nrow() %>% 
    expect_equal(0)
  
  res_dta %>% 
    dplyr::filter(Energy.stage != "Final (fuel)") %>% 
    nrow() %>% 
    expect_equal(0)
  
  # Country A tests
  # Coal products
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Coal products", Product == "Blast furnace gas") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.7894737, tolerance = 1e-6)
  
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Coal products", Product == "Coke oven coke") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.2105263, tolerance = 1e-6)
  # Oil products
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil products", Product == "Kerosene type jet fuel excl. biofuels") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.432098, tolerance = 1e-5)
  
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil products", Product == "Motor gasoline excl. biofuels") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.567901, tolerance = 1e-6)
  # Natural gas
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Natural gas", Product == "Natural gas") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(1, tolerance = 1e-6)
  # Oil and gas products
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products", Product == "Natural gas") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.147368, tolerance = 1e-5)
  
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products", Product == "Kerosene type jet fuel excl. biofuels") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.368421, tolerance = 1e-6)
  
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products", Product == "Motor gasoline excl. biofuels") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.4842105, tolerance = 1e-6)
  # All fossil fuels
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Product == "Natural gas") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.122807, tolerance = 1e-4)
  
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Product == "Kerosene type jet fuel excl. biofuels") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.3070175, tolerance = 1e-4)
  
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Product == "Motor gasoline excl. biofuels") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.4035088, tolerance = 1e-4)
  
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Product == "Blast furnace gas") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.1315789, tolerance = 1e-4)
  
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Product == "Coke oven coke") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.0350877, tolerance = 1e-4)
  
  # Country B
  # Coal products
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "Coal products", Product == "Coke oven coke") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.1333333, tolerance = 1e-6)
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "Coal products", Product == "Blast furnace gas") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.8666667, tolerance = 1e-6)
  # Natural gas
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "Natural gas", Product == "Natural gas") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(1, tolerance = 1e-6)
  # Oil and gas products
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "Oil and gas products", Product == "Natural gas") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.3125, tolerance = 1e-6)
  
  
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
    calc_share_ff_use_by_product_by_group()
  
  
  # Country A tests
  # Coal products
  res_gma %>% 
    dplyr::filter(Country == "A", Product.Group == "Coal products", Product == "{A}_Blast furnace gas") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.7894737, tolerance = 1e-6)
  
  res_gma %>% 
    dplyr::filter(Country == "A", Product.Group == "Coal products", Product == "{A}_Coke oven coke") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.08421052, tolerance = 1e-6)
  
  res_gma %>% 
    dplyr::filter(Country == "A", Product.Group == "Coal products", Product == "{B}_Coke oven coke") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.1263158, tolerance = 1e-6)
  # Oil products
  res_gma %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil products", Product == "{A}_Kerosene type jet fuel excl. biofuels") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.432098, tolerance = 1e-5)
  
  res_gma %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil products", Product == "{A}_Motor gasoline excl. biofuels") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.567901, tolerance = 1e-6)
  # Natural gas
  res_gma %>% 
    dplyr::filter(Country == "A", Product.Group == "Natural gas", Product == "{A}_Natural gas") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(1, tolerance = 1e-6)
  # Oil and gas products
  res_gma %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products", Product == "{A}_Natural gas") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.147368, tolerance = 1e-5)
  
  res_gma %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products", Product == "{A}_Kerosene type jet fuel excl. biofuels") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.368421, tolerance = 1e-6)
  
  res_gma %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products", Product == "{A}_Motor gasoline excl. biofuels") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.4842105, tolerance = 1e-6)
  # All fossil fuels
  res_gma %>% 
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Product == "{A}_Natural gas") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.122807, tolerance = 1e-4)
  
  res_gma %>% 
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Product == "{A}_Kerosene type jet fuel excl. biofuels") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.3070175, tolerance = 1e-4)
  
  res_gma %>% 
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Product == "{A}_Motor gasoline excl. biofuels") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.4035088, tolerance = 1e-4)
  
  res_gma %>% 
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Product == "{A}_Blast furnace gas") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.1315789, tolerance = 1e-4)
  
  res_gma %>% 
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Product == "{A}_Coke oven coke") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.01403508, tolerance = 1e-4)
  
  res_gma %>% 
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Product == "{B}_Coke oven coke") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.02105262, tolerance = 1e-4)
  
  # Country B
  # Coal products
  res_gma %>% 
    dplyr::filter(Country == "B", Product.Group == "Coal products", Product == "{B}_Coke oven coke") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.1333333, tolerance = 1e-6)
  res_gma %>% 
    dplyr::filter(Country == "B", Product.Group == "Coal products", Product == "{B}_Blast furnace gas") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.8666667, tolerance = 1e-6)
  # Natural gas
  res_gma %>% 
    dplyr::filter(Country == "B", Product.Group == "Natural gas", Product == "{A}_Natural gas") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(1, tolerance = 1e-6)
  res_gma %>% 
    dplyr::filter(Country == "B", Product.Group == "Natural gas", Product == "{B}_Natural gas") %>% 
    nrow() %>% 
    expect_equal(0)
  # Oil and gas products
  res_gma %>% 
    dplyr::filter(Country == "B", Product.Group == "Oil and gas products", Product == "{A}_Natural gas") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.3125, tolerance = 1e-6)
  
  
  # THIRD, QUICK TEST WITH NON-ENERGY USES
  tidy_AB_data_non_energy <- tidy_AB_data %>%
    tibble::add_row(
      Country = "A",
      Method = "PCM",
      Energy.type = "E",
      Last.stage = "Final",
      Year = 2018,
      Ledger.side = "Consumption",
      Flow.aggregation.point = "Industry",
      Flow = IEATools::non_energy_flows$non_energy_use_industry_transformation_energy,
      Product = "Coke oven coke",
      Unit = "ktoe",
      E.dot = 300,
      matnames = "Y"
    )
  
  # Calculating energy use by product excluding non-energy uses:
  res_dta_excl_non_energy <- tidy_AB_data_non_energy %>%
    ECCTools::transform_to_dta(requirement_matrices_list = c("U_feed"),
                               select_dta_observations = FALSE) %>%
    calc_share_ff_use_by_product_by_group()
  
  # Checking coke oven coke
  res_dta_excl_non_energy %>%
    dplyr::filter(Country == "A", Product == "Coke oven coke", Product.Group == "Coal products") %>%
    magrittr::extract2("Share") %>%
    expect_equal(0.2105263, tolerance = 1e-6)
  
  res_dta_excl_non_energy %>%
    dplyr::filter(Country == "A", Product == "Coke oven coke", Product.Group == "All fossil fuels") %>%
    magrittr::extract2("Share") %>%
    expect_equal(0.0350877, tolerance = 1e-6)
  
  # Calculating energy use by product including non-energy uses:
  res_dta_incl_non_energy <- tidy_AB_data_non_energy %>%
    ECCTools::transform_to_dta(requirement_matrices_list = c("U_feed"),
                               select_dta_observations = FALSE) %>%
    calc_share_ff_use_by_product_by_group(include_non_energy_uses = TRUE)
  
  # Checking coke oven coke
  res_dta_incl_non_energy %>%
    dplyr::filter(Country == "A", Product == "Coke oven coke", Product.Group == "Coal products") %>% 
    magrittr::extract2("Share") %>%
    expect_equal(0.4, tolerance = 1e-6)
  
    res_dta_incl_non_energy %>%
    dplyr::filter(Country == "A", Product == "Blast furnace gas", Product.Group == "Coal products") %>%
    magrittr::extract2("Share") %>%
    expect_equal(0.6, tolerance = 1e-6)
  
    res_dta_incl_non_energy %>%
    dplyr::filter(Country == "A", Product == "Coke oven coke", Product.Group == "All fossil fuels") %>%
    magrittr::extract2("Share") %>%
    expect_equal(0.08333333, tolerance = 1e-6)
  
    res_dta_incl_non_energy %>%
    dplyr::filter(Country == "A", Product == "Blast furnace gas", Product.Group == "All fossil fuels") %>%
    magrittr::extract2("Share") %>%
    expect_equal(0.125, tolerance = 1e-6)
  
    res_dta_incl_non_energy %>%
    dplyr::filter(Country == "A", Product == "Natural gas", Product.Group == "All fossil fuels") %>%
    magrittr::extract2("Share") %>%
    expect_equal(0.1166667, tolerance = 1e-6)
})



# test_that("calc_shares_elec_by_ff_group works",{
#   
#   
#   
# })



# test_that("calc_shares_heat_by_ff_group works",{
#   
#   
#   
# })



# test_that("calc_shares_ff_by_group_inc_elec_heat works",{
#   
#   
#   
# })