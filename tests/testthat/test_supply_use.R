
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
    calc_total_use_by_product()
  
  # Checking coke oven coke
  res_dta_excl_non_energy %>% 
    dplyr::filter(Country == "A", Product == "Coke oven coke") %>% 
    magrittr::extract2("Total_Product_Use") %>% 
    expect_equal(200)
  
  # Calculating energy use by product including non-energy uses:
  res_dta_incl_non_energy <- tidy_AB_data_non_energy %>% 
    ECCTools::transform_to_dta(requirement_matrices_list = c("U_feed"),
                               select_dta_observations = FALSE) %>% 
    calc_total_use_by_product(include_non_energy_uses = TRUE)
  
  # Checking coke oven coke
  res_dta_incl_non_energy %>% 
    dplyr::filter(Country == "A", Product == "Coke oven coke") %>% 
    magrittr::extract2("Total_Product_Use") %>% 
    expect_equal(500)
})


test_that("calc_primary_products_supply_by_group works",{

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
    calc_primary_products_supply_by_group()
  
  # Testing
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Coal products") %>% 
    magrittr::extract2("Total_Group_Supply") %>% 
    expect_equal(5000)
  
  # Doesn't quite match with the ECC diagram in ECCTools package description because
  # 1) Have added 500 oil and gas extraction for creating unbalances
  # 2) Natural gas also created by the oil refineries in our example to add challenge
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("Total_Group_Supply") %>% 
    expect_equal(12600)
  
  res_dta %>% 
    dplyr::filter(Country == "B") %>% 
    nrow() %>% 
    expect_equal(0)

  
  # SECOND, WE TEST THE GMA APPROACH
  
  # Calculating total use of each product
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
    calc_primary_products_supply_by_group()

  # Testing
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Coal products") %>% 
    magrittr::extract2("Total_Group_Supply") %>% 
    expect_equal(5000)
  
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("Total_Group_Supply") %>% 
    expect_equal(12600)
  
  res_dta %>% 
    dplyr::filter(Country == "B") %>% 
    nrow() %>% 
    expect_equal(0)
})



test_that("calc_all_products_use_by_group works",{

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
  
  # Calculating total energy use by product group
  tidy_AB_dta <- tidy_AB_data %>% 
    ECCTools::transform_to_dta(requirement_matrices_list = c("U_feed"),
                               select_dta_observations = FALSE)
  
  res_dta <- tidy_AB_dta %>% 
    calc_all_products_use_by_group()

  # Testing
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Coal products") %>% 
    magrittr::extract2("Total_Group_Use") %>% 
    expect_equal(950)
  
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil products") %>% 
    magrittr::extract2("Total_Group_Use") %>% 
    expect_equal(4050)
  
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("Total_Group_Use") %>% 
    expect_equal(4750)
  
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "Natural gas") %>% 
    magrittr::extract2("Total_Group_Use") %>% 
    expect_equal(700)
  
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "Coal products") %>% 
    magrittr::extract2("Total_Group_Use") %>% 
    expect_equal(750)
  
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "Oil products") %>% 
    magrittr::extract2("Total_Group_Use") %>% 
    expect_equal(2200)
  
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("Total_Group_Use") %>% 
    expect_equal(3200)
  
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "Natural gas") %>% 
    magrittr::extract2("Total_Group_Use") %>% 
    expect_equal(1000)
  
  
  # SECOND, WE TEST THE GMA APPROACH
  
  # Calculating total energy use by product group
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
    calc_all_products_use_by_group()
  
  # Testing
  res_gma %>% 
    dplyr::filter(Country == "A", Product.Group == "Coal products") %>% 
    magrittr::extract2("Total_Group_Use") %>% 
    expect_equal(950)
  
  res_gma %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil products") %>% 
    magrittr::extract2("Total_Group_Use") %>% 
    expect_equal(4050)
  
  res_gma %>% 
    dplyr::filter(Country == "A", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("Total_Group_Use") %>% 
    expect_equal(4750)
  
  res_gma %>% 
    dplyr::filter(Country == "A", Product.Group == "Natural gas") %>% 
    magrittr::extract2("Total_Group_Use") %>% 
    expect_equal(700)
  
  res_gma %>% 
    dplyr::filter(Country == "B", Product.Group == "Coal products") %>% 
    magrittr::extract2("Total_Group_Use") %>% 
    expect_equal(750)
  
  res_gma %>% 
    dplyr::filter(Country == "B", Product.Group == "Oil products") %>% 
    magrittr::extract2("Total_Group_Use") %>% 
    expect_equal(2200)
  
  res_gma %>% 
    dplyr::filter(Country == "B", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("Total_Group_Use") %>% 
    expect_equal(3200)
  
  res_gma %>% 
    dplyr::filter(Country == "B", Product.Group == "Natural gas") %>% 
    magrittr::extract2("Total_Group_Use") %>% 
    expect_equal(1000)
  
  
  # THIRD,TRYING WITH NON-ENERGY USES FLOWS
  
  # Adding a non-energy use flow
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
    calc_all_products_use_by_group()
  
  # Checking coal products
  res_dta_excl_non_energy %>% 
    dplyr::filter(Country == "A", Product.Group == "Coal products") %>% 
    magrittr::extract2("Total_Group_Use") %>% 
    expect_equal(950)
  
  # Calculating energy use by product including non-energy uses:
  res_dta_incl_non_energy <- tidy_AB_data_non_energy %>% 
    ECCTools::transform_to_dta(requirement_matrices_list = c("U_feed"),
                               select_dta_observations = FALSE) %>% 
    calc_all_products_use_by_group(include_non_energy_uses = TRUE)
  
  # Checking coal products
  res_dta_incl_non_energy %>% 
    dplyr::filter(Country == "A", Product.Group == "Coal products") %>% 
    magrittr::extract2("Total_Group_Use") %>% 
    expect_equal(1250)
})



test_that("calc_primary_ff_supply works",{

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
  
  # Calculating total energy use by product group
  tidy_AB_dta <- tidy_AB_data %>% 
    ECCTools::transform_to_dta(requirement_matrices_list = c("U_feed"),
                               select_dta_observations = FALSE)
  
  res_dta <- tidy_AB_dta %>% 
    calc_primary_ff_supply()

  # Testing
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("Total_Group_Use") %>% 
    expect_equal(17600)

  res_dta %>% 
    dplyr::filter(Country == "B") %>% 
    nrow() %>% 
    expect_equal(0)

  
  # SECOND, WE TEST THE GMA APPROACH
  
  # Calculating total energy use by product group
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
    calc_primary_ff_supply()
  
  # Testing
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("Total_Group_Use") %>% 
    expect_equal(17600)
  
  res_dta %>% 
    dplyr::filter(Country == "B") %>% 
    nrow() %>% 
    expect_equal(0)
})



test_that("calc_ff_use works",{

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
  
  # Calculating total energy use by product group
  tidy_AB_dta <- tidy_AB_data %>% 
    ECCTools::transform_to_dta(requirement_matrices_list = c("U_feed"),
                               select_dta_observations = FALSE)
  
  res_dta <- tidy_AB_dta %>% 
    calc_ff_use()
  
  # Testing
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("Total_Group_Use") %>% 
    expect_equal(5700)
  
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("Total_Group_Use") %>% 
    expect_equal(3950)
  

  # SECOND, WE TEST THE GMA APPROACH
  
  # Calculating total energy use by product group
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
    calc_ff_use()

  # Testing
  res_dta %>% 
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("Total_Group_Use") %>% 
    expect_equal(5700)
  
  res_dta %>% 
    dplyr::filter(Country == "B", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("Total_Group_Use") %>% 
    expect_equal(3950)
  
  
  # THIRD,TRYING WITH NON-ENERGY USES FLOWS
  
  # Adding a non-energy use flow
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
    calc_ff_use()
  
  # Testing
  res_dta_excl_non_energy %>% 
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("Total_Group_Use") %>% 
    expect_equal(5700)
  
  res_dta_excl_non_energy %>% 
    dplyr::filter(Country == "B", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("Total_Group_Use") %>% 
    expect_equal(3950)
  
  # Calculating energy use by product including non-energy uses:
  res_dta_incl_non_energy <- tidy_AB_data_non_energy %>% 
    ECCTools::transform_to_dta(requirement_matrices_list = c("U_feed"),
                               select_dta_observations = FALSE) %>% 
    calc_ff_use(include_non_energy_uses = TRUE)
  
  # Testing
  res_dta_incl_non_energy %>% 
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("Total_Group_Use") %>% 
    expect_equal(6000)
  
  res_dta_incl_non_energy %>% 
    dplyr::filter(Country == "B", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("Total_Group_Use") %>% 
    expect_equal(3950)
})

