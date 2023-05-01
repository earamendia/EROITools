

test_that("calc_share_primary_ff_supply_by_product_by_group works",{

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
    calc_share_primary_ff_supply_by_product_by_group()
  
  # Testing
  res_dta %>% 
    dplyr::filter(Country == "A", Product == "Coking coal", Product.Group == "Coal products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(1)
  
  res_dta %>% 
    dplyr::filter(Country == "A", Product == "Crude oil", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.6746032, tolerance = 1e-5)
  
  res_dta %>% 
    dplyr::filter(Country == "A", Product == "Natural gas", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.3253968, tolerance = 1e-5)
  
  res_dta %>% 
    dplyr::filter(Country == "A", Product == "Coking coal", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.2840909, tolerance = 1e-5)
  
  res_dta %>% 
    dplyr::filter(Country == "A", Product == "Crude oil", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.4829545, tolerance = 1e-5)
  
  res_dta %>% 
    dplyr::filter(Country == "A", Product == "Natural gas", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.2329545, tolerance = 1e-5)
  
  res_dta %>% 
    dplyr::filter(Country == "B") %>% 
    nrow() %>% 
    expect_equal(0)
  
  
  # SECOND, WE TEST THE GMA APPROACH
  
  tidy_AB_data_gma <- tidy_AB_data %>%
    ECCTools::transform_to_gma()
  
  tidy_AB_data_gma_prepared <- tidy_AB_data_gma %>% 
    prepare_gma_for_shares()
    # dplyr::mutate(
    #   Country = stringr::str_extract(Flow, "\\{.*\\}") %>% 
    #     stringr::str_remove("\\{") %>% 
    #     stringr::str_remove("\\}"),
    #   Flow = stringr::str_remove(Flow, "\\{.*\\}_"),
    #   product_without_origin = stringr::str_remove(Product, "\\{.*\\}_"),
    # )
  
  res_gma <- tidy_AB_data_gma_prepared %>% 
    calc_share_primary_ff_supply_by_product_by_group()
  
  # Testing
  res_gma %>% 
    dplyr::filter(Country == "A", Product == "{A}_Coking coal", Product.Group == "Coal products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(1)
  
  res_gma %>% 
    dplyr::filter(Country == "A", Product == "{A}_Crude oil", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.6746032, tolerance = 1e-5)
  
  res_gma %>% 
    dplyr::filter(Country == "A", Product == "{A}_Natural gas", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.3253968, tolerance = 1e-5)
  
  res_gma %>% 
    dplyr::filter(Country == "A", Product == "{A}_Coking coal", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.2840909, tolerance = 1e-5)
  
  res_gma %>% 
    dplyr::filter(Country == "A", Product == "{A}_Crude oil", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.4829545, tolerance = 1e-5)
  
  res_gma %>% 
    dplyr::filter(Country == "A", Product == "{A}_Natural gas", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.2329545, tolerance = 1e-5)
  
  res_gma %>% 
    dplyr::filter(Country == "B") %>% 
    nrow() %>% 
    expect_equal(0)
})


test_that("calc_share_ff_use_by_product_by_group works",{

  # Path to dummy AB data
  A_B_path <- system.file("extdata/A_B_data_full_2018_format_stat_diffs_stock_changes.csv", package = "EROITools")
  #"inst/extdata/A_B_data_full_2018_format_stat_diffs_stock_changes.csv"
  
  # Loading data
  tidy_AB_data <- A_B_path %>% 
    IEATools::load_tidy_iea_df(unit_val = "ktoe") %>% 
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
    prepare_gma_for_shares()
    # dplyr::mutate(
    #   Country = stringr::str_extract(Flow, "\\{.*\\}") %>% 
    #     stringr::str_remove("\\{") %>% 
    #     stringr::str_remove("\\}"),
    #   Flow = stringr::str_remove(Flow, "\\{.*\\}_"),
    #   product_without_origin = stringr::str_remove(Product, "\\{.*\\}_"),
    # )
  
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
    
    # Trying with GMA:
    tidy_AB_data_non_energy_gma <- tidy_AB_data_non_energy %>% 
      ECCTools::transform_to_gma()
    
    tidy_AB_data_non_energy_gma_adapted <- tidy_AB_data_non_energy_gma %>% 
      prepare_gma_for_shares()
    
    res_gma_inc_non_energy_uses <- tidy_AB_data_non_energy_gma_adapted %>% 
      calc_share_ff_use_by_product_by_group(include_non_energy_uses = TRUE)
    
    # Checking coke oven coke
    res_gma_inc_non_energy_uses %>%
      dplyr::filter(Country == "A", Product == "{A}_Coke oven coke", Product.Group == "Coal products") %>%
      magrittr::extract2("Share") %>% 
      expect_equal(0.2153846, tolerance = 1e-6)

    res_gma_inc_non_energy_uses %>%
      dplyr::filter(Country == "A", Product == "{B}_Coke oven coke", Product.Group == "Coal products") %>%
      magrittr::extract2("Share") %>% 
      expect_equal(0.1846154, tolerance = 1e-6)

    res_gma_inc_non_energy_uses %>%
      dplyr::filter(Country == "A", Product == "{A}_Blast furnace gas", Product.Group == "Coal products") %>%
      magrittr::extract2("Share") %>%
      expect_equal(0.6, tolerance = 1e-6)

    res_gma_inc_non_energy_uses %>%
      dplyr::filter(Country == "A", Product == "{A}_Coke oven coke", Product.Group == "All fossil fuels") %>%
      magrittr::extract2("Share") %>%
      expect_equal(0.04487179, tolerance = 1e-6)

    res_gma_inc_non_energy_uses %>%
      dplyr::filter(Country == "A", Product == "{B}_Coke oven coke", Product.Group == "All fossil fuels") %>%
      magrittr::extract2("Share") %>%
      expect_equal(0.03846154, tolerance = 1e-6)

    res_gma_inc_non_energy_uses %>%
      dplyr::filter(Country == "A", Product == "{A}_Blast furnace gas", Product.Group == "All fossil fuels") %>%
      magrittr::extract2("Share") %>%
      expect_equal(0.125, tolerance = 1e-6)

    res_gma_inc_non_energy_uses %>%
      dplyr::filter(Country == "A", Product == "{A}_Natural gas", Product.Group == "All fossil fuels") %>%
      magrittr::extract2("Share") %>%
      expect_equal(0.1166667, tolerance = 1e-6)
})



test_that("calc_shares_elec_by_ff_group works",{

  # Path to dummy AB data
  A_B_path <- system.file("extdata/A_B_data_full_2018_format_stat_diffs_stock_changes.csv", package = "EROITools")
  
  # Loading data
  tidy_AB_data <- A_B_path %>% 
    IEATools::load_tidy_iea_df(unit_val = "ktoe") %>% 
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
    calc_shares_elec_by_ff_group()
  
  # Testing
  res_dta %>% 
    dplyr::filter(Product.Group == "Coal products", Product == "Electricity [from Coal products]") %>% 
    dplyr::select(Share) %>% 
    dplyr::pull() %>% 
    expect_equal(c(1, 1))
  res_dta %>% 
    dplyr::filter(Product.Group == "Oil products", Product == "Electricity [from Oil products]") %>% 
    nrow() %>% 
    expect_equal(0)
  res_dta %>%
    dplyr::filter(Product.Group == "Natural gas", Product == "Electricity [from Natural gas]") %>% 
    dplyr::select(Share) %>% 
    dplyr::pull() %>% 
    expect_equal(c(1, 1))
  res_dta %>%
    dplyr::filter(Product.Group == "Oil and gas products", Product == "Electricity [from Natural gas]") %>% 
    dplyr::select(Share) %>% 
    dplyr::pull() %>% 
    expect_equal(c(1, 1))
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Product == "Electricity [from Coal products]") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.8181818, tolerance = 1e-5)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Product == "Electricity [from Natural gas]") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.1818182, tolerance = 1e-5)
  res_dta %>%
    dplyr::filter(Country == "B", Product.Group == "All fossil fuels", Product == "Electricity [from Coal products]") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.1176471, tolerance = 1e-5)
  res_dta %>%
    dplyr::filter(Country == "B", Product.Group == "All fossil fuels", Product == "Electricity [from Natural gas]") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.8823529, tolerance = 1e-5)
  
  
  # SECOND, WE TEST THE GMA APPROACH
  
  tidy_AB_data_gma <- tidy_AB_data %>%
    ECCTools::transform_to_gma()
  
  tidy_AB_data_gma_prepared <- tidy_AB_data_gma %>% 
    prepare_gma_for_shares()

  
  res_gma <- tidy_AB_data_gma_prepared %>% 
    calc_shares_elec_by_ff_group()
  
  # Testing
  res_gma %>% 
    dplyr::filter(Product.Group == "Coal products", Product == "{A}_Electricity [from Coal products]") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(1)
  res_gma %>% 
    dplyr::filter(Product.Group == "Oil products", Product == "{A}_Electricity [from Oil products]") %>% 
    nrow() %>% 
    expect_equal(0)
  res_gma %>%
    dplyr::filter(Product.Group == "Natural gas", Product == "{A}_Electricity [from Natural gas]") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(1)
  res_gma %>%
    dplyr::filter(Product.Group == "Oil and gas products", Product == "{B}_Electricity [from Natural gas]") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(1)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Product == "{A}_Electricity [from Coal products]") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.8181818, tolerance = 1e-5)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Product == "{A}_Electricity [from Natural gas]") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.1818182, tolerance = 1e-5)
  res_gma %>%
    dplyr::filter(Country == "B", Product.Group == "All fossil fuels", Product == "{B}_Electricity [from Coal products]") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.1176471, tolerance = 1e-5)
  res_gma %>%
    dplyr::filter(Country == "B", Product.Group == "All fossil fuels", Product == "{B}_Electricity [from Natural gas]") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.8823529, tolerance = 1e-5)
})



test_that("calc_shares_heat_by_ff_group works",{

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
    calc_shares_heat_by_ff_group()
  
  # Testing
  res_dta %>% 
    dplyr::filter(Product.Group == "Coal products", Product == "Heat [from Coal products]") %>% 
    dplyr::select(Share) %>% 
    dplyr::pull() %>% 
    expect_equal(c(1, 1))
  res_dta %>% 
    dplyr::filter(Product.Group == "Oil products", Product == "Heat [from Oil products]") %>% 
    nrow() %>% 
    expect_equal(0)
  res_dta %>%
    dplyr::filter(Product.Group == "Natural gas", Product == "Heat [from Natural gas]") %>% 
    dplyr::select(Share) %>% 
    dplyr::pull() %>% 
    expect_equal(c(1, 1))
  res_dta %>%
    dplyr::filter(Product.Group == "Oil and gas products", Product == "Heat [from Natural gas]") %>% 
    dplyr::select(Share) %>% 
    dplyr::pull() %>% 
    expect_equal(c(1, 1))
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Product == "Heat [from Coal products]") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.8181818, tolerance = 1e-5)
  res_dta %>%
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Product == "Heat [from Natural gas]") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.1818182, tolerance = 1e-5)
  res_dta %>%
    dplyr::filter(Country == "B", Product.Group == "All fossil fuels", Product == "Heat [from Coal products]") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.1176471, tolerance = 1e-5)
  res_dta %>%
    dplyr::filter(Country == "B", Product.Group == "All fossil fuels", Product == "Heat [from Natural gas]") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.8823529, tolerance = 1e-5)
  
  
  # SECOND, WE TEST THE GMA APPROACH
  
  tidy_AB_data_gma <- tidy_AB_data %>%
    ECCTools::transform_to_gma()
  
  tidy_AB_data_gma_prepared <- tidy_AB_data_gma %>% 
    prepare_gma_for_shares()
  
  res_gma <- tidy_AB_data_gma_prepared %>% 
    calc_shares_heat_by_ff_group()
  
  # Testing
  res_gma %>% 
    dplyr::filter(Product.Group == "Coal products", Product == "{A}_Heat [from Coal products]") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(1)
  res_gma %>% 
    dplyr::filter(Product.Group == "Oil products", Product == "{A}_Heat [from Oil products]") %>% 
    nrow() %>% 
    expect_equal(0)
  res_gma %>%
    dplyr::filter(Product.Group == "Natural gas", Product == "{A}_Heat [from Natural gas]") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(1)
  res_gma %>%
    dplyr::filter(Product.Group == "Oil and gas products", Product == "{B}_Heat [from Natural gas]") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(1)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Product == "{A}_Heat [from Coal products]") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.8181818, tolerance = 1e-5)
  res_gma %>%
    dplyr::filter(Country == "A", Product.Group == "All fossil fuels", Product == "{A}_Heat [from Natural gas]") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.1818182, tolerance = 1e-5)
  res_gma %>%
    dplyr::filter(Country == "B", Product.Group == "All fossil fuels", Product == "{B}_Heat [from Coal products]") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.1176471, tolerance = 1e-5)
  res_gma %>%
    dplyr::filter(Country == "B", Product.Group == "All fossil fuels", Product == "{B}_Heat [from Natural gas]") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.8823529, tolerance = 1e-5)
})


# ADD SOME TEST WITH NON-ENERGY USES?????
test_that("calc_shares_ff_by_group_inc_elec_heat works",{

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
    calc_shares_ff_by_group_inc_elec_heat()
  
  # Testing
  # Checking coal products shares
  res_dta %>% 
    dplyr::filter(Country == "A", Product == "Blast furnace gas", Product.Group == "Coal products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.200974, tolerance = 1e-5)
  res_dta %>% 
    dplyr::filter(Country == "A", Product == "Coke oven coke", Product.Group == "Coal products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.053593, tolerance = 1e-5)
  res_dta %>% 
    dplyr::filter(Country == "A", Product == "Electricity [from Coal products]", Product.Group == "Coal products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.7015834, tolerance = 1e-5)
  res_dta %>% 
    dplyr::filter(Country == "A", Product == "Heat [from Coal products]", Product.Group == "Coal products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.04384896, tolerance = 1e-5)
  # Checking oil products shares
  res_dta %>% 
    dplyr::filter(Country == "A", Product == "Kerosene type jet fuel excl. biofuels", Product.Group == "Oil products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.432098, tolerance = 1e-5)
  res_dta %>% 
    dplyr::filter(Country == "A", Product == "Motor gasoline excl. biofuels", Product.Group == "Oil products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.56790, tolerance = 1e-5)
  # Checking natural gas shares
  res_dta %>% 
    dplyr::filter(Country == "A", Product == "Natural gas", Product.Group == "Natural gas") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.5310345, tolerance = 1e-5)
  res_dta %>% 
    dplyr::filter(Country == "A", Product == "Electricity [from Natural gas]", Product.Group == "Natural gas") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.4413793, tolerance = 1e-5)
  res_dta %>% 
    dplyr::filter(Country == "A", Product == "Heat [from Natural gas]", Product.Group == "Natural gas") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.02758621, tolerance = 1e-5)
  # Checking Oil and gas products shares
  res_dta %>% 
    dplyr::filter(Country == "A", Product == "Natural gas", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.130398, tolerance = 1e-5)
  res_dta %>% 
    dplyr::filter(Country == "A", Product == "Kerosene type jet fuel excl. biofuels", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.3259949, tolerance = 1e-5)
  res_dta %>% 
    dplyr::filter(Country == "A", Product == "Motor gasoline excl. biofuels", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.4284505, tolerance = 1e-5)
  res_dta %>% 
    dplyr::filter(Country == "A", Product == "Heat [from Natural gas]", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.006773914, tolerance = 1e-5)
  res_dta %>% 
    dplyr::filter(Country == "A", Product == "Electricity [from Natural gas]", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.1083827, tolerance = 1e-5)
  # Checking All fossil fuels group shares
  res_dta %>% 
    dplyr::filter(Country == "A", Product == "Natural gas", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.07692308, tolerance = 1e-5)
  res_dta %>% 
    dplyr::filter(Country == "A", Product == "Kerosene type jet fuel excl. biofuels", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.1923077, tolerance = 1e-5)
  res_dta %>% 
    dplyr::filter(Country == "A", Product == "Motor gasoline excl. biofuels", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.2527473, tolerance = 1e-5)
  res_dta %>% 
    dplyr::filter(Country == "A", Product == "Heat [from Natural gas]", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.003996, tolerance = 1e-5)
  res_dta %>% 
    dplyr::filter(Country == "A", Product == "Electricity [from Natural gas]", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.06393605, tolerance = 1e-5)
  res_dta %>% 
    dplyr::filter(Country == "A", Product == "Blast furnace gas", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.08241758, tolerance = 1e-5)
  res_dta %>% 
    dplyr::filter(Country == "A", Product == "Coke oven coke", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.02197802, tolerance = 1e-5)
  res_dta %>% 
    dplyr::filter(Country == "A", Product == "Electricity [from Coal products]", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.2877123, tolerance = 1e-5)
  res_dta %>% 
    dplyr::filter(Country == "A", Product == "Heat [from Coal products]", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.01798201, tolerance = 1e-5)
  # Couple of country B tests as well:
  res_dta %>% 
    dplyr::filter(Country == "B", Product == "Natural gas", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.2300406, tolerance = 1e-5)
  res_dta %>% 
    dplyr::filter(Country == "B", Product == "Heat [from Natural gas]", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.0608931, tolerance = 1e-5)
  res_dta %>% 
    dplyr::filter(Country == "B", Product == "Electricity [from Natural gas]", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.202977, tolerance = 1e-5)
  res_dta %>% 
    dplyr::filter(Country == "B", Product == "Kerosene type jet fuel excl. biofuels", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.2300406, tolerance = 1e-5)
  
  
  # SECOND, WE TEST THE GMA APPROACH
  
  tidy_AB_data_gma <- tidy_AB_data %>%
    ECCTools::transform_to_gma()
  
  tidy_AB_data_gma_prepared <- tidy_AB_data_gma %>% 
    prepare_gma_for_shares()
  
  res_gma <- tidy_AB_data_gma_prepared %>% 
    calc_shares_ff_by_group_inc_elec_heat()
  
  # Testing
  # Coal products
  res_gma %>% 
    dplyr::filter(Country == "A", Product == "{A}_Blast furnace gas", Product.Group == "Coal products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.200974, tolerance = 1e-5)
  res_gma %>% 
    dplyr::filter(Country == "A", Product == "{A}_Coke oven coke", Product.Group == "Coal products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.0214372, tolerance = 1e-5)
  res_gma %>% 
    dplyr::filter(Country == "A", Product == "{B}_Coke oven coke", Product.Group == "Coal products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.0321558, tolerance = 1e-5)
  res_gma %>% 
    dplyr::filter(Country == "A", Product == "{A}_Electricity [from Coal products]", Product.Group == "Coal products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.7015834, tolerance = 1e-5)
  res_gma %>% 
    dplyr::filter(Country == "A", Product == "{A}_Heat [from Coal products]", Product.Group == "Coal products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.04384896, tolerance = 1e-5)
  # Checking oil products shares
  res_gma %>% 
    dplyr::filter(Country == "A", Product == "{A}_Kerosene type jet fuel excl. biofuels", Product.Group == "Oil products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.432098, tolerance = 1e-5)
  res_gma %>% 
    dplyr::filter(Country == "A", Product == "{A}_Motor gasoline excl. biofuels", Product.Group == "Oil products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.56790, tolerance = 1e-5)
  # Checking natural gas shares
  res_gma %>% 
    dplyr::filter(Country == "A", Product == "{A}_Natural gas", Product.Group == "Natural gas") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.5310345, tolerance = 1e-5)
  res_gma %>% 
    dplyr::filter(Country == "A", Product == "{A}_Electricity [from Natural gas]", Product.Group == "Natural gas") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.4413793, tolerance = 1e-5)
  res_gma %>% 
    dplyr::filter(Country == "A", Product == "{A}_Heat [from Natural gas]", Product.Group == "Natural gas") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.02758621, tolerance = 1e-5)
  # Checking Oil and gas products shares
  res_gma %>% 
    dplyr::filter(Country == "A", Product == "{A}_Natural gas", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.130398, tolerance = 1e-5)
  res_gma %>% 
    dplyr::filter(Country == "A", Product == "{A}_Kerosene type jet fuel excl. biofuels", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.3259949, tolerance = 1e-5)
  res_gma %>% 
    dplyr::filter(Country == "A", Product == "{A}_Motor gasoline excl. biofuels", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.4284505, tolerance = 1e-5)
  res_gma %>% 
    dplyr::filter(Country == "A", Product == "{A}_Heat [from Natural gas]", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.006773914, tolerance = 1e-5)
  res_gma %>% 
    dplyr::filter(Country == "A", Product == "{A}_Electricity [from Natural gas]", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.1083827, tolerance = 1e-5)
  # Checking All fossil fuels group shares
  res_gma %>% 
    dplyr::filter(Country == "A", Product == "{A}_Natural gas", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.07692308, tolerance = 1e-5)
  res_gma %>% 
    dplyr::filter(Country == "A", Product == "{A}_Kerosene type jet fuel excl. biofuels", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.1923077, tolerance = 1e-5)
  res_gma %>% 
    dplyr::filter(Country == "A", Product == "{A}_Motor gasoline excl. biofuels", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.2527473, tolerance = 1e-5)
  res_gma %>% 
    dplyr::filter(Country == "A", Product == "{A}_Heat [from Natural gas]", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.003996, tolerance = 1e-5)
  res_gma %>% 
    dplyr::filter(Country == "A", Product == "{A}_Electricity [from Natural gas]", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.06393605, tolerance = 1e-5)
  res_gma %>% 
    dplyr::filter(Country == "A", Product == "{A}_Blast furnace gas", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.08241758, tolerance = 1e-5)
  res_gma %>% 
    dplyr::filter(Country == "A", Product == "{A}_Coke oven coke", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.008791208, tolerance = 1e-5)
  res_gma %>% 
    dplyr::filter(Country == "A", Product == "{B}_Coke oven coke", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.01318681, tolerance = 1e-5)
  res_gma %>% 
    dplyr::filter(Country == "A", Product == "{A}_Electricity [from Coal products]", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.2877123, tolerance = 1e-5)
  res_gma %>% 
    dplyr::filter(Country == "A", Product == "{A}_Heat [from Coal products]", Product.Group == "All fossil fuels") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.01798201, tolerance = 1e-5)
  # Couple of country B tests as well:
  res_gma %>% 
    dplyr::filter(Country == "B", Product == "{B}_Natural gas", Product.Group == "Oil and gas products") %>% 
    nrow() %>% expect_equal(0)
  res_gma %>% 
    dplyr::filter(Country == "B", Product == "{A}_Natural gas", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.2300406, tolerance = 1e-5)
  res_gma %>% 
    dplyr::filter(Country == "B", Product == "{B}_Heat [from Natural gas]", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.0608931, tolerance = 1e-5)
  res_gma %>% 
    dplyr::filter(Country == "B", Product == "{B}_Electricity [from Natural gas]", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.202977, tolerance = 1e-5)
  res_gma %>% 
    dplyr::filter(Country == "B", Product == "{B}_Kerosene type jet fuel excl. biofuels", Product.Group == "Oil and gas products") %>% 
    magrittr::extract2("Share") %>% 
    expect_equal(0.2300406, tolerance = 1e-5)
  res_gma %>% 
    dplyr::filter(Country == "B", Product == "{A}_Kerosene type jet fuel excl. biofuels", Product.Group == "Oil and gas products") %>% 
    nrow() %>% expect_equal(0)
})



test_that("prepare_gma_for_shares function works",{
  
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
  
  # Converting to gma:
  tidy_AB_data_gma <- tidy_AB_data %>%
    ECCTools::transform_to_gma()
  
  # Plain code version of df:
  tidy_AB_data_gma_prepared <- tidy_AB_data_gma %>% 
    dplyr::mutate(
      Country = stringr::str_extract(Flow, "\\{.*\\}") %>% 
        stringr::str_remove("\\{") %>% 
        stringr::str_remove("\\}"),
      Flow = stringr::str_remove(Flow, "\\{.*\\}_"),
      product_without_origin = stringr::str_remove(Product, "\\{.*\\}_"),
    )
  
  # Function version of df:
  tidy_AB_data_gma_prepared_bis <- prepare_gma_for_shares(tidy_AB_data_gma)
  
  # Comparing and expecting the same df:
  expect_true(all(tidy_AB_data_gma_prepared == tidy_AB_data_gma_prepared_bis))
})




