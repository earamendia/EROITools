# This script creates constants and saves them in the right locations.
# If there are any changes to these constants,
# source this script before building the package.

#
# Preparing the tidy_dta_AB_erois
#

tidy_AB_dta <- ECCTools::tidy_AB_data %>%
  IEATools::add_psut_matnames() %>% 
  ECCTools::transform_to_dta(requirement_matrices_list = c("U_feed"),
                             select_dta_observations = FALSE)

tidy_io_AB_dta <- tidy_AB_dta %>%
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

usethis::use_data(tidy_AB_erois_dta, overwrite = TRUE)


#
# Preparing the tidy_gma_AB_erois
#

tidy_AB_data_gma <- ECCTools::tidy_AB_data %>%
  IEATools::add_psut_matnames() %>% 
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

usethis::use_data(tidy_AB_data_gma, overwrite = TRUE)





