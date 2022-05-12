

# tidy_AB_erois_gma <- tidy_io_AB_gma %>% 
#   Recca::calc_E_EIOU() %>% 
#   glimpse()
#   
# 
# View(tidy_AB_erois_gma$e_EIOU[[1]])
# 
# 
# 
# 
#   
#   Recca::calc_erois() %>% 
#   EROITools::extract_tidy_product_erois() %>% 
#   dplyr::mutate(
#     Eroi.method = "DTA"
#   ) %>% 
#   dplyr::relocate(.data[["Eroi.method"]], .after = Year)