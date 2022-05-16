

# aggregated_useful_erois <- tidy_shares_df %>%
#   dplyr::inner_join(.tidy_erois_df %>% 
#                       dplyr::select(-.data[[country]]), by = c({method}, {energy_type}, {last_stage}, {year}, {product})) %>%
#   print()
#   
#   
#   dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]],
#                   .data[[eroi.method]], .data[[type]], .data[[boundary]], .data[[non_energy_uses]], .data[[product.group]], .data[[energy.stage]]) %>%
#   dplyr::summarise(
#     Group.eroi.inversed = sum(.data[[share]] * (1/.data[[useful_stage_eroi]])) / sum(.data[[share]])
#   ) %>% 
#   dplyr::mutate(
#     "{group.eroi}" := 1 / Group.eroi.inversed
#   ) %>% 
#   dplyr::select(-Group.eroi.inversed)




# # Calculating useful stage EROIs for each product
# useful_erois_df <- tidy_io_global_erois_df %>% 
#   dplyr::left_join(FU_global_efficiencies_df, 
#                    by = c("Country", "Method", "Energy.type", "Year", "Product")) %>% 
#   dplyr::mutate(
#     Useful_Stage_EROI = EROI * .data[[Average_Efficiency_Col]]
#   ) %>% 
#   # This should get rid of "RoW" rows for national level calcs.
#   dplyr::filter(! is.na(Useful_Stage_EROI))
# 
# 
# 
# 
# # Adapt national IO erois to add a product without origin column
# tidy_national_io_erois_adapted <- tidy_io_national_erois_df %>% 
#   dplyr::mutate(product_without_origin = stringr::str_remove(Product, "\\{.*\\}_")) %>% 
#   dplyr::select(-Country)
# 
# # Calculates tidy_national_useful_stage_erois
# tidy_national_useful_stage_erois <- FU_national_efficiencies_df %>%
#   dplyr::rename(product_without_origin = Product) %>% 
#   dplyr::left_join(
#     tidy_national_io_erois_adapted,
#     by = c("Method", "Energy.type", "Year", "product_without_origin")
#   ) %>% 
#   dplyr::mutate(
#     Useful_Stage_EROI = Average_Efficiency_By_Country * EROI
#   ) %>% 
#   dplyr::select(-product_without_origin)
