
#' Title
#'
#' @param .tidy_io_erois 
#' @param tidy_FU_efficiencies 
#' @param country 
#' @param method 
#' @param energy_type 
#' @param year 
#' @param product 
#' @param product_without_origin 
#' @param average_efficiency 
#' @param eroi 
#' @param useful_stage_eroi 
#' @param eroi_calc_method 
#'
#' @return
#' @export
#'
#' @examples
push_to_useful_erois <- function(.tidy_io_erois,
                                 tidy_FU_efficiencies,
                                 country = IEATools::iea_cols$country,
                                 method = IEATools::iea_cols$method,
                                 energy_type = IEATools::iea_cols$energy_type,
                                 year = IEATools::iea_cols$year,
                                 product = IEATools::iea_cols$product,
                                 product_without_origin = "product_without_origin",
                                 average_efficiency = "Average_Efficiency_Col",
                                 eroi = "EROI",
                                 useful_stage_eroi = "Useful_Stage_EROI",
                                 eroi_calc_method = c("dta", "gma")){
  
  eroi_calc_method <- match.arg(eroi_calc_method)
  
  if (eroi_calc_method == "dta"){
    # Adapt EROI data frame to add a product without origin column:
    tidy_io_erois_adapted <- .tidy_io_erois %>% 
      dplyr::mutate(
        "{product_without_origin}" := stringr::str_remove(.data[[product]], "\\{.*\\}_")
      ) %>% 
      dplyr::select(-.data[[country]])
    
    # Bind and calculate useful stage EROIs:
    tidy_useful_erois <- tidy_FU_efficiencies %>% 
      dplyr::rename(
        "{product_without_origin}" := .data[[product]]
      ) %>% 
      dplyr::left_join(
        tidy_io_erois_adapted,
        by = c({method}, {energy_type}, {year}, {product_without_origin})
      ) %>% 
      dplyr::mutate(
        "{useful_stage_eroi}" := .data[[eroi]] * .data[[average_efficiency]]
      ) %>% 
      dplyr::select(-.data[[product_without_origin]])
    
  } else if (eroi_calc_method == "gma"){
    # Calculate useful stage EROIs data frame
    tidy_useful_erois <- .tidy_io_erois %>% 
      dplyr::left_join(tidy_FU_efficiencies, by = c({country}, {method}, {energy_type}, {year}, {product})) %>% 
      dplyr::mutate(
        "{useful_stage_eroi}" := .data[[eroi]] * .data[[average_efficiency]]
      ) %>% 
      dplyr::filter(! is.na(.data[[useful_stage_eroi]]))
  }
  
  return(tidy_useful_erois)
}