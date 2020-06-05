#' Get Sigungu Code Data Frame 
#'
#' This function allows you to get sgg code data frame.
#'
#' @param sgg_code_rds_fname sgg_code_name rds filename. Default value is "sgg_code_name.rds".
#' @param sgg_code_xls_fname sgg_code_name xls filename. Default value is "sggcode.xlsx".
#' @return A data frame of sgg_code_name
#' @keywords sgg_code
#' @examples
#' get_sgg_code_name_df()
#' @export
get_sgg_code_name_df <- function(sgg_code_rds_fname = "sgg_code_name.rds", sgg_code_xls_fname = "sggcode.xlsx") {
  if (exists("sgg_code_name")) {
    return (sgg_code_name)
  } else if(file.exists(sgg_code_rds_fname)) {
    sgg_code_name <- readRDS(file = sgg_code_rds_fname)
  } else {
    sgg_code_name <- readxl::read_excel(paste0(getwd(), "/",sgg_code_xls_fname), sheet = "sggcode")
    colnames(sgg_code_name) <- c('sggcode', 'sggname', 'sidoname')
    sgg_code_name <- sgg_code_name %>%
      dplyr::mutate(sidocode = stringr::str_sub(sggcode, 1,2)) %>%
      dplyr::select(sidocode, sidoname, sggcode, sggname)
    saveRDS(sgg_code_name, file = sgg_code_rds_fname)
  }
  return (sgg_code_name)
} 

#' Get Sido Code Character String
#'
#' This function allows you to get sido_code by passing sido_name.
#' 
#' @param sgg_code_name_df sgg_code_name dataframe
#' @param sidonamestr sido_name string (e.g. '서울특별시', '경기도').
#' @return A character string of sido_code
#' @keywords sgg_code, sido_name, sido_code
#' @examples
#' get_sidocode(sgg_code_name_df, '서울특별시')
#' @export
get_sidocode <- function(sgg_code_name_df, sidonamestr) {
  resultdf <- sgg_code_name_df %>% 
    dplyr::filter (sidoname == sidonamestr) %>%
    dplyr::select(sidocode) 
  return(resultdf$sidocode[1])  
}

#' Get Sigungu Code Character String
#'
#' This function allows you to get sgg_code by passing ssg_name.
#' 
#' @param sgg_code_name_df sgg_code_name dataframe.
#' @param sidonamestr sido_name string (e.g. '서울특별시', '경기도').
#' @param sggnamestr sgg_name string (e.g. '강남구', '수원시').
#' @return A character string of sgg_code
#' @keywords sgg_code, sido_name, sgg_name, sgg_code
#' @examples
#' get_sggcode(sgg_code_name_df, '서울특별시', '강남구')
#' @export
get_sggcode <- function(sgg_code_name_df, sidonamestr, sggnamestr) {

  if (sggnamestr == sidonamestr) { sggcode <- paste0(get_sidocode(sgg_code_name_df, sidonamestr), '000')
   } else {
    sggcodedf <-   sgg_code_name_df %>% dplyr::filter (sidoname == sidonamestr & sggname == sggnamestr) %>% select(sggcode)
    sggcode <- sggcodedf$sggcode[1]   
  }
  return(sggcode)  
}