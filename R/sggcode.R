#' A sgg_code function
#'
#' This function allows you to get sgg code data frame.
#' @param 
#' @keywords sgg_code
#' @export
#' @examples
#' get_sgg_code_name_df()

get_sgg_code_name_df <- function() {
  sgg_code_name_filename <- "sgg_code_name.rds"
  if(file.exists(sgg_code_name_filename)) {
    sgg_code_name <- readRDS(file = sgg_code_name_filename)
  } else {
    sgg_code_name <- read_excel(paste0(getwd(), "/",'sggcode.xlsx'), sheet = "sggcode")
    colnames(sgg_code_name) <- c('sggcode', 'sggname', 'sidoname')
    sgg_code_name <- sgg_code_name %>%
      mutate(sidocode = str_sub(sggcode, 1,2)) %>%
      select(sidocode, sidoname, sggcode, sggname)
    saveRDS(sgg_code_name, file = sgg_code_name_filename)
  }
  return (sgg_code_name)
} 

#' A sgg_code function
#'
#' This function allows you to get sido_code by passing sido_name.
#' @param sgg_code_name_df sgg_code_name dataframe.
#' @param sidonamestr sido_name string (e.g. '서울특별시', '경기도').
#' @keywords sgg_code, sido_name, sido_code
#' @export
#' @examples
#' get_sidocode(sgg_code_name_df, '서울특별시')

get_sidocode <- function(sgg_code_name_df, sidonamestr) {
  resultdf <- sgg_code_name_df %>% 
    filter (sidoname == sidonamestr) %>%
    select(sidocode) 
  return(resultdf$sidocode[1])  
}

#' A sgg_code function
#'
#' This function allows you to get sgg_code by passing ssg_name.
#' @param sgg_code_name_df sgg_code_name dataframe.
#' @param sidonamestr sido_name string (e.g. '서울특별시', '경기도').
#' @param sggnamestr sgg_name string (e.g. '강남구', '수원시').
#' @keywords sgg_code, sido_name, sgg_name, sgg_code
#' @export
#' @examples
#' get_sggcode(sgg_code_name_df, '서울특별시', '강남구')

get_sggcode <- function(sgg_code_name_df, sidonamestr, sggnamestr) {

  if (sggnamestr == sidonamestr) { sggcode <- paste0(get_sidocode(sgg_code_name_df, sidonamestr), '000')
   } else {
    sggcodedf <-   sgg_code_name_df %>% filter (sidoname == sidonamestr & sggname == sggnamestr) %>% select(sggcode)
    sggcode <- sggcodedf$sggcode[1]   
  }
  return(sggcode)  
}