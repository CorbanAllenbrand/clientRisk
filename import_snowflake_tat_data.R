#' Snowflake TAT Data Import Function
#'
#' This function allows you import data from Snowflake.
#' @param files A list of files that were exported from Snowflake.
#' @export

import_snowflake_tat_data <- function(files){
  
  data <- vector("list", length = length(files))
  
  for(i in 1:length(files)){
    res <- read.csv(files[[i]])
    res <- janitor::clean_names(res)
    data[[i]] <- res
  }
  
  tat_data <- do.call(rbind,data)
  tat_data[,1] <- NULL
  names(tat_data) <- c("account_number","entry_date","tat")
  
  return(tat_data)
  
}