#' Cognos CMT Data Import Function
#'
#' This function allows you import data from the Cognos CMT report tool.
#' @param files A list of files that were exported from the Cognos CMT tool.
#' @export

import_cognos_data <- function(files){
  
  data <- vector("list", length = length(files))
  
  for(i in 1:length(files)){
    res <- readxl::read_xlsx(files[[i]])
    res <- janitor::clean_names(res)
    data[[i]] <- res
  }
  
  cognos <- do.call(rbind,data)
  
  names(cognos)[which(names(cognos) %in% c("ncs_call_types"))] <- "ncs_calls"
  names(cognos)[which(names(cognos) %in% c("tnp_release_date"))] <- "tnp"
  names(cognos)[which(names(cognos) %in% c("billing_errors_total"))] <- "billing_errors"
  
  return(cognos)
  
}

