#' At Risk Report Import Function
#'
#' This function allows you import data from the at risk report.
#' @param files A list of files that were exported from the at risk report.
#' @export

import_client_status_data <- function(files){
  
  data <- vector("list", length = length(files))
  
  for(i in 1:length(files)){
    res <- read.csv(files[[i]])
    res <- janitor::clean_names(res)
    data[[i]] <- res
  }
  
  client <- do.call(rbind,data)
  
  client$at_risk_case_final_status <- ifelse(client$at_risk_case_final_status %in% c("Partial-Lost"),"Lost",client$at_risk_case_final_status)
  
  client <- client[which(client$at_risk_case_final_status %in% c("Saved","Lost")),]
  
  return(client)
  
}


