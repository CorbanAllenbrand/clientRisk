#' Data Preprocessing Function
#'
#' This function allows you to preprocess the data that is needed as input into the three risk models.
#' @param cognos.data Data that comes from the Cognois CMT report.
#' @param clientstatus.data Data that comes from the at-risk report.
#' @param tat.data Data that comes from Snowflake.
#' @export

cmt_data_cleaner <- function(cognos.data, status.data, tat.data){
  
  cmt_data <- cognos.data
  client_status <- status.data
  tat_data <- tat.data

  
  ### Next block of code takes the Week column and does string processing to 
  ### generate a new column that counts the week number. This week number is 
  ### important for later use of the muliple period logitic regression models.
  ### At the end of the block, three columns are added to the cmt_data dataframe
  ### - begin_of_week, end_of_week, week_num.

  week_split <-  stringr::str_split(cmt_data$week,"-")
  end_of_week <- lapply(week_split,function(x){
    stringr::str_trim(x[2])
  })
  begin_of_week <- lapply(week_split,function(x){
    stringr::str_trim(x[1])
  })
  
  cmt_data$end_of_week <- lubridate::mdy(unlist(end_of_week))
  cmt_data$begin_of_week <- lubridate::mdy(unlist(begin_of_week))
  
  eows <- sort(as.Date(unique(unlist(end_of_week)), format="%m/%d/%Y"))
  eows_parsed <- strptime(eows, "%Y-%m-%d")
  eows <- format(eows_parsed, "%m/%d/%Y")
  
  bows <- sort(as.Date(unique(unlist(begin_of_week)), format="%d/%m/%Y"))
  bows_parsed <- strptime(bows, "%Y-%m-%d")
  bows <- format(bows_parsed, "%m/%d/%Y")
  
  eow_index <- seq(1,length(eows))
  match <- vector("numeric",length = nrow(cmt_data))
  for(i in 1:length(end_of_week)){
    match[i] <- which(eows == end_of_week[[i]])
  }
  cmt_data$week_num <- match
  
  #############################################################################
  ### Next block of code creates start and end columns from the week_num column
  ### in order to allow for dynamic covariates in the hazard regression model. 
  ### Without these start and end times, the model would wrongly assume that all
  ### of the risk exposure factors are constant in time. The end result are 
  ### risk_status and date_closed variables that are added to the cmt_data 
  ### dataframe. Final new_cmt_data is result of merging the original cmt_date 
  ### with the risk_status and date_closed variables.
  
  start <- sapply(cmt_data$week_num,function(x){
    val <- x - 1
  })
  end <- start + 1
  
  cmt_data$start <- start
  cmt_data$end <- end
  
  ##############################################################################
  ### Next block of code processes the client.status file which has the risk
  ### status of that client as "lost" or "saved" and the date the last Salesforce
  ### case was closed, which proxies for the date the client was lost. 
  
  close_split <- stringr::str_split(client_status$closed_date," ")
  date_closed <- lapply(close_split, function(x){
    stringr::str_trim(x[1])
  })
  client_status$date_closed <- lubridate::mdy(date_closed)
  client_status <- subset(client_status,
                          select=c("account_number",
                                   "at_risk_case_final_status",
                                   "date_closed")
                          )
  client_status$risk_status <- client_status$at_risk_case_final_status
  client_status$at_risk_case_final_status <- NULL
  for(i in 1:length(client_status$risk_status)){
    if(identical(client_status$risk_status[i],"Partial-Lost")){
      client_status$risk_status[i] <- "Lost"
    }
  }
  unique_client <- unique(client_status$account_number)
  unique_status <- NULL
  unique_date_closed <- NULL
  for( i in unique_client){
    ind1 <- which(client_status$account_number == i)
    ind2 <- which.min(client_status$date_closed[ind1])
    ind3 <- ind1[ind2]
    unique_status <- c(unique_status,
                       client_status$risk_status[ind3])
    unique_date_closed <- c(unique_date_closed,
                            format(client_status$date_closed[ind3],
                                   "%Y-%m-%d")
                            )
  }
  
  status_closed_df <- data.frame(account_number = unique_client,
                                 risk_status = unique_status,
                                 date_closed = unique_date_closed)
  
  new_cmt_data <- merge(cmt_data,status_closed_df,by = "account_number")
  
  ##############################################################################
  ### Next block of code removes rows (observations) for clients whose begin_of_week date
  ### occurs after the date_closed date. The reasoning is that once a client is 
  ### lost then no new information should be used in risk assessment. Any 
  ### residual testing and resulting variable values should be ignored. Also,
  ### a lost_event variable is added to the new_cmt_data dataframe which records
  ### a "1" on the (client,week_num) row that corresponds to the week when the
  ### client was lost. Last, a new idaa_electronic_ratio variable is created
  ### which measures the proportion of requisitions that were electronic of
  ### all recorded requisitions in a week.
  
  for(i in unique(new_cmt_data$account_number)){
    index <- which(new_cmt_data$account_number == i)
    if(all(new_cmt_data$risk_status[index] == "Lost")){
      bool <- new_cmt_data$begin_of_week[index] > new_cmt_data$date_closed[index]
      if(any(bool) == T){
        new_cmt_data <- new_cmt_data[-(index[bool]),]
      }
      if(any(bool) == F){
        new_cmt_data <- new_cmt_data[-(index[!bool]),]
      }
    }
  }
  
  new_cmt_data$lost_event <- rep(0,nrow(new_cmt_data))
  
  for(i in unique(new_cmt_data$account_number)){
    index <- which(new_cmt_data$account_number == i)
    df <- new_cmt_data[index,]
    if(all(df$risk_status == "Lost")){
      new_cmt_data$lost_event[index[new_cmt_data$week_num[index] == max(as.numeric(new_cmt_data$week_num[index]))]] <- 1
    }
  }
  
  
  new_cmt_data$idaa_electronic_ratio <- new_cmt_data$idaa_elec / (new_cmt_data$idaa_elec + new_cmt_data$idaa_man + 0.0000001)
  
  ##############################################################################
  ### Next block of code takes in turnaround time (TAT) data and computes the 
  ### median TAT for each client in the new_cmt_data dataframe. The median is
  ### evaluated over all tests whose date of release occured before the date_closed
  ### value for a "lost" client or for all dates whose release falls in the 
  ### temporal scope of the investigation. Taking the median across time assumes 
  ### that a single median tat for each client is a decent approximation and the
  ### median tat represents a time-invariant TAT for that client. Changes in TAT
  ### between weeks is ignored by this approximation and might discard some 
  ### information.

  for(i in unique(tat_data$account_number)){
    index <- which(tat_data$account_number == i)
    bool <- tat_data$entry_date[index] > new_cmt_data$date_closed[new_cmt_data$account_number == i][1]
    if(any(is.na(bool))){
      tat_data <- tat_data[-(index[is.na(bool)]),]
    } else if(any(bool) == T){
      tat_data <- tat_data[-(index[bool]),]
    }
  }
  
  tat_df <- dplyr::group_by(tat_data,account_number) %>%
    dplyr::summarize(median_tat = median(tat))
  
  final_cmt_data <- merge(new_cmt_data,tat_df,by="account_number")
  
  final_cmt_data$id <- NULL
  
  return(final_cmt_data)
}
