#' Random Forest with Survical Regression Function
#'
#' This function allows you to fit a random forest with surival regression model and get variable importance of the input features. First plot "plot1" returns variable importance in terms of VIMP where the higher the VIMP value the more important the variable is. Second plot "plot2" returns variable importance according to minimum depth of tree where the smaller the depth the more important a variable is.
#' @param data Data set to be used in model fitting. Data set is obtained from the data_cleaner() function.
#' @export

rf <- function(data){
 
  rf_model <- randomForestSRC::rfsrc(Surv(week_num, lost_event) ~ sf_cases + billing_errors + tiq + median_tat + idaa_electronic_ratio + nof + ncs_calls + tnp,
                    data = data,
                    ntree=1000,
                    importance = "permute")
  
  plot1 <- ggRandomForests:::plot.gg_vimp(rf_model) +
    ggplot2::theme(legend.position = c(.8,.2))+
    ggplot2::labs(y = "Variable Importance (VIMP)",
         fill="VIMP > 0")+
    ggplot2::scale_fill_brewer(palette="Set2")+
    ggplot2::theme_bw()
  
  rf_md <- randomForestSRC::var.select(rf_model, verbose = FALSE)
  ggMindepth <- ggRandomForests:::gg_minimal_depth(rf_md, )
  plot2 <- plot(ggMindepth)
  
  return(list(rf.model = rf_model,
              variable.importance1 = plot1,
              variable.importance2 = plot2))
    
  
}
