#' Survival Regression Function
#'
#' This function allows you to fit survival regression, survival regression with backwards stepwise selection, and print results.
#' @param data Data set to be used in model fitting. Data set is obtained from the data_cleaner() function.
#' @export

survival_regression <- function(data){
  
  oldw <- getOption("warn")
  options(warn = -1)
  
  surv_mod <- survival::coxph(Surv(start, end, lost_event) ~ sf_cases + billing_errors + tiq + median_tat + idaa_electronic_ratio + nof + ncs_calls + tnp,
                    data = data)
  
  step_surv_mod <- step(surv_mod,
                        direction = "backward", 
                        trace = F)
  
  if(surv_mod$iter == 0){
    stop("Algorithm could not start, might be because of not enough data, need more 'Lost' clients.")
    }
  
  plot1 <- survminer::ggforest(surv_mod, 
             data=data,
             main = "Client Survival Regression Results")
  plot2 <- survminer::ggforest(step_surv_mod, 
             data = data,
            main = "Client Survival Regression Results after Variable Selection")
  
  plot3 <- modelsummary::modelplot(surv_mod, exponentiate = T)+ ggplot2::scale_color_brewer(type="qual")+ggplot2::theme_bw()
  plot4 <- modelsummary::modelplot(step_surv_mod, exponentiate = T)
    
  options(warn = oldw)
  
  return(list(survival_full_model = surv_mod,
              survival_simplified_model = step_surv_mod,
              full_model_results =plot1,
              simplified_model_results = plot2,
              full_model_results2 = plot3,
              simplified_model_results2 = plot4))
  
}
