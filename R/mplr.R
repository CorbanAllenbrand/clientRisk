#' Multiple Period Logistic Regression Function
#'
#' This function allows you to estimate a multiple period logistic regression model, a mulitple period logistic regression model with backwards stepwise selection, and print out results.
#' @param data Data set that is used in the model estimation. Data set is obtained by using the data_cleaner() function.
#' @param output.file.name Name of a html file that the model result table is printed to. The file extension for this should be ".html"
#' @export

mplr <- function(data, output.file.name){
  
  oldw <- getOption("warn")
  options(warn = -1)
  
  glm_mod <- glm(lost_event ~ week_num + sf_cases + billing_errors+ median_tat + idaa_electronic_ratio + ncs_calls + nof + tiq + tnp,
               family = "binomial",
               data = data)
  
plot1 <- modelsummary::modelplot(glm_mod, exponentiate = TRUE) + ggplot2::theme_bw()

glm_step <- MASS::stepAIC(glm_mod, 
                    direction = "backward", 
                    trace = F,
                    data = data)

stargazer::stargazer(glm_mod,glm_step, type="html", align=TRUE, out=output.file.name,
          title = "Mulitple Period Logistic Regression Results",
          ci=TRUE, ci.level=0.95,
          column.labels=c("Pre-Selection","Post-Selection"))

plot2 <- modelsummary::modelplot(glm_step, exponentiate = TRUE) + ggplot2::theme_bw()

options(warn = oldw)
  
return(list(mlpr.full = glm_mod,
            mlpr.reduced = glm_step,
            mlpr.full.results = plot1,
            mlpr.reduced.results = plot2))

}
