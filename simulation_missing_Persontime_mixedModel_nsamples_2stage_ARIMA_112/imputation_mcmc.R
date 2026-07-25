# this R code is to define a imputation model based on mle

library(Hmisc)
library(lme4)
library(mice)
library(ranger)
imputation_mcmc <- function(data) {
  data <- data %>% dplyr::select(-lag16, -lag17, -lag18, -lag19)
  # first estimate cumulative hazard by Alan-Nelson estimator
  data$H <- nelsonaalen(data, age_end, failure)
    tmp <- data%>%
      dplyr::select("id", starts_with("lag"), "failure", "H")
    imp_Data <- mice(tmp,m=1,maxit=10,meth="rf",seed=500)
    tmp <- complete(imp_Data) 
  
  data <- data %>% dplyr::select(-paste0("lag", 1:15))
  data <- left_join(data, tmp) %>%
    arrange(id)
 
  return(data)
  
}
