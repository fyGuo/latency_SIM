# this R code is to define a imputation model based on mle

library(Hmisc)
library(lme4)
library(mice)
library(ranger)
imputation_mcmc <- function(data) {
  data <- data %>% dplyr::select(-lag16, -lag17, -lag18, -lag19)
  # first estimate cumulative hazard by Alan-Nelson estimator
  data$H <- nelsonaalen(data, age_end, failure)
  # data <- data %>% dplyr::mutate(lag01_diff = lag0 - lag1,
  #                                lag12_diff = lag1 - lag2,
  #                                lag23_diff = lag2 - lag3,
  #                                lag34_diff = lag3 - lag4,
  #                                lag45_diff = lag4 - lag5,
  #                                lag56_diff = lag5 - lag6,
  #                                lag67_diff = lag6 - lag7,
  #                                lag78_diff = lag7 - lag8,
  #                                lag89_diff = lag8 - lag9,
  #                                lag910_diff = lag9 - lag10,
  #                                lag91011_diff = lag10 - lag11,
  #                                lag1112_diff = lag11 - lag12,
  #                                lag1213_diff = lag12 - lag13,
  #                                lag1314_diff = lag13 - lag14,
  #                                lag1415_diff = lag14 - lag15,
  # )
  # 
  tmp <- data%>%
      dplyr::select("id", starts_with("lag"), "failure", "H")
  imp_Data <- mice(tmp,m=1,maxit=10,meth="rf",seed=500)
    tmp <- complete(imp_Data) 
  
  data <- data %>% dplyr::select(-paste0("lag", 1:15))
  data <- left_join(data, tmp) %>%
    arrange(id)
  
  data <- data %>% 
    group_by(id) %>%
    mutate(
      lag1  = if_else(k != 1, lag(lag0, 1), lag1),
      lag2  = if_else(k != 1, lag(lag1, 1), lag2),
      lag3  = if_else(k != 1, lag(lag2, 1), lag3),
      lag4  = if_else(k != 1, lag(lag3, 1), lag4),
      lag5  = if_else(k != 1, lag(lag4, 1), lag5),
      lag6  = if_else(k != 1, lag(lag5, 1), lag6),
      lag7  = if_else(k != 1, lag(lag6, 1), lag7),
      lag8  = if_else(k != 1, lag(lag7, 1), lag8),
      lag9  = if_else(k != 1, lag(lag8, 1), lag9),
      lag10 = if_else(k != 1, lag(lag9, 1), lag10),
      lag11 = if_else(k != 1, lag(lag10, 1), lag11),
      lag12 = if_else(k != 1, lag(lag11, 1), lag12),
      lag13 = if_else(k != 1, lag(lag12, 1), lag13),
      lag14 = if_else(k != 1, lag(lag13, 1), lag14),
      lag15 = if_else(k != 1, lag(lag14, 1), lag15)
    ) %>%
    ungroup()
  
  
  
 
  return(data)
  
}
