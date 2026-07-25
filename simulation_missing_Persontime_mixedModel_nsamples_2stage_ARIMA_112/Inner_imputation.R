# this R code is to define a imputation model based on mle

library(Hmisc)
library(lme4)

inner_imputation <- function(data) {
  # for each person time let's select the current expousre(lag0)
  tmp <- data  %>% dplyr::select(age_start, lag0, id)
  
  # here we model the exposure as a function of age_start 
  model <- lmer(lag0 ~ age_start + (1|id) , data = tmp)
  
  # for each person at baseline, we need to impute 16 years' day, which is four points 
  residual_sd <- summary(model)$sigma
  
  tmp1 <- tmp %>% dplyr::mutate(origin = 1)
  tmp2 <- tmp %>% dplyr::mutate(age_start = age_start+1, lag0 = NA, origin = 0)
  newdata <- base::rbind(tmp1, tmp2) %>% dplyr::arrange(id, age_start)
  
  
  newdata$lag0_new <- predict(model, newdata) + rnorm(dim(newdata)[1], mean = 0, sd= residual_sd)
  newdata <- newdata %>% dplyr::mutate(lag0 = if_else(is.na(lag0), lag0_new, lag0))
  
  # let's first extract the person-time that enters the study
  baseline_data <- tmp  %>% group_by(id) %>%
    dplyr::filter(row_number()== 1) %>%
    ungroup()
  
  data <- full_join(newdata, data, by = c("age_start", "lag0", "id"))

  data <- data %>%
    group_by(id) %>%
    mutate(
      k = row_number(),
      lag1 = ifelse(k == 1, lag1, lag(lag0, 1)),
      lag2 = ifelse(k == 1, lag2, lag(lag0, 2)),
      lag3 = ifelse(k == 1, lag3, lag(lag0, 3)),
      lag4 = ifelse(k == 1, lag4, lag(lag0, 4)),
      lag5 = ifelse(k == 1, lag5, lag(lag0, 5)),
      lag6 = ifelse(k == 1, lag6, lag(lag0, 6)),
      lag7 = ifelse(k == 1, lag7, lag(lag0, 7)),
      lag8 = ifelse(k == 1, lag8, lag(lag0, 8)),
      lag9 = ifelse(k == 1, lag9, lag(lag0, 9)),
      lag10 = ifelse(k == 1, lag10, lag(lag0, 10)),
      lag11 = ifelse(k == 1, lag11, lag(lag0, 11)),
      lag12 = ifelse(k == 1, lag12, lag(lag0, 12)),
      lag13 = ifelse(k == 1, lag13, lag(lag0, 13)),
      lag14 = ifelse(k == 1, lag14, lag(lag0, 14)),
      lag15 = ifelse(k == 1, lag15, lag(lag0, 15)),
      lag16 = ifelse(k == 1, lag16, lag(lag0, 16))
    ) %>% 
    filter(origin == 1) %>%
    dplyr::select(-lag0_new) %>% 
    ungroup()
  
  return(data)
  
}
