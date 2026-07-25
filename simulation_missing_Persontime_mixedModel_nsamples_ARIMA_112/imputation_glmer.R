# this R code is to define a imputation model based on mle

library(Hmisc)
library(lme4)

imputation_glmer <- function(data) {
  # for each person time let's select the current expousre(lag0)
  tmp <- data  %>% dplyr::select(age_start, lag0, id)
  
  # here we model the exposre as a function of age_start 
  model <- lmer(lag0 ~ age_start + (1|id) , data = tmp)
  
  # for each person at baseline, we need to impute 16 years' day, which is four points 
  residual_sd <- summary(model)$sigma
  # let's first extract the person-time that enters the study
  baseline_data <- tmp  %>% group_by(id) %>%
    dplyr::filter(row_number()== 1) %>%
    ungroup()

  # then let's impute the baseline data 
  lag1_data  <- baseline_data %>% dplyr::mutate(age_start = age_start - 1)
  lag2_data  <- baseline_data %>% dplyr::mutate(age_start = age_start - 2)
  lag3_data  <- baseline_data %>% dplyr::mutate(age_start = age_start - 3)
  lag4_data  <- baseline_data %>% dplyr::mutate(age_start = age_start - 4)
  lag5_data  <- baseline_data %>% dplyr::mutate(age_start = age_start - 5)
  lag6_data  <- baseline_data %>% dplyr::mutate(age_start = age_start - 6)
  lag7_data  <- baseline_data %>% dplyr::mutate(age_start = age_start - 7)
  lag8_data  <- baseline_data %>% dplyr::mutate(age_start = age_start - 8)
  lag9_data  <- baseline_data %>% dplyr::mutate(age_start = age_start - 9)
  lag10_data <- baseline_data %>% dplyr::mutate(age_start = age_start -10)
  lag11_data <- baseline_data %>% dplyr::mutate(age_start = age_start -11)
  lag12_data <- baseline_data %>% dplyr::mutate(age_start = age_start -12)
  lag13_data <- baseline_data %>% dplyr::mutate(age_start = age_start -13)
  lag14_data <- baseline_data %>% dplyr::mutate(age_start = age_start -14)
  lag15_data <- baseline_data %>% dplyr::mutate(age_start = age_start -15)
  
  
  # the prediction has accounted for the random effect
  lag1_data$lag0  <- predict(model, lag1_data)  + rnorm(nrow(lag1_data),  mean = 0, sd = residual_sd)
  lag2_data$lag0  <- predict(model, lag2_data)  + rnorm(nrow(lag2_data),  mean = 0, sd = residual_sd)
  lag3_data$lag0  <- predict(model, lag3_data)  + rnorm(nrow(lag3_data),  mean = 0, sd = residual_sd)
  lag4_data$lag0  <- predict(model, lag4_data)  + rnorm(nrow(lag4_data),  mean = 0, sd = residual_sd)
  lag5_data$lag0  <- predict(model, lag5_data)  + rnorm(nrow(lag5_data),  mean = 0, sd = residual_sd)
  lag6_data$lag0  <- predict(model, lag6_data)  + rnorm(nrow(lag6_data),  mean = 0, sd = residual_sd)
  lag7_data$lag0  <- predict(model, lag7_data)  + rnorm(nrow(lag7_data),  mean = 0, sd = residual_sd)
  lag8_data$lag0  <- predict(model, lag8_data)  + rnorm(nrow(lag8_data),  mean = 0, sd = residual_sd)
  lag9_data$lag0  <- predict(model, lag9_data)  + rnorm(nrow(lag9_data),  mean = 0, sd = residual_sd)
  lag10_data$lag0 <- predict(model, lag10_data) + rnorm(nrow(lag10_data), mean = 0, sd = residual_sd)
  lag11_data$lag0 <- predict(model, lag11_data) + rnorm(nrow(lag11_data), mean = 0, sd = residual_sd)
  lag12_data$lag0 <- predict(model, lag12_data) + rnorm(nrow(lag12_data), mean = 0, sd = residual_sd)
  lag13_data$lag0 <- predict(model, lag13_data) + rnorm(nrow(lag13_data), mean = 0, sd = residual_sd)
  lag14_data$lag0 <- predict(model, lag14_data) + rnorm(nrow(lag14_data), mean = 0, sd = residual_sd)
  lag15_data$lag0 <- predict(model, lag15_data) + rnorm(nrow(lag15_data), mean = 0, sd = residual_sd)
  
  # rename all the lag variables
  names(lag1_data)[names(lag1_data) == "lag0"] <- "lag1"
  names(lag2_data)[names(lag2_data) == "lag0"] <- "lag2"
  names(lag3_data)[names(lag3_data) == "lag0"] <- "lag3"
  names(lag4_data)[names(lag4_data) == "lag0"] <- "lag4"
  names(lag5_data)[names(lag5_data) == "lag0"] <- "lag5"
  names(lag6_data)[names(lag6_data) == "lag0"] <- "lag6"
  names(lag7_data)[names(lag7_data) == "lag0"] <- "lag7"
  names(lag8_data)[names(lag8_data) == "lag0"] <- "lag8"
  names(lag9_data)[names(lag9_data) == "lag0"] <- "lag9"
  names(lag10_data)[names(lag10_data) == "lag0"] <- "lag10"
  names(lag11_data)[names(lag11_data) == "lag0"] <- "lag11"
  names(lag12_data)[names(lag12_data) == "lag0"] <- "lag12"
  names(lag13_data)[names(lag13_data) == "lag0"] <- "lag13"
  names(lag14_data)[names(lag14_data) == "lag0"] <- "lag14"
  names(lag15_data)[names(lag15_data) == "lag0"] <- "lag15"
  
  # exclude age_start to avoid bugs
  lag1_data  <- lag1_data  %>% dplyr::select(-age_start)
  lag2_data  <- lag2_data  %>% dplyr::select(-age_start)
  lag3_data  <- lag3_data  %>% dplyr::select(-age_start)
  lag4_data  <- lag4_data  %>% dplyr::select(-age_start)
  lag5_data  <- lag5_data  %>% dplyr::select(-age_start)
  lag6_data  <- lag6_data  %>% dplyr::select(-age_start)
  lag7_data  <- lag7_data  %>% dplyr::select(-age_start)
  lag8_data  <- lag8_data  %>% dplyr::select(-age_start)
  lag9_data  <- lag9_data  %>% dplyr::select(-age_start)
  lag10_data <- lag10_data %>% dplyr::select(-age_start)
  lag11_data <- lag11_data %>% dplyr::select(-age_start)
  lag12_data <- lag12_data %>% dplyr::select(-age_start)
  lag13_data <- lag13_data %>% dplyr::select(-age_start)
  lag14_data <- lag14_data %>% dplyr::select(-age_start)
  lag15_data <- lag15_data %>% dplyr::select(-age_start)
  
  baseline_data <- full_join(baseline_data, lag1_data, by = "id") %>%
    full_join(lag2_data,  by = "id") %>%
    full_join(lag3_data,  by = "id") %>%
    full_join(lag4_data,  by = "id") %>%
    full_join(lag5_data,  by = "id") %>%
    full_join(lag6_data,  by = "id") %>%
    full_join(lag7_data,  by = "id") %>%
    full_join(lag8_data,  by = "id") %>%
    full_join(lag9_data,  by = "id") %>%
    full_join(lag10_data, by = "id") %>%
    full_join(lag11_data, by = "id") %>%
    full_join(lag12_data, by = "id") %>%
    full_join(lag13_data, by = "id") %>%
    full_join(lag14_data, by = "id") %>%
    full_join(lag15_data, by = "id")
  

  # last update the whole data frame with the imputed baseline data
  # first let's exlcude the lag1 to lag19 columns and regenerate them
  data <- data %>% dplyr::select(-paste0("lag", 1:19))
  data <- left_join(data, baseline_data) %>%
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
