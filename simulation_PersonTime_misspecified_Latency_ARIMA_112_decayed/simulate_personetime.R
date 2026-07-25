library(tidyverse)
library(survival)
library(splines)

exposure_function <- function(age, sigma, beta, rho, lambda) {
  exposure   <- numeric(length(age))
  random_eta <- numeric(length(age))
  epsilon    <- numeric(length(age))
  for (i in 1:length(age)) {
    random_eta[i] <- rnorm(n = 1, sd = 10)
    if (i == 1) {
      epsilon[i] <- random_eta[i]
    } else {
      epsilon[i] <- 0.67*epsilon[i-1] + random_eta[i] + 0.18*random_eta[i-1]
    }
    exposure[i] <- (age[i] - 70) + epsilon[i]
  }
  return(exposure)
}

# Damped exponential weighting: A * exp(-B * l).
# B controls the decay rate; shape conditions: B=0.4 (fast), B=0.2 (medium).
alpha_function <- function(l, A = 0.3, B = 0.2){
  A * exp(-B * l)
}

generate_data <- function(sample_size, beta = 0.01, rho = 0.3, lambda = 0.5,
                          A = 0.3, B = 0.2,
                          lambda_0 = 0.1, gamma = 0.1){

  entry_age <- sample(50, size = sample_size, replace = TRUE)
  df <- data.frame(person_id = 1:sample_size, entry_age = entry_age)

  tmp <- data.frame()
  for (i in 1:max(df$person_id)){
    df_person <- data.frame(age = (df$entry_age[i]-30):(df$entry_age[i]+20))
    df_person$exposure <- exposure_function(df_person$age, sigma = 20,
                                            beta = 0.01, rho = rho, lambda = lambda)

    df_person <- df_person %>% mutate(
      lag1  = lag(exposure,  1), lag2  = lag(exposure,  2), lag3  = lag(exposure,  3),
      lag4  = lag(exposure,  4), lag5  = lag(exposure,  5), lag6  = lag(exposure,  6),
      lag7  = lag(exposure,  7), lag8  = lag(exposure,  8), lag9  = lag(exposure,  9),
      lag10 = lag(exposure, 10), lag11 = lag(exposure, 11), lag12 = lag(exposure, 12),
      lag13 = lag(exposure, 13), lag14 = lag(exposure, 14), lag15 = lag(exposure, 15),
      lag16 = lag(exposure, 16), lag17 = lag(exposure, 17), lag18 = lag(exposure, 18),
      lag19 = lag(exposure, 19), lag20 = lag(exposure, 20), lag21 = lag(exposure, 21),
      lag22 = lag(exposure, 22), lag23 = lag(exposure, 23), lag24 = lag(exposure, 24),
      lag25 = lag(exposure, 25), lag26 = lag(exposure, 26), lag27 = lag(exposure, 27),
      lag28 = lag(exposure, 28), lag29 = lag(exposure, 29), lag30 = lag(exposure, 30))

    names(df_person)[names(df_person) == "exposure"] <- "lag0"
    df_person <- df_person[31:50, ]
    df_person$id <- i

    df_person$cumExposure <- as.matrix(df_person[, paste0("lag", 0:15)]) %*%
      alpha_function(0:15, A = A, B = B)

    df_person$lambda       <- lambda_0 * exp(gamma * df_person$cumExposure)
    df_person$failure_time <- rexp(nrow(df_person), rate = df_person$lambda)
    df_person$failure      <- if_else(df_person$failure_time > 1, 0, 1)

    first_failure_row <- which(df_person$failure == 1)[1]
    first_failure_row <- if_else(is.na(first_failure_row), nrow(df_person), first_failure_row)
    df_person <- df_person[1:first_failure_row, ]
    tmp <- rbind(tmp, df_person)
  }

  names(tmp)[names(tmp) == "age"] <- "age_start"
  tmp$age_end <- if_else(tmp$failure == 0, tmp$age_start + 0.999999,
                         tmp$age_start + tmp$failure_time)
  return(tmp)
}
