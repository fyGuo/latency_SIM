# please change the following directory to your own directory
library(tidyverse)
library(survival)
library(splines)

# This code simulates person-time data for the permutation-test study. Exposure
# histories follow the same ARIMA(1,1,2) process as the main simulations; the
# only generalisation is that the latency weighting curve is supplied directly
# as a vector `weights` (length = latency), so any scenario from weight_curves.R
# can be generated.

# write a function to generate the time-varying exposure level with age
exposure_function <- function(age, sigma, beta, rho, lambda) {
  exposure <- numeric(length(age))
  random_eta <- numeric(length(age))
  epsilon <- numeric(length(age))
  for (i in 1:length(age)) {
    random_eta[i] <- rnorm(n = 1, sd = 10)
    if (i == 1) {
      epsilon[i] <- random_eta[i]
    } else if(i >= 2) {
      epsilon[i] <- 0.67*epsilon[i-1] + random_eta[i] + 0.18*random_eta[i-1]
    }
    exposure[i] <- (age[i] - 70) + epsilon[i]
  }
  return(exposure)
}

# This function generates the simulation data under an arbitrary weighting curve.
#   weights  : numeric vector of length `latency`, the true weight at lags 0..L-1
#   intercept: baseline log-hazard shift (larger -> more events)
generate_data <- function(sample_size, weights, beta = 0.01, rho = 0.3, lambda = 0.5,
                          lambda_0 = 0.5, gamma = 0.1, intercept = 0, latency = 16){

  stopifnot(length(weights) == latency)

  # first generate the entry age from a discrete uniform distribution
  entry_age <- sample(50, size = sample_size, replace = TRUE)

  df <- data.frame(person_id = 1:sample_size, entry_age = entry_age)

  tmp <- data.frame()
  # for each individual, generate their past 30 years and future 20 years of exposure
  for (i in 1:max(df$person_id)){
    df_person <- data.frame(age = (df$entry_age[i]-30):(df$entry_age[i]+20))
    df_person$exposure <- exposure_function(df_person$age, sigma = 20, beta = 0.01, rho = rho, lambda = lambda)

    # generate 30 lag terms for each age
    for (k in 1:30) df_person[[paste0("lag", k)]] <- dplyr::lag(df_person$exposure, k)
    # rename the current exposure value to lag0
    names(df_person)[names(df_person) == "exposure"] <- "lag0"

    # only keep data from row 31 because that's the entry age
    df_person <- df_person[31:50,]
    df_person$id <- i

    # weighted cumulative exposure using the supplied weighting curve
    df_person$cumExposure <- as.matrix(df_person[, paste0("lag", 0:(latency-1))]) %*% weights

    # `intercept` shifts the baseline log-hazard (more events for larger values)
    df_person$lambda <- lambda_0*exp(intercept + gamma * df_person$cumExposure)

    df_person$failure_time <- rexp(dim(df_person)[1], rate = df_person$lambda)
    df_person$failure <- if_else(df_person$failure_time > 1, 0, 1)

    # cut the follow-up at the first failure
    first_failure_row <- which(df_person$failure == 1)[1]
    first_failure_row <- if_else(is.na(first_failure_row), dim(df_person)[1], first_failure_row)
    df_person <- df_person[1:first_failure_row,]
    tmp <- rbind(tmp, df_person)
  }

  names(tmp)[names(tmp) == "age"] <- "age_start"
  tmp$age_end <- if_else(tmp$failure == 0, tmp$age_start + 0.999999, tmp$age_start + tmp$failure_time)

  return(tmp)
}
