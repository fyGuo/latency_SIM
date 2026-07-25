# Data-generating functions copied from simulation_Persontime_ARIMA_112 (the
# hard-coded setwd() and trailing scratch code were removed so this can be sourced
# safely). The ARIMA(1,1,2)-type exposure process and weighting curve are unchanged.
library(tidyverse)
library(survival)
library(splines)

# time-varying exposure level as a function of age (ARIMA(1,1,2)-style noise)
exposure_function <- function(age, sigma, beta, rho, lambda) {
  exposure   <- numeric(length(age))
  random_eta <- numeric(length(age))
  epsilon    <- numeric(length(age))
  for (i in 1:length(age)) {
    random_eta[i] <- rnorm(n = 1, sd = 10)
    if (i == 1) {
      epsilon[i] <- random_eta[i]
    } else if (i >= 2) {
      epsilon[i] <- 0.67 * epsilon[i - 1] + random_eta[i] + 0.18 * random_eta[i - 1]
    }
    exposure[i] <- (age[i] - 70) + epsilon[i]
  }
  return(exposure)
}

# weighting curve for the cumulative exposure (B-spline)
alpha_function <- function(l, alpha1 = 0.3, alpha2 = -0.01, alpha3 = 0.01, alpha4 = 0, knot = 5) {
  weights <- bs(l, knots = knot, Boundary.knots = c(0, 16)) %*% c(alpha1, alpha2, alpha3, alpha4)
  return(weights)
}

# generate the person-time simulation data
generate_data <- function(sample_size, beta = 0.01, rho = 0.3, lambda = 0.5, alpha1 = 0.3, alpha2 = -0.01,
                          alpha3 = 0.01, alpha4 = 0, knot = 5,
                          lambda_0 = 0.1, gamma = 0.1) {

  # entry age from a discrete uniform distribution on 50..70 (so the trajectory's
  # age axis stays positive: ages run from entry-30 to entry+20)
  entry_age <- sample(50:70, size = sample_size, replace = TRUE)
  df <- data.frame(person_id = 1:sample_size, entry_age = entry_age)

  tmp <- data.frame()
  for (i in 1:max(df$person_id)) {
    df_person <- data.frame(age = (df$entry_age[i] - 30):(df$entry_age[i] + 20))
    df_person$exposure <- exposure_function(df_person$age, sigma = 20, beta = 0.01, rho = rho, lambda = lambda)

    for (k in 1:30) df_person[[paste0("lag", k)]] <- dplyr::lag(df_person$exposure, k)
    names(df_person)[names(df_person) == "exposure"] <- "lag0"

    df_person <- df_person[31:50, ]
    df_person$id <- i

    df_person$cumExposure <- as.matrix(df_person[, paste0("lag", 0:15)]) %*%
      alpha_function(0:15, alpha1 = alpha1, alpha2 = alpha2, alpha3 = alpha3, alpha4 = alpha4, knot = knot)

    df_person$lambda       <- lambda_0 * exp(gamma * df_person$cumExposure)
    df_person$failure_time <- rexp(dim(df_person)[1], rate = df_person$lambda)
    df_person$failure      <- if_else(df_person$failure_time > 1, 0, 1)

    first_failure_row <- which(df_person$failure == 1)[1]
    first_failure_row <- if_else(is.na(first_failure_row), dim(df_person)[1], first_failure_row)
    df_person <- df_person[1:first_failure_row, ]
    tmp <- rbind(tmp, df_person)
  }

  names(tmp)[names(tmp) == "age"] <- "age_start"
  tmp$age_end <- if_else(tmp$failure == 0, tmp$age_start + 0.999999, tmp$age_start + tmp$failure_time)
  return(tmp)
}
