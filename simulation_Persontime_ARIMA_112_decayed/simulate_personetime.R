library(tidyverse)
library(survival)

# This code is to simulate person time data 
library(splines)

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

# this is to write an alpha function to calculate the weighted cumulative exposure
# exponential decay: A * exp(-B * l)
# B controls the decay rate; shape conditions: B=0.4 (fast), B=0.2 (medium), B=0.1 (slow)
alpha_function <- function(l, A = 0.3, B = 0.2){
  A * exp(-B * l)
}

# This function is to generate the simualton data
generate_data <- function(sample_size, beta = 0.01, rho = 0.3, lambda = 0.5, A = 0.3, B = 0.2,
                          lambda_0 = 0.1, gamma = 0.1){

# first generate the entry age from a discrete uniform distribution from 50 to 70
entry_age <- sample(50, size = sample_size, replace = TRUE)

# make a data.frame to store each person's information
df <- data.frame(
  person_id = 1:sample_size,
  entry_age = entry_age)

tmp <- data.frame()
# for each individual, generate their past 30 years and future 20 years history of exposure
for (i in 1:max(df$person_id)){
  df_person <- data.frame(age = (df$entry_age[i]-30):(df$entry_age[i]+20))
  df_person$exposure <- exposure_function(df_person$age, sigma = 20, beta = 0.01, rho = rho, lambda = lambda)
  
  # we will generate 30 lag terms for each age
  df_person <- df_person %>% mutate(lag1 = lag(exposure, 1),
                       lag2 = lag(exposure, 2),
                       lag3 = lag(exposure, 3),
                       lag4 = lag(exposure, 4),
                       lag5 = lag(exposure, 5),
                       lag6 = lag(exposure, 6),
                       lag7 = lag(exposure, 7),
                        lag8 = lag(exposure, 8),
                        lag9 = lag(exposure, 9),
                        lag10 = lag(exposure, 10),
                        lag11 = lag(exposure, 11),
                        lag12 = lag(exposure, 12),
                        lag13 = lag(exposure, 13),
                        lag14 = lag(exposure, 14),
                        lag15 = lag(exposure, 15),
                        lag16 = lag(exposure, 16),
                        lag17 = lag(exposure, 17),
                        lag18 = lag(exposure, 18),
                        lag19 = lag(exposure, 19),
                        lag20 = lag(exposure, 20),
                        lag21 = lag(exposure, 21),
                        lag22 = lag(exposure, 22),
                        lag23 = lag(exposure, 23),
                        lag24 = lag(exposure, 24),
                        lag25 = lag(exposure, 25),
                        lag26 = lag(exposure, 26),
                        lag27 = lag(exposure, 27),
                        lag28 = lag(exposure, 28),
                        lag29 = lag(exposure, 29),
                        lag30 = lag(exposure, 30)) 
  # we rename the current exposure value to lag0
  names(df_person)[names(df_person) == "exposure"] <- "lag0"

  # only keep data from row 31 because that's the entry age
  df_person <- df_person[31:50,]

  # assign person id to the person
  df_person$id <- i

  # next step, we calculate the weighted cumulative exposure and check if the survival time at each
  # age is greater than 1. If it is greater than 1, we keep following the person
  # otherwise, we stop following the person
  # we will use an exponential distribution to generate the time-varying survival time 
  df_person$cumExposure <- as.matrix(df_person[,paste0("lag", 0:15)]) %*% alpha_function(0:15, A = A, B = B)
  df_person <- df_person[seq(1, dim(df_person)[1], 1),]
  
  df_person$lambda <- lambda_0*exp(gamma * df_person$cumExposure)

  df_person$failure_time <- rexp(dim(df_person)[1], rate = df_person$lambda)
  df_person$failure <- if_else(df_person$failure_time > 1, 0, 1)

  # check when the first failure occurs
  first_failure_row <- which(df_person$failure == 1)[1]

  # if there is no failure, just use the nrow of the data.frame
  first_failure_row <- if_else(is.na(first_failure_row), dim(df_person)[1], first_failure_row)
  # cut data.frame at the first failure
  df_person <- df_person[1:first_failure_row,]
  tmp <- rbind(tmp, df_person)
}
  # here we rename the age as start_age and generate a end age. If failure = 0 then age_end = age_start + 1, if failure = 1
  # then age_end = age_start + failure_time

  names(tmp)[names(tmp) == "age"] <- "age_start"
 # tmp <- tmp %>% mutate(failure_time = if_else(failure_time <= 0.000001,  0.000001, failure_time))
  tmp$age_end <- if_else(tmp$failure == 0, tmp$age_start + 0.999999, tmp$age_start + tmp$failure_time)

  return(tmp)
}

#sim_data <- generate_data(1000)
#knot <- 5
#X <- as.matrix(sim_data[,paste0("lag", 0:19)]) %*%  bs(0:19, knots = knot, Boundary.knots = c(-1, 20)) 

#fit <- coxph(Surv(sim_data$age_start, sim_data$age_end, sim_data$failure) ~ X)
#plot( bs(0:19, knots = knot, Boundary.knots = c(0, 20)) %*% c(alpha1, alpha2, alpha3, alpha4), col = "blue")
#points(bs(0:19, knots = knot, Boundary.knots = c(0, 20)) %*% (coef(fit)/0.1), col = "red")
