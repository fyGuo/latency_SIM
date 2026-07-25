setwd("/n/home00/fyguo/proj_spline/simulation_PersonTime_misspecified_Latency_age_ARIMA_112")
source("simulate_personetime.R")
source("simulation_output.R")

library(future)
library(furrr)
library(LatencyA)



library(dplyr)

temp <- list()  # Use a list to store results safely

knot <- 10
for (mis_latency in c(10, 13, 16, 19, 22, 25)) {
  result <- simulation_output(sample_size = 5000, knot = knot, degree = 5, method  = "NCSpline", latency = mis_latency)
  
  if (nrow(result) == 0) {  # If result is an empty data.frame, store a placeholder
    result <- data.frame(sim_id = NA,
               l = NA,
               log_HR_true = NA,
               log_HR_estimate = NA)
  } 
  result$latency <- mis_latency
  temp[[length(temp) + 1]] <- result  # Append to list
}

temp_df <- bind_rows(temp)  # Combine all results into a single data.frame safely
temp_df$method <- "Spline"

saveRDS(temp_df, "simulation_spline.rds")
Sys.time()

#temp %>% group_by(l, latency) %>%
#  summarise(log_HR_true = mean(log_HR_true),
#            log_HR_estimate_mean = mean(log_HR_estimate),
#            log_HR_estimate_se = sd(log_HR_estimate)) %>%
#  ggplot() + 
#  geom_line(aes(x = l, y = log_HR_estimate_mean, color = as.factor(latency), group = latency)) 


