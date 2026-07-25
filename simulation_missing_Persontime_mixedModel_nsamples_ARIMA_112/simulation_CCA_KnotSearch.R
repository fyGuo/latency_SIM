setwd("/n/home00/fyguo/proj_spline/simulation_missing_Persontime_mixedModel_nsamples_ARIMA_112")
source("simulate_personetime.R")
source("simulation_output.R")

library(future)
library(furrr)
library(LatencyA)



# simulate for KnotGridSearch with different knot
temp <- data.frame()

for (knot in c(5, 10, 15)) {
  result <- simulation_output(sample_size = 5000, knot = knot, method  = "GridSearchKnot" , latency = 16,
                              missingness_method = "compete-case")
  result$knot <- knot
  temp <- rbind(temp, result)
}

temp$method <- "GridSearchKnot"
saveRDS(temp, "simulation_CCA_KnotSearch.rds")

Sys.time()

#result%>% group_by(l) %>%
#  summarise(log_HR_true = mean(log_HR_true),
#           log_HR_estimate_mean = mean(log_HR_estimate),
#            log_HR_estimate_se = sd(log_HR_estimate)) %>%
#  ggplot() + 
#  geom_line(aes(x = l, y = log_HR_estimate_mean), color = "blue") + 
#  geom_line(aes(x = l, y= log_HR_true), color = "red")


