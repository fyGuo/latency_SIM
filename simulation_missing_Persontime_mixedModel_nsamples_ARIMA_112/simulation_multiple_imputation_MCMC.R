setwd("/n/home00/fyguo/proj_spline/simulation_missing_Persontime_mixedModel_nsamples_ARIMA_112")

source("simulate_personetime.R")
source("simulation_output.R")
source("imputation_mcmc.R")

library(future)
library(furrr)
library(LatencyA)
library(tictoc)

tic()
# simulate for KnotGridSearch with different knot
temp <- data.frame()

for (knot in c(10, 15)) {
  result <- simulation_output(sample_size = 5000, knot = knot, method  = "NA", latency = 16,
                              missingness_method = "multiple_imputation_mcmc_KnotSearch")
  result$knot <- knot
  temp <- rbind(temp, result)
}

temp$method <- "GridSearchKnot_MI_MCMC"
saveRDS(temp, "simulation_MI_MCMC.rds")

Sys.time()
toc()
#result%>% group_by(l) %>%
#  summarise(log_HR_true = mean(log_HR_true),
#           log_HR_estimate_mean = mean(log_HR_estimate),
#            log_HR_estimate_se = sd(log_HR_estimate)) %>%
#  ggplot() + 
#  geom_line(aes(x = l, y = log_HR_estimate_mean), color = "blue") + 
#  geom_line(aes(x = l, y= log_HR_true), color = "red")


