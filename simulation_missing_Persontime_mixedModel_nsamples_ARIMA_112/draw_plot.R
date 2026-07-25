setwd("/n/home00/fyguo/proj_spline/simulation_missing_Persontime_mixedModel_nsamples_ARIMA_112")
library(tidyverse)
library(ggsci)

# read simulation results
# KnotGridSearchwith CCA method

result_CCA_KnotSearch <- readRDS("simulation_CCA_KnotSearch.rds")
result_CCA_KnotSearch <- result_CCA_KnotSearch  %>% group_by(knot,  l) %>%
  summarise(log_HR_true = mean(log_HR_true, na.rm = T),
            log_HR_estimate_mean = mean(log_HR_estimate, na.rm = T),
            log_HR_estimate_se = sd(log_HR_estimate, na.rm = T))
result_CCA_KnotSearch $method <- "Complete-person-time analysis"


#spline with multiple imputation
result_simulation_MI_spline <- readRDS("simulation_MI.rds")
result_simulation_MI_spline <- result_simulation_MI_spline  %>% group_by(knot,  l) %>%
  summarise(log_HR_true = mean(log_HR_true, na.rm = T),
            log_HR_estimate_mean = mean(log_HR_estimate, na.rm = T),
            log_HR_estimate_se = sd(log_HR_estimate, na.rm = T))
result_simulation_MI_spline$method <- "Multiple imputation (mixed effect models)"


#spline with multiple imputation mcmc
result_simulation_MI_mcmc <- readRDS("simulation_MI_MCMC.rds")
result_simulation_MI_mcmc <-result_simulation_MI_mcmc   %>% group_by(knot,  l) %>%
  summarise(log_HR_true = mean(log_HR_true, na.rm = T),
            log_HR_estimate_mean = mean(log_HR_estimate, na.rm = T),
            log_HR_estimate_se = sd(log_HR_estimate, na.rm = T))
result_simulation_MI_mcmc $method <- "Multiple imputation (random forests)"

# impute with 0
result_simulation_0 <- readRDS("simulation_imputation0_KnotSearch.rds")
result_simulation_0 <-result_simulation_0   %>% group_by(knot,  l) %>%
  summarise(log_HR_true = mean(log_HR_true, na.rm = T),
            log_HR_estimate_mean = mean(log_HR_estimate, na.rm = T),
            log_HR_estimate_se = sd(log_HR_estimate, na.rm = T))
result_simulation_0$method <- "Imputation with 0"


# combine all the results together

temp <- bind_rows(result_CCA_KnotSearch, result_simulation_MI_spline, result_simulation_0 , result_simulation_MI_mcmc)

temp$knot <- paste0("Shape~varsigma==", temp$knot)
temp$knot <- factor(temp$knot,
                    levels = c("Shape~varsigma==5",
                               "Shape~varsigma==10",
                               "Shape~varsigma==15"))

temp <- temp %>% filter(knot !="Shape~varsigma==5" )



temp2 <- temp
temp2$log_HR_estimate_mean <- temp2$log_HR_true
temp2$method <- "True latency-risk curve"
temp2$log_HR_estimate_se <- 0
temp <- rbind(temp, temp2)


temp$method <- factor(temp$method, levels = c("True latency-risk curve",
                                              "Complete-person-time analysis",
                                              "Imputation with 0",
                                              "Multiple imputation (mixed effect models)",
                                              "Multiple imputation (random forests)"))


ggplot(temp )+ 
  geom_line(aes(x = l, y = log_HR_estimate_mean, color = method, linetype = method)) + 
  geom_ribbon(aes(x = l, y = log_HR_estimate_mean, 
                  ymin = log_HR_estimate_mean - 1.96*log_HR_estimate_se,
                  ymax = log_HR_estimate_mean + 1.96*log_HR_estimate_se, fill = method), alpha = 0.3) + 
  facet_grid(.~knot, labeller = label_parsed) + 
  theme(legend.position = "bottom") + 
  
  theme_bw() + 
  scale_linetype_manual(values = c("dotted", "solid", "dashed", "twodash", "longdash")) + 
  scale_color_manual(values = c("black", "#3B4992FF", "#631879FF", "#008280FF", "#BB0021FF")) + 
  scale_fill_manual(values = c("black", "#3B4992FF", "#631879FF", "#008280FF", "#BB0021FF")) + 
  labs(x = expression(Time~lag~(l)),
       y = expression(logHR[p](l)), 
       color = "Models",
       fill = "Models",
       linetype = "Models") +
  theme(legend.position = "bottom",
        legend.background = element_blank())+ 
  coord_fixed(ratio = 200)  

width <-  30
height <- width*0.5

ggsave("figure6.png", units = "cm", height = height, width = width, dpi = 300)


