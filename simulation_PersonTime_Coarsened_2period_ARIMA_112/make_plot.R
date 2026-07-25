setwd("~/proj_spline/simulation_PersonTime_Coarsened_2period_ARIMA_112")
library(tidyverse)
library(ggsci)

# read simulation results

# KnotSearch results without missingness
result_KnotGridSearch <- readRDS("/n/home00/fyguo/proj_spline/simulation_Persontime_ARIMA_112/simulation_KnotGridSearch.rds")
result_KnotGridSearch <- result_KnotGridSearch  %>% group_by(knot,  l) %>%
  summarise(log_HR_true = mean(log_HR_true, na.rm = T),
            log_HR_estimate_mean = mean(log_HR_estimate, na.rm = T),
            log_HR_estimate_se = sd(log_HR_estimate, na.rm = T))
result_KnotGridSearch$method <- "Measured every period"




# KnotGridSearch coarsened data

result_KnotGridSearch_2period <- readRDS("simulation_KnotGridSearch.rds")
result_KnotGridSearch_2period<- result_KnotGridSearch_2period %>% group_by(knot,  l) %>%
  summarise(log_HR_true = mean(log_HR_true, na.rm = T),
            log_HR_estimate_mean = mean(log_HR_estimate, na.rm = T),
            log_HR_estimate_se = sd(log_HR_estimate, na.rm = T))
result_KnotGridSearch_2period$method <- "Measured every two periods"


# Knot GridSearch no coarsened data

result_KnotGridSearch_4period <- readRDS("/n/home00/fyguo/proj_spline/simulation_PersonTime_Coarsened_ARIMA_112/simulation_KnotGridSearch.rds")
result_KnotGridSearch_4period <- result_KnotGridSearch_4period  %>% group_by(knot,  l) %>%
  summarise(log_HR_true = mean(log_HR_true, na.rm = T),
            log_HR_estimate_mean = mean(log_HR_estimate, na.rm = T),
            log_HR_estimate_se = sd(log_HR_estimate, na.rm = T))
result_KnotGridSearch_4period$method <- "Measured every four periods"





# combine all the results together

temp <- bind_rows(result_KnotGridSearch, result_KnotGridSearch_2period , result_KnotGridSearch_4period)

temp$knot <- paste0("Shape~varsigma==", temp$knot)
temp$knot <- factor(temp$knot,
                    levels = c("Shape~varsigma==5",
                               "Shape~varsigma==10",
                               "Shape~varsigma==15"))


temp2 <- temp
temp2$log_HR_estimate_mean <- temp2$log_HR_true
temp2$method <- "True latency-risk curve"
temp2$log_HR_estimate_se <- 0
temp <- rbind(temp, temp2)

temp$method <- factor(temp$method, levels = c("True latency-risk curve",  "Measured every period", "Measured every two periods",
                                              "Measured every four periods"))

temp <- temp %>% filter(knot != "Shape~varsigma==5")
ggplot(temp )+ 
  geom_line(aes(x = l, y = log_HR_estimate_mean, color = method, linetype = method)) + 
  geom_ribbon(aes(x = l, y = log_HR_estimate_mean, 
                  ymin = log_HR_estimate_mean - 1.96*log_HR_estimate_se,
                  ymax = log_HR_estimate_mean + 1.96*log_HR_estimate_se, fill = method), alpha = 0.3) + 
  facet_grid(.~knot, labeller = label_parsed) + 
  theme(legend.position = "bottom") + 
  theme_bw() + 
  scale_linetype_manual(values = c("dotted", "solid", "dashed", "twodash")) + 
  scale_color_manual(values = c("black", "#3B4992FF", "#008B45FF", "#BB0021FF")) + 
  scale_fill_manual(values = c("black", "#3B4992FF", "#008B45FF", "#BB0021FF")) + 
  labs(x = expression(Time~lag~(l)),
       y = expression(logHR[p](l)), 
       color = "Models",
       fill = "Models",
       linetype = "Models") +
  theme(legend.position = "bottom",
        legend.background = element_blank())+ 
  coord_fixed(ratio = 500)  


ggsave("figure5.png", units = "cm", height = 15, width = 30, dpi = 300)


