setwd("/n/home00/fyguo/proj_spline/simulation_PersonTime_misspecified_Latency_age_ARIMA_112")
library(tidyverse)
library(ggsci)
# read simulation results



# KnotGridSearch

result_KnotGridSearch <- readRDS("simulation_KnotGridSearch.rds")
result_KnotGridSearch <- result_KnotGridSearch %>% group_by(latency,  l) %>%
  summarise(log_HR_true = mean(log_HR_true),
            log_HR_estimate_mean = mean(log_HR_estimate),
            log_HR_estimate_se = sd(log_HR_estimate))
result_KnotGridSearch <- result_KnotGridSearch %>% filter(latency != 25)
result_KnotGridSearch$method <- "Knot-search"


# TermSearch 
result_TermSearch <- readRDS("simulation_TermSearch.rds")
result_TermSearch <- result_TermSearch %>% group_by(latency,  l) %>%
  summarise(log_HR_true = mean(log_HR_true),
            log_HR_estimate_mean = mean(log_HR_estimate),
            log_HR_estimate_se = sd(log_HR_estimate))
result_TermSearch <- result_TermSearch %>% filter(latency != 25)
result_TermSearch$method <- "Term-Search"



# combine all the results together

temp <- bind_rows(result_KnotGridSearch, result_TermSearch)
temp$latency <- paste0("latency==", temp$latency)

temp$method <- factor(temp$method,
                      levels = c("Knot-search", "Term-Search"))
temp$latency <- as.factor(temp$latency)
ggplot(temp) + 
  geom_line(aes(x = l, y = log_HR_estimate_mean, color = latency, group = latency)) + 
  geom_ribbon(aes(x = l, ymax = log_HR_estimate_mean + 1.96*log_HR_estimate_se , ymin =  log_HR_estimate_mean - 1.96*log_HR_estimate_se, color = latency, fill = latency), alpha = 0.3)+
  facet_grid(latency~method, labeller = label_parsed) + 
  labs(color = "Specified latency length \n True latency is 16",
       fill = "Specified latency length \n True latency is 16",
       linetype = "Specified latency length \n True latency is 16") + 
  scale_color_aaas() + 
  scale_fill_aaas() +
  theme_bw() + 
  labs(x = expression(Time~lag~(l)),
       y = expression(logHR[p](l))) +
  theme(legend.position = "none",
        legend.background = element_blank()) + 
  coord_fixed(ratio = 500)  

width <-  15
height <- width*1
ggsave("Figure4.png", units = "cm", height = height, width = width, dpi = 300)
