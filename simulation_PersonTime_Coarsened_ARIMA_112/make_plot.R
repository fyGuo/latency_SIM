setwd("/n/home00/fyguo/proj_spline/simulation_PersonTime_Coarsened_ARIMA_112")
library(tidyverse)
library(ggsci)

# read simulation results



# KnotGridSearch coarsened data

result_KnotGridSearch <- readRDS("simulation_KnotGridSearch.rds")
result_KnotGridSearch <- result_KnotGridSearch %>% group_by(knot,  l) %>%
  summarise(log_HR_true = mean(log_HR_true, na.rm = T),
            log_HR_estimate_mean = mean(log_HR_estimate, na.rm = T),
            log_HR_estimate_se = sd(log_HR_estimate, na.rm = T))
result_KnotGridSearch$method <- "Grid-search (infrequent measure)"


# Knot GridSearch no coarsened data

result_KnotGridSearch_noC <- readRDS("/n/home00/fyguo/proj_spline/simulation_PersonTime/simulation_KnotGridSearch.rds")
result_KnotGridSearch_noC <- result_KnotGridSearch_noC %>% group_by(knot,  l) %>%
  summarise(log_HR_true = mean(log_HR_true, na.rm = T),
            log_HR_estimate_mean = mean(log_HR_estimate, na.rm = T),
            log_HR_estimate_se = sd(log_HR_estimate, na.rm = T))
result_KnotGridSearch_noC$method <- "Grid-search"

# TermSearch with 5 terms coarsened data
result_TermSearch_5 <- readRDS("simulation_TermSearch5.rds")
result_TermSearch_5 <- result_TermSearch_5 %>% group_by(knot,  l) %>%
  summarise(log_HR_true = mean(log_HR_true),
            log_HR_estimate_mean = mean(log_HR_estimate),
            log_HR_estimate_se = sd(log_HR_estimate))
result_TermSearch_5$method <- "Term-search (infrequent measure)"


# TermSearch no coarsned data
result_TermSearch_noC <- readRDS("/n/home00/fyguo/proj_spline/simulation_PersonTime/simulation_TermSearch.rds")
result_TermSearch_noC <- result_TermSearch_noC %>% group_by(knot,  l) %>%
  summarise(log_HR_true = mean(log_HR_true),
            log_HR_estimate_mean = mean(log_HR_estimate),
            log_HR_estimate_se = sd(log_HR_estimate))
result_TermSearch_noC$method <- "Term-search"


# combine all the results together

temp <- bind_rows(result_KnotGridSearch, result_TermSearch_5, result_KnotGridSearch_noC, result_TermSearch_noC)

temp$knot <- paste0("Shape~varsigma==", temp$knot)
temp$knot <- factor(temp$knot,
                    levels = c("Shape~varsigma==5",
                               "Shape~varsigma==10",
                               "Shape~varsigma==15"))
temp$icc <- 0.2
saveRDS(temp, "data_figure5_02.rds")

ggplot(temp )+ 
  geom_line(aes(x = l, y = log_HR_estimate_mean, color = method, linetype = method)) + 
  geom_line(aes(x = l, y = log_HR_true), color = "black", linetype = "dashed")  + 
  geom_ribbon(aes(x = l, y = log_HR_estimate_mean, 
                  ymin = log_HR_estimate_mean - 1.96*log_HR_estimate_se,
                  ymax = log_HR_estimate_mean + 1.96*log_HR_estimate_se, fill = method), alpha = 0.3) + 
  facet_grid(.~knot, labeller = label_parsed) + 
  theme(legend.position = "bottom") + 
  theme_bw() + 
  scale_color_aaas() + 
  scale_fill_aaas() + 
  labs(x = expression(Time~lag~(l)),
       y = expression(logHR[p](l)), 
       color = "Models",
       fill = "Models",
       linetype = "Models") +
  theme(legend.position = c(0.9, 0.82),
        legend.background = element_blank())


ggsave("figure5.png", units = "cm", height = 15, width = 30, dpi = 300)


