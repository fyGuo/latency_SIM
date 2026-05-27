setwd("~/proj_spline/simulation_Persontime_ARIMA_112")
library(tidyverse)
library(ggsci)
library(ggpubr)
########################################
# make figure 2

# knot search models
result_knot <- readRDS("simulation_KnotGridSearch.rds")

result_knot <- result_knot %>% group_by(knot,  l) %>%
  summarise(log_HR_true = mean(log_HR_true),
            log_HR_estimate_mean = mean(log_HR_estimate),
            log_HR_estimate_se = sd(log_HR_estimate))

result_knot$method <- "Knot-search RCspline model"

# term search models
result_term <- readRDS("simulation_TermSearch.rds")
result_term <- result_term %>% group_by(knot, l) %>%
  summarise(log_HR_true = mean(log_HR_true),
            log_HR_estimate_mean = mean(log_HR_estimate),
            log_HR_estimate_se = sd(log_HR_estimate))
result_term$method <- "Term-search RCspline model"

# restricted cubic splines with 5 degrees of freedom

result_NCS <- readRDS("simulation_NCSpline.rds")
result_NCS <- result_NCS %>% filter(degree == 5) %>%
  group_by(knot, l) %>%
  summarise(log_HR_true = mean(log_HR_true),
            log_HR_estimate_mean = mean(log_HR_estimate),
            log_HR_estimate_se = sd(log_HR_estimate))
result_NCS$method <- "Spline model with df = 5"



# polynomial with 5 degrees of freedom

result_poly <- readRDS("simulation_Polynomial.rds")
result_poly <- result_poly  %>% filter(degree == 5) %>%
  group_by(knot, l) %>%
  summarise(log_HR_true = mean(log_HR_true),
            log_HR_estimate_mean = mean(log_HR_estimate),
            log_HR_estimate_se = sd(log_HR_estimate))
result_poly$method <- "Polynomial model with df = 5"

temp <- rbind(result_knot, result_term,  result_poly)

temp$knot <- paste0("Shape~varsigma==", temp$knot)
temp$knot <- factor(temp$knot,
                    levels = c("Shape~varsigma==5",
                               "Shape~varsigma==10",
                               "Shape~varsigma==15"))
temp$method <- factor(temp$method,
                      levels = c("Knot-search RCspline model", "Term-search RCspline model", "Spline model with df = 5", "Polynomial model with df = 5"))

temp2 <- temp
temp2$log_HR_estimate_mean <- temp2$log_HR_true
temp2$method <- "True latency-risk curve"
temp2$log_HR_estimate_se <- 0
temp <- rbind(temp, temp2)

temp$icc <- 0.2

temp$method <- factor(temp$method,
                    levels = c("True latency-risk curve", "Knot-search RCspline model", "Term-search RCspline model", "Polynomial model with df = 5", "Spline model with df = 5"))

saveRDS(temp, "data_figure2_02.rds")


whole_plot <- ggplot(temp) + 
  geom_line(aes(x = l, y = log_HR_estimate_mean, color = method, linetype = method)) + 
  geom_point(aes(x = l, y = log_HR_estimate_mean, shape = method,  color = method,)) +
  facet_grid(.~knot, labeller = label_parsed) + 
  geom_ribbon(aes(x = l, ymin = log_HR_estimate_mean-1.96*log_HR_estimate_se, ymax =log_HR_estimate_mean+1.96*log_HR_estimate_se, 
                  fill = method), alpha = 0.2)  + 
  scale_linetype_manual(values = c("dotted", "solid", "dashed", "twodash")) + 
  scale_color_manual(values = c("black", "#3B4992FF", "#DF8F44FF", "#BB0021FF")) + 
  scale_fill_manual(values = c("black", "#3B4992FF", "#DF8F44FF", "#BB0021FF")) + 
  theme_bw() +
  labs(x = expression(Time~lag~(l)),
       y = expression(logHR[p](l)),
       color = "Models",
       fill = "Models",
       linetype = "Models",
       shape = "Models") + 
  theme(legend.position = "none",
        legend.background = element_blank())
#########################
# zoom up the tail when shape = 5
tail_plot <- ggplot(temp) + 
  geom_line(aes(x = l, y = log_HR_estimate_mean, color = method, linetype = method)) + 
  geom_point(aes(x = l, y = log_HR_estimate_mean, shape = method,  color = method,)) +
  geom_ribbon(aes(x = l, ymin = log_HR_estimate_mean-1.96*log_HR_estimate_se, ymax =log_HR_estimate_mean+1.96*log_HR_estimate_se, 
                  fill = method), alpha = 0.2)  + 
  facet_grid(.~knot, labeller = label_parsed) + 
  scale_linetype_manual(values = c("dotted", "solid", "dashed", "twodash")) + 
  scale_color_manual(values = c("black", "#3B4992FF", "#DF8F44FF", "#BB0021FF")) + 
  scale_fill_manual(values = c("black", "#3B4992FF", "#DF8F44FF", "#BB0021FF")) + 
  theme_bw() +
  labs(x = expression(Time~lag~(l)),
       y = expression(logHR[p](l)),
       color = "Models",
       fill = "Models",
       linetype = "Models",
       shape = "Models") + 
  theme(legend.position = "bottom",
        legend.background = element_blank())+
  coord_cartesian(xlim = c(10, 15), ylim = c(-0.002, 0.005))
ggarrange(whole_plot, tail_plot, nrow = 2, labels = c("A", "B"))

width <-  23
height <- width*0.8
ggsave("figure2.png", units = "cm", width =width, height = height , dpi = 300)
