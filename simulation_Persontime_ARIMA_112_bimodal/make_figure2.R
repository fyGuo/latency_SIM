setwd(tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) getwd()
))
library(tidyverse)
library(ggsci)
library(ggpubr)
########################################
# make figure 2

# knot search models
result_knot <- readRDS("simulation_KnotGridSearch.rds")

result_knot <- result_knot %>% group_by(B, l) %>%
  summarise(log_HR_true = mean(log_HR_true),
            log_HR_estimate_mean = mean(log_HR_estimate),
            log_HR_estimate_se = sd(log_HR_estimate))

result_knot$method <- "Knot-search RCspline model"

# term search models
result_term <- readRDS("simulation_TermSearch.rds")
result_term <- result_term %>% group_by(B, l) %>%
  summarise(log_HR_true = mean(log_HR_true),
            log_HR_estimate_mean = mean(log_HR_estimate),
            log_HR_estimate_se = sd(log_HR_estimate))
result_term$method <- "Term-search RCspline model"



# polynomial with 5 degrees of freedom

result_poly <- readRDS("simulation_Polynomial.rds")
result_poly <- result_poly  %>% filter(degree == 5) %>%
  group_by(B, l) %>%
  summarise(log_HR_true = mean(log_HR_true),
            log_HR_estimate_mean = mean(log_HR_estimate),
            log_HR_estimate_se = sd(log_HR_estimate))
result_poly$method <- "Polynomial model with df = 5"

temp <- rbind(result_knot, result_term,  result_poly)

temp$B <- paste0("B==", temp$B)
temp$B <- factor(temp$B,
                 levels = c("B==-1", "B==1"))
temp$method <- factor(temp$method,
                      levels = c("Knot-search RCspline model", "Term-search RCspline model", "Polynomial model with df = 5"))
temp2 <- temp
temp2$log_HR_estimate_mean <- temp2$log_HR_true
temp2$method <- "True latency-risk curve"
temp2$log_HR_estimate_se <- 0
temp <- rbind(temp, temp2)

temp$method <- factor(temp$method,
                    levels = c("True latency-risk curve", "Knot-search RCspline model", "Term-search RCspline model", "Polynomial model with df = 5"))

saveRDS(temp, "data_figure2_02.rds")


whole_plot <- ggplot(temp) + 
  geom_line(aes(x = l, y = log_HR_estimate_mean, color = method, linetype = method)) + 
  geom_point(aes(x = l, y = log_HR_estimate_mean, shape = method,  color = method,)) +
  facet_grid(.~B, labeller = label_parsed) + 
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
  facet_grid(.~B, labeller = label_parsed) + 
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
  coord_cartesian(xlim = c(10, 15), ylim = c(-0.015, 0.015))
ggarrange(whole_plot, tail_plot, nrow = 2, labels = c("A", "B"))

width <-  23
height <- width*0.8
ggsave("figure2.png", units = "cm", width =width, height = height , dpi = 300)

#######################
# randomly select 20 curves to show the variability of the estimates
set.seed(123)
index <- sample(1:300, 10)
result_knot_all <- readRDS("simulation_KnotGridSearch.rds")
result_knot_all <- result_knot_all %>% filter(sim_id %in% index)
result_knot_all$method <- "Knot-search RCspline model"

result_term_all <- readRDS("simulation_TermSearch.rds")
result_term_all <- result_term_all %>% filter(sim_id %in% index)
result_term_all$method <- "Term-search RCspline model"
result_term_all$sim_id <- result_term_all$sim_id + 50

result_poly_all <- readRDS("simulation_Polynomial.rds")
result_poly_all <- result_poly_all %>% filter(sim_id %in% index)
result_poly_all <- result_poly_all %>% filter(degree == 5)
result_poly_all$method <- "Polynomial model with df = 5"
result_poly_all <- result_poly_all %>% select(-degree)
result_poly_all$sim_id <- result_poly_all$sim_id + 100


temp_all <- rbind(result_knot_all, result_term_all,  result_poly_all)

temp_all$B <- paste0("B==", temp_all$B)
temp_all$B <- factor(temp_all$B,
                 levels = c("B==-1", "B==1", "B==2"))
temp_all$method <- factor(temp_all$method,
                      levels = c("Knot-search RCspline model", "Term-search RCspline model", "Polynomial model with df = 5"))


ggplot(temp_all) + 
  geom_line(aes(x = l, y = log_HR_estimate, group = sim_id, color = method), alpha = 0.7) + 
  geom_line(aes(x = l, y = log_HR_true), color = "black", linetype = "dashed") + 
  facet_grid(method~B) + 
  scale_color_manual(values = c("#3B4992FF", "#DF8F44FF", "#BB0021FF")) + 
  theme_bw() +
  labs(x = expression(Time~lag~(l)),
       y = expression(logHR[p](l)),
       color = "Models") + 
  theme(legend.position = "bottom",
        legend.background = element_blank())


ggsave("figure2_variability.png", units = "cm", width =width, height = height , dpi = 300)
