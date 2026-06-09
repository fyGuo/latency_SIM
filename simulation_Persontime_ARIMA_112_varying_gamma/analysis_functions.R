library(Hmisc)
library(tidyverse)
library(stats)
library(survival)
library(splines)

# A function to implement the method proposed by Michels which polynomial  terms
poly_fun <- function(sim_data, K, latency_length = 20) {
  X <- sim_data[, paste0("lag", 0:(latency_length-1))]
  v <- 0:(latency_length-1)
  Sum <- rowSums(X)
  # here we conduct the summation term of k
  Sum_mat <- matrix(NA, nrow = dim(sim_data)[1], ncol = K)
  for (k in 1:K){
    Sum_mat[,k] <- as.matrix(X[, paste0("lag", 0:(latency_length-1))]) %*% v^k
  }
  fit <- coxph(Surv(sim_data$age_start, sim_data$age_end, sim_data$failure)~ Sum + Sum_mat,
                control = coxph.control(timefix = FALSE))
  coef(fit) %>% return()
}

spline_function_GridSearch <- function(sim_data, J = NULL, knots = NULL, latency_length = 20){
  X <- sim_data[, paste0("lag", 0:(latency_length-1))]
  v <- 0:(latency_length-1)
  mini_AIC <- 99999999
  fit_best <- NA
  knots_best <- NA
  # here we may put multiple possible number of knots as J
  for (j in J){
    # generate the B matrix
    B <- matrix(NA, nrow = latency_length, ncol = j )
    # the first column of B matrix is the intercept, and thus, is 1
    B[,1] <- 1
    
    # Make a list of potential knots
    knot_list <- vector(mode = "list", length = j)
    for (i in 1:j){
      cut_start <- quantile(v, 0.1) %>% round()
      cut_end <- quantile(v, 0.9) %>% round()
      if (i == 1) {
        knot_list[[i]] <- min(v):cut_start
      } else if (i == j){
        knot_list[[i]] <- (cut_end +1):max(v)
      } else {
        knot_list[[i]] <- (round(quantile(cut_start:cut_end, (i-2)/(j-2)))+1) : 
          round(quantile(cut_start:cut_end, (i-1)/(j-2)))
      }
    }
    
    knot_list <- expand.grid(knot_list)
    for (i in 1:dim(knot_list)[1]){
      knots <- as.numeric(knot_list[i,])
      spline <- rcspline.eval(0:19, knots = knots, inclx = TRUE)
      B[, 2:(j)] <- as.matrix(spline)
      
      Sum_mat <- matrix(NA, nrow = dim(sim_data)[1], ncol = j)
      Sum_mat <- as.matrix(X) %*% B
 
      fit <- coxph(Surv(sim_data$age_start, sim_data$age_end, sim_data$failure)~Sum_mat,
                   control = coxph.control(timefix = FALSE))
      AIC <- extractAIC(fit)[2]
      if (AIC < mini_AIC) {
        mini_AIC <- AIC
        fit_best <- fit 
        knots_best <- knots
      }
    }
  }
  list(knots_best, coef(fit_best)) %>% return()
}




