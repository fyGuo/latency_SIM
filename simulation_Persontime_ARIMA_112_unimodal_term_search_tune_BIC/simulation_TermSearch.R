setwd(tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) getwd()
))

source("simulate_personetime.R")
source("simulation_output.R")

library(future)
library(furrr)
library(LatencyA)

# Three term_knots scenarios to compare:
#   dense  : every lag from 0 to 15  (16 terms)
#   even   : every other lag          (8 terms)
#   sparse : 5 evenly spaced lags     (5 terms)
knots_scenarios <- list(
  list(term_knots = 0:15,                         label = "16 terms"),
  list(term_knots = c(0, 2, 4, 6, 8, 10, 12, 14), label = "8 terms"),
  list(term_knots = c(0, 4, 8, 12, 15),            label = "5 terms")
)

temp <- data.frame()

for (s in knots_scenarios) {
  result <- simulation_output(sample_size = 5000, knot = 10,
                              term_knots  = s$term_knots)
  result$knot             <- 10
  result$term_knots_label <- s$label
  temp <- rbind(temp, result)
}

temp$method <- "TermSearch"
saveRDS(temp, "simulation_TermSearch.rds")

Sys.time()
