# ── Illustrate exposure trajectories ──────────────────────────────────────────
# Generate ONE simulated person-time data set, randomly pick 10 individuals, and
# plot each one's exposure level over time (age). For a given person the first
# observed row already carries the full exposure history in its lag columns
# (lag0..lag30 = exposure at the entry age and the 30 preceding years); the lag0
# values of the follow-up rows extend the trajectory forward. We reconstruct the
# complete (age, exposure) path from those and overlay the 10 subjects.

# ── self-locating working directory ───────────────────────────────────────────
local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", grep("^--file=", a, value = TRUE))
  if (!length(f)) { i <- which(a == "-f"); if (length(i)) f <- a[i + 1L] }
  if (length(f) && nzchar(f)) setwd(dirname(normalizePath(f)))
})
source("simulate_personetime.R")

# ── 1. generate one simulated data set ────────────────────────────────────────
set.seed(2026)
sim_data <- generate_data(sample_size = 500)
saveRDS(sim_data, "simulated_data.rds")

# ── 2. randomly select 10 individuals ─────────────────────────────────────────
set.seed(7)
selected_ids <- sort(sample(unique(sim_data$id), 10))
cat("Selected ids:", paste(selected_ids, collapse = ", "), "\n")

# ── 3. reconstruct each selected person's exposure trajectory over age ─────────
LAGS <- 0:30
build_traj <- function(pid) {
  p     <- sim_data %>% filter(id == pid) %>% arrange(age_start)
  first <- p[1, ]
  hist  <- tibble(age      = first$age_start - LAGS,                     # 31y of history
                  exposure = as.numeric(first[paste0("lag", LAGS)]))
  fu    <- tibble(age = p$age_start, exposure = p$lag0)                  # follow-up
  bind_rows(hist, fu) %>%
    distinct(age, .keep_all = TRUE) %>%
    arrange(age) %>%
    mutate(id = pid)
}
traj <- map_dfr(selected_ids, build_traj)
write_csv(traj, "selected_trajectories.csv")

# ── 4. plot: one exposure trajectory per person ───────────────────────────────
p <- ggplot(traj, aes(x = age, y = exposure, colour = factor(id), group = id)) +
  geom_line(linewidth = 0.6) +
  scale_colour_viridis_d(name = "Subject") +
  labs(x = "Age (years)", y = "Exposure level",
       title = "Simulated exposure trajectories of 10 randomly selected subjects") +
  theme_bw(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 11))

ggsave("figure_exposure_trajectory.png", p,
       units = "cm", width = 22, height = 13, dpi = 300, bg = "white")
message("Saved figure_exposure_trajectory.png, simulated_data.rds, selected_trajectories.csv")
