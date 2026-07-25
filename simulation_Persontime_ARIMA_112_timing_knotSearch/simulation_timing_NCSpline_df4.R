local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", grep("^--file=", a, value = TRUE))
  if (!length(f)) { i <- which(a == "-f"); if (length(i)) f <- a[i + 1L] }
  if (!length(f) && requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable())
    f <- rstudioapi::getSourceEditorContext()$path
  if (length(f) && nzchar(f)) setwd(dirname(normalizePath(f)))
})
source("simulation_timing.R")

temp <- data.frame()

for (sample_size in c(300, 500, 1000, 3000, 5000)) {
  result <- simulation_timing(sample_size = sample_size,
                               method     = "NCSpline",
                               degree     = 4)
  cat(sprintf("NCSpline df=4  sample_size = %5d  mean elapsed = %.4f s\n",
              sample_size, mean(result$elapsed, na.rm = TRUE)))
  temp <- rbind(temp, result)
}

saveRDS(temp, "timing_NCSpline_df4.rds")
Sys.time()
