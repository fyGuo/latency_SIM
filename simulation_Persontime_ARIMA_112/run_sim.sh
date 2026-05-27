#!/bin/bash

# Navigate to the script directory (optional, but ensures correct working directory)
cd "/Users/qianzihan/Desktop/proj_spline/simulation_Persontime_07"

# Run the R script using R CMD BATCH
# R CMD BATCH --quiet --no-restore --no-save simulation_CoxNCSpline.R simulation_CoxNCSpline.out
# R CMD BATCH --quiet --no-restore --no-save simulation_PolyCox.R simulation_PolyCox.out
# R CMD BATCH --quiet --no-restore --no-save simulation_GridSearchKnot.R simulation_GridSearchKnot.out
R CMD BATCH --quiet --no-restore --no-save simulation_TermSearch.R simulation_TermSearch.out
