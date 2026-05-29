#!/bin/bash

cd "$(dirname "$0")"

# Run the R script using R CMD BATCH
 R CMD BATCH --quiet --no-restore --no-save simulation_PolyCox.R simulation_PolyCox.out
# R CMD BATCH --quiet --no-restore --no-save simulation_GridSearchKnot.R simulation_GridSearchKnot.out
#R CMD BATCH --quiet --no-restore --no-save simulation_TermSearch.R simulation_TermSearch.out
