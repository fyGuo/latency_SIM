#!/bin/bash

# Navigate to this script's own directory, so the project can be moved between
# machines without editing any path.
cd "$(dirname "$0")"

# Local serial runner: 2 methods x 6 shapes x 6 parts of 50 sims. On the cluster,
# prefer the array jobs instead of this script:
#   sbatch run_coverage_Knotsearch.sbatch
#   sbatch run_coverage_Termsearch.sbatch
#
# Each (method, scenario, part) is selected via the METHOD / SCENARIO / PART
# environment variables and writes coverage_<method>_<scenario>_part<k>.rds.
for method in Knotsearch Termsearch; do
  for scenario in unimodal bimodal_pos bimodal_neg decayed constant_low constant_high; do
    for part in 1 2 3 4 5 6; do
      METHOD=$method SCENARIO=$scenario PART=$part \
        R CMD BATCH --quiet --no-restore --no-save \
        coverage_simulation.R "coverage_${method}_${scenario}_part${part}.out"
    done
  done
done
