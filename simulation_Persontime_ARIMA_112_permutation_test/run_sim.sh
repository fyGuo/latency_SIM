#!/bin/bash

# Navigate to this script's own directory, so the project can be moved between
# machines without editing any path.
cd "$(dirname "$0")"

# Each scenario's 300 replicates are split into 6 parts of 50 sims, selected by
# the PART environment variable (or SLURM_ARRAY_TASK_ID on the cluster). On the
# cluster, prefer the array jobs instead of this script:
#   sbatch run_permtest_<scenario>.sbatch
#
# This script runs everything serially (36 part-runs) for local use.
for scenario in unimodal bimodal_pos bimodal_neg decayed constant_low constant_high; do
  for part in 1 2 3 4 5 6; do
    PART=$part R CMD BATCH --quiet --no-restore --no-save \
      "permtest_${scenario}.R" "permtest_${scenario}_part${part}.out"
  done
done
