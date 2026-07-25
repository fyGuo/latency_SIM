#!/bin/bash
# Knot-search coverage simulation across all six weighting shapes.
# 6 shapes x 6 parts of 50 sims (boot_iter = 300) = 36 array tasks. The array
# index 1..36 is mapped inside coverage_simulation.R to (scenario, part) as
#   scenario = (idx-1)/6 + 1   (unimodal, bimodal_pos, bimodal_neg, decayed,
#                               constant_low, constant_high)
#   part     = (idx-1)%6 + 1
# Each task writes coverage_Knotsearch_<scenario>_part<k>.rds, so a crashed job
# only loses its own part.
#
# Usage:
#   sbatch run_coverage_Knotsearch.sbatch                 # all 36 tasks
#   sbatch --array=7-12 run_coverage_Knotsearch.sbatch    # only bimodal_pos (all 6 parts)
#   sbatch --array=3 run_coverage_Knotsearch.sbatch       # unimodal, part 3 only
#
# When all parts are done, combine with:
#   R CMD BATCH --quiet --no-restore --no-save make_coverage_table.R
#
#SBATCH -c 80                  # cores per job; future multicore uses all of them
#SBATCH -t 0-30:00             # Runtime in D-HH:MM
#SBATCH -p hsph                # Partition to submit to
#SBATCH --mem-per-cpu=10000    # 10G per core; 800G per job
#SBATCH --array=1-36           # 6 shapes x 6 parts
#SBATCH -o myoutput_%A_%a.out  # STDOUT; %A = array job id, %a = task index
#SBATCH -e myerrors_%A_%a.err  # STDERR
#SBATCH --account=mwang_lab_hsph

. /n/home00/fyguo/spack/share/spack/setup-env.sh
spack --version
spack load r
spack load r-devtools
spack load r-mice
spack load r-xgboost
module load cmake

export METHOD=Knotsearch
# SLURM_ARRAY_TASK_ID (1-36) tells coverage_simulation.R which (scenario, part) to run.
DIR=/n/home00/fyguo/proj_spline/simulation_Persontime_ARIMA_112_coverage
R CMD BATCH --quiet --no-restore --no-save \
  "$DIR/coverage_simulation.R" \
  "$DIR/coverage_Knotsearch_task${SLURM_ARRAY_TASK_ID}.out"
