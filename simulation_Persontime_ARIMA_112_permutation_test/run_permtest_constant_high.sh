#!/bin/bash
# Permutation test (constant_high curve): 300 replicates split into 6 array jobs of
# 50 sims each (n_perm = 300); each job writes its own
# permtest_constant_high_part<k>.rds, so a crashed job only loses its part.
# Previous monolithic runs hit the time limit at ~110/300 sims (~16 min/sim);
# 50 sims ~ 14 h, so 2 days leaves ample headroom.
#
# Usage:
#   sbatch run_permtest_constant_high.sh              # all 6 parts
#   sbatch --array=3 run_permtest_constant_high.sh    # resubmit part 3 only
#   sbatch --array=2,4-6 run_permtest_constant_high.sh
#
#SBATCH -c 80                  # cores per job; future multicore uses all of them
#SBATCH -t 0-20:00             # Runtime in D-HH:MM
#SBATCH -p hsph                # Partition to submit to
#SBATCH --mem-per-cpu=10000    # 10G per core; 800G per job
#SBATCH --array=1-6            # 6 independent jobs, one per 50-sim part
#SBATCH -o myoutput_%A_%a.out  # STDOUT; %A = array job id, %a = part index
#SBATCH -e myerrors_%A_%a.err  # STDERR
#SBATCH --account=mwang_lab_hsph

. /n/home00/fyguo/spack/share/spack/setup-env.sh
spack --version
spack load r
spack load r-devtools
spack load r-mice
spack load r-xgboost
module load cmake

# SLURM_ARRAY_TASK_ID (1-6) tells the R script which part to run.
DIR=/n/home00/fyguo/proj_spline/simulation_Persontime_ARIMA_112_permutation_test
R CMD BATCH --quiet --no-restore --no-save \
  "$DIR/permtest_constant_high.R" \
  "$DIR/permtest_constant_high_part${SLURM_ARRAY_TASK_ID}.out"
