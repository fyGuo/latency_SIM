#!/bin/bash
#SBATCH -c 30         # Number of cores (-c)
#SBATCH -t 0-24:00          # Runtime in D-HH:MM, minimum of 10 minutes
#SBATCH -p hsph # Partition to submit to
#SBATCH --mem-per-cpu=20000          # Memory pool for all cores (see also --mem-per-cpu)
#SBATCH -o myoutput_%j.out  # File to which STDOUT will be written, %j inserts jobid
#SBATCH -e myerrors_%j.err  # File to which STDERR will be written, %j inserts jobid
#SBATCH --account=mwang_lab_hsph

. /n/home00/fyguo/spack/share/spack/setup-env.sh
spack --version
spack load r
spack load r-devtools
spack load r-mice
spack load r-xgboost
module load cmake
# Under sbatch, $0 points to SLURM's spool copy of this script, so dirname($0)
# is not the project folder. Hardcode the path instead.
cd /n/home00/fyguo/proj_spline/simulation_Persontime_ARIMA_112_timing_knotSearch

run_sim() {
  local script="$1"
  local out="${script%.R}.out"
  echo "[$(date '+%Y-%m-%d %H:%M:%S')] Starting $script ..."
  R CMD BATCH --quiet --no-restore --no-save "$script" "$out"
  echo "[$(date '+%Y-%m-%d %H:%M:%S')] Done    $script  ->  $out"
}

run_sim "simulation_timing_GridSearchKnot.R"
run_sim "simulation_timing_NCSpline_df3.R"
run_sim "simulation_timing_NCSpline_df4.R"
run_sim "simulation_timing_NCSpline_df5.R"

echo "All timing simulations complete."
