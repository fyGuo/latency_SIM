#!/bin/bash
set -euo pipefail

cd "$(dirname "$0")"

run_sim() {
  local script="$1"
  local out="${script%.R}.out"
  echo "[$(date '+%Y-%m-%d %H:%M:%S')] Starting $script ..."
  R CMD BATCH --quiet --no-restore --no-save "$script" "$out"
  echo "[$(date '+%Y-%m-%d %H:%M:%S')] Done    $script  ->  $out"
}

# run_sim "simulation_PolyCox.R"
run_sim "simulation_GridSearchKnot.R"
echo "GridSearchKnot simulations complete."

run_sim "simulation_TermSearch.R"
echo "TermSearch simulations complete."

echo "All simulations complete."
