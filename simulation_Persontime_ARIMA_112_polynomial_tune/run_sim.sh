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

run_sim "simulation_PolyCox.R"

echo "All simulations complete."
