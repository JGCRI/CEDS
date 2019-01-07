#!/bin/bash

sbatch make-extended-activity.sh

sleep 54000s

sbatch make-SO2.sh
sbatch make-NOx.sh
sbatch make-NMVOC.sh
sbatch make-NH3.sh
sbatch make-CO.sh
sbatch make-BC.sh
sbatch make-OC.sh
sbatch make-CH4.sh
sbatch make-CO2.sh

# Check once a minute if all the emissions are done
while [ $( ls slurm-* 2>/dev/null | wc -l ) -le 1 ] || [ $( tail -n 1 slurm-* | grep 'Current time' | wc -l ) -ne $( ls slurm-* | wc -l ) ]
do
sleep 60s
done

# Do the non-parallel stuff
cd ../../
module load R
Rscript code/module-S/S1.2.aggregate_summary_data.R
