#!/bin/bash

sbatch make-activity.sh

sleep 1800s

sbatch make-SO2.sh
sbatch make-NOx.sh
sbatch make-NMVOC.sh
sbatch make-NH3.sh
sbatch make-CO.sh
sbatch make-BC.sh
sbatch make-OC.sh
sbatch make-CH4.sh
sbatch make-CO2.sh

