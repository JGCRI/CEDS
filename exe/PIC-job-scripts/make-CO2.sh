#!/bin/bash
#SBATCH -A GCAM
#SBATCH -t 05:00:00
#SBATCH -N 1
#SBATCH -p shared
#SBATCH -n 1

#Set up your environment you wish to run in with module commands.
module purge
module load R/3.3.3

#Actually codes starts here
now=$(date)
echo "Current time : $now"

cd /people/feng999/CEDS
make CO2-emissions

now=$(date)
echo "Current time : $now"

