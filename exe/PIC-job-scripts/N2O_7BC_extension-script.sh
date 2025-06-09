#!/bin/bash
#SBATCH -A ceds
#SBATCH -t 01:00:00
#SBATCH -N 1
#SBATCH -p shared
#SBATCH -n 1
#SBATCH --mail-user <email_address@host>
#SBATCH --mail-type END

#Set up your environment you wish to run in with module commands.
module purge
module load R/3.6.3

#Actual codes starts here
now=$(date)
echo "Current time : $now"

cd /rcfs/projects/ceds/CEDS-dev
Rscript code/module-H/H1.1a.Aggregate_NH3_NOx_for_N2O_7BC_ext.R

now=$(date)
echo "Current time : $now"

