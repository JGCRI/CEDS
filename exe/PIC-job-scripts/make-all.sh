#!/bin/bash
#SBATCH -A ceds
#SBATCH -t 25:00:00
#SBATCH -N 1
#SBATCH -p shared
#SBATCH -n 1
#SBATCH --mail-user <email_address@host>
#SBATCH --mail-type END

#Set up your environment you wish to run in with module commands.
module purge
module load R/3.6.3

#Actually codes starts here
now=$(date)
echo "Current time : $now"

cd /rcfs/projects/ceds/CEDS-dev
make

now=$(date)
echo "Current time : $now"

