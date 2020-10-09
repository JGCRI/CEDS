#!/bin/bash
#SBATCH -A ceds
#SBATCH -t 10:00:00
#SBATCH -N 1
#SBATCH -p shared
#SBATCH -n 1
#SBATCH --mail-user <email_address@host>
#SBATCH --mail-type END

#Set up your environment you wish to run in with module commands.
module purge
module load R/3.5.1

#Actually codes starts here
now=$(date)
echo "Current time : $now"

cd /qfs/people/<user_name>/CEDS
make CH4-emissions

now=$(date)
echo "Current time : $now"

