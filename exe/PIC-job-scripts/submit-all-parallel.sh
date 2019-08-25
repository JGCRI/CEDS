#!/bin/bash
# Submit all CEDS species to run in parallel.

# Run activity first, get the ID of that job and have all
# other species run only if activity completed successfully.
actid=$(sbatch --parsable make-activity.sh)

# Wait for activity to finish, then run all species as their own jobs
SO2id=$(sbatch --parsable --dependency=afterok:$actid make-SO2.sh)
NOxid=$(sbatch --parsable --dependency=afterok:$actid make-NOx.sh)
NMVOCid=$(sbatch --parsable --dependency=afterok:$actid make-NMVOC.sh)
NH3id=$(sbatch --parsable --dependency=afterok:$actid make-NH3.sh)
COid=$(sbatch --parsable --dependency=afterok:$actid make-CO.sh)
BCid=$(sbatch --parsable --dependency=afterok:$actid make-BC.sh)
OCid=$(sbatch --parsable --dependency=afterok:$actid make-OC.sh)
CH4id=$(sbatch --parsable --dependency=afterok:$actid make-CH4.sh)
CO2id=$(sbatch --parsable --dependency=afterok:$actid make-CO2.sh)

# Do the non-parallel stuff only after all species have finished 
# without erroring
sbatch --dependency=afterok:$SO2id:$NOxid:$NMVOCid:$NH3id:$COid:$BCid:$OCid:$CH4id:$CO2id ./summary-script.sh 

# show dependencies in squeue output:
squeue -u $USER -a -o "%.5a %.10l %.6D %.6t %N %.40E"
