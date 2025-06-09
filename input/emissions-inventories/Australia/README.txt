With the update out to 2022 for this inventory the old excel inventory files no longer need to be updated.

The only data that needs to be updated is the aggregate inventory database:
Aus_total_air_emissions_by_sector.csv

which is summed from the larger NPI database of point source emissions by a script in the CEDS_Data internal pre-processing repo.

The old files are still used to extract diffuse emission values (which do not change year to year) so as to construct an approximate country level inventory for comparison. But these do not need to be updated.
