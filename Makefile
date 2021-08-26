# Specify directories
MOD_A = code/module-A
MOD_B = code/module-B
MOD_C = code/module-C
MOD_D = code/module-D
MOD_E = code/module-E
MOD_F = code/module-F
MOD_G = code/module-G
MOD_H = code/module-H
MOD_S = code/module-S
PARAMS = code/parameters
SOCIO_DATA = input/general
ENERGY_DATA = input/energy
EF_DATA = input/default-emissions-data
EF_PARAMETERS = input/default-emissions-data/EF_parameters
MAPPINGS = input/mappings
EN_MAPPINGS = input/mappings/energy
SC_MAPPINGS = input/mappings/scaling
ACTIV = input/activity
INV_DATA = input/emissions-inventories
MED_OUT = intermediate-output
DIAG_OUT = diagnostic-output
FINAL_OUT = final-emissions
EXT_IN = input/extension
USER_EN_PROCESS	= input/energy/user-defined-energy
USER_EN_IN = input/energy/user-defined-energy/user_energy_input
EXT_DATA = input/extension/extension-data
LOGS = logs
DOCS = documentation

# --------------------------------------------------------------

# This is a recursive Makefile system.
# When the Makefile is invoked from outside, MAKELEVEL is 0 by
# default. When "all" or any of the "___-emissions" targets are
# built, the prefix emission abbreviation will be caught and
# assigned to EM, and Make called recursively on this Makefile.
# MAKELEVEL will then be 1, and the "emissions" target will then
# be visible, causing the system to build all the system files
# as normal, as long as the "emissions" target specifies the
# final output file.

ifeq ($(MAKELEVEL),0)

# By default, EM is set to "NONE". This will trigger warnings
# and/or errors if not overwritten by a real abbreviation once
# the system is fully implemented and actually has input files
# with specific emissions types.
export EM = NONE

# The % acts as a wildcard that saves the emissions abbreviation
# prefix, and sets EM to that value, specifying the emissions
# species for the rest of the Makefile.
%-emissions: export EM = $*
%-gridded: export EM = $*

# Prints out the selected emissions type
# and calls the recursive Make
%-emissions:
	@printf '%*s\n' "$${COLUMNS:-$$(tput cols)}" '' | tr ' ' =
	@printf '%*s' "$${COLUMNS:-$$(($$(tput cols) / 2 - 10))}" '' | tr ' ' ' '
	@echo "Selected emission is $(EM)"
	@printf '%*s\n' "$${COLUMNS:-$$(tput cols)}" '' | tr ' ' =
	@$(MAKE) emissions

%-gridded:
	@printf '%*s\n' "$${COLUMNS:-$$(tput cols)}" '' | tr ' ' =
	@printf '%*s' "$${COLUMNS:-$$(($$(tput cols) / 2 - 10))}" '' | tr ' ' ' '
	@echo "Gridding $(EM) emissions"
	@printf '%*s\n' "$${COLUMNS:-$$(tput cols)}" '' | tr ' ' =
	@$(MAKE) gridded-emissions

activity: $(MED_OUT)/A.total_activity_extended.csv \
	$(EXT_DATA)/A.Pig_Iron_Production.csv \
	$(MED_OUT)/A.coal_heat_content.csv

else

# This target is only visible to the Makefile during the
# recursive call, once EM has been specified, and triggers the
# build of the entire system. This target must be the final
# outputs of the system.
emissions: $(FINAL_OUT)/current-versions/CEDS_$(EM)_emissions_by_country_sector_%.csv

gridded-emissions: $(FINAL_OUT)/gridded-emissions/$(EM)-em-anthro* \
	$(FINAL_OUT)/gridded-emissions/$(EM)-em-SOLID-BIOFUEL-anthro* \
	$(FINAL_OUT)/gridded-emissions/$(EM)-em-AIR-anthro*


endif

# --------------------------------------------------------------

# List rules: Main body of Makefile begins here

# --------------------------------------------------------------

# TODO: Add CO2-emissions, BC-emissions, etc. as they are
# integrated into the system.

# The command "make all" will run the system for all emissions
# species listed here. A command of "make ___-emissions" (for
# example, "make SO2-emissions") will run the system for only
# that emissions species. To add a new emissions type, simply
# add the relevant "-emissions" to "all" and ensure that all
# necessary scripts have been created and placed in the correct
# modules. You may also wish to create a new .bat file
# specifically to run the system with the new emissions type.

# Note that this is an inefficient method of creating data for multiple species
# If a multi-processor machine is available, CEDS should be instead run in parallel for multiple species
all: SO2-emissions BC-emissions OC-emissions NOx-emissions CO-emissions NMVOC-emissions CO2-emissions NH3-emissions CH4-emissions N2O-emissions
part1: NOx-emissions SO2-emissions NH3-emissions
part2: BC-emissions OC-emissions CO2-emissions  N2O-emissions
part3: CO-emissions NMVOC-emissions CH4-emissions

# --------------------------------------------------------------

# Targets used to remove output files for a fresh run
clean-all: \
	clean-intermediate clean-diagnostic clean-final clean-logs clean-io clean-modA clean-modB clean-modC \
	clean-modD clean-modE clean-modF clean-modH clean-gridding clean-user_defined_energy

clean-user_defined_energy:
# Deletes all CSVs in the directory except for:
# 1) CEDS user-defined energy inputs, metadata, and instructions (U.*.csv)
# 2) Relevant mapping and instructions files for user-defined energy input which require pre-processing for use in CEDS (A.*-instructions.csv, A.*_sector_map.csv, A.*-mapping.xlsx)
	find $(USER_EN_IN) -name "*.csv" ! -name "U.*.csv" ! -name "A.*-instructions.csv" ! -name "A.*_sector_map.csv" -delete

# Deletes all xlsx files that don't end in -mapping.xlsx and begin with "A." or "U."
	find $(USER_EN_IN) -name "*.xlsx" ! -name "A.*-mapping.xlsx" ! -name "U.*-mapping.xlsx" -delete

# Deletes all txt files that aren't the README file
	find $(USER_EN_IN) -name "*.txt" ! -name "README.txt" -delete

clean-intermediate:
	rm -fv $(MED_OUT)/*.csv

clean-diagnostic:
	rm -fv $(DIAG_OUT)/*.csv \
	rm -fv $(DIAG_OUT)/summary-plots/*.csv \
	rm -fv $(DIAG_OUT)/summary-plots/*.pdf \
	rm -fv $(DIAG_OUT)/ceds-comparisons/*.pdf \
	rm -fv $(DIAG_OUT)/ceds-comparisons/*.csv \
	rm -fv $(DIAG_OUT)/ceds-comparisons/sector-level/*.csv \
	rm -fv $(DIAG_OUT)/ceds-comparisons/sector-level/*.pdf \
	rm -fv $(DIAG_OUT)/user-data/*.csv \
	rm -fv $(DIAG_OUT)/user-data/*.png \
	rm -fv $(DIAG_OUT)/gridding-diagnostic-plots/cell_group_by_sector/*.pdf \
	rm -fv $(DIAG_OUT)/gridding-diagnostic-plots/cell_group_totals/*.jpeg \
	rm -fv $(DIAG_OUT)/gridding-diagnostic-plots/seasonal-line-plots/*.pdf \
	rm -fv $(DIAG_OUT)/gridding-diagnostic-plots/single_cell_by_sector/*.pdf \
	rm -fv $(DIAG_OUT)/gridding-diagnostic-plots/single_cell_totals/*.jpeg

clean-final:
	rm -fv $(FINAL_OUT)/*.csv

clean-logs:
	rm -fv $(LOGS)/*.log \
	rm -fv $(LOGS)/*.R.d

clean-io:
	rm -fv $(DOCS)/*IO_documentation.csv

clean-modA:
	rm -fv $(MED_OUT)/A*.csv \
	rm -fv $(EXT_IN)/extension-data/A.*.csv

clean-modB:
	rm -fv $(MED_OUT)/B*.csv \
	rm -fv $(EF_PARAMETERS)/B.*.csv \
	rm -fv $(EF_DATA)/non-combustion-emissions/B.*.csv \
	rm -fv $(EXT_IN)/extension-data/B.*.csv

clean-modC:
	rm -fv $(MED_OUT)/C*.csv \
	rm -fv $(EF_DATA)/non-combustion-emissions/C.*.csv

clean-modD:
	rm -fv $(MED_OUT)/D*.csv

clean-modE:
	rm -fv $(MED_OUT)/E*.csv

clean-modF:
	rm -fv $(MED_OUT)/F*.csv

clean-modH:
	rm -fv $(MED_OUT)/H*.csv
	find $(EXT_IN) -name "H*.csv" ! -name "H.N2O_7BC_extension-NH3_and_NOx_sectors_1_2*.csv"  -delete

clean-SO2:
	rm -fv $(MED_OUT)/*SO2*.csv \
	rm -fv $(EF_DATA)/non-combustion-emissions/C.SO2*.csv

clean-NOx:
	rm -fv $(MED_OUT)/*NOx*.csv \
	rm -fv $(EF_DATA)/non-combustion-emissions/C.NOx*.csv

clean-CH4:
	rm -fv $(MED_OUT)/*CH4*.csv \
	rm -fv $(EF_DATA)/non-combustion-emissions/C.CH4*.csv

clean-CO:
	rm -fv $(MED_OUT)/*CO*.csv \
	rm -fv $(EF_DATA)/non-combustion-emissions/C.CO*.csv

clean-CO2:
	rm -fv $(MED_OUT)/*CO2*.csv \
	rm -fv $(EF_DATA)/non-combustion-emissions/C.CO2*.csv

clean-NMVOC:
	rm -fv $(MED_OUT)/*NMVOC*.csv \
	rm -fv $(EF_DATA)/non-combustion-emissions/C.NMVOC*.csv

clean-NH3:
	rm -fv $(MED_OUT)/*NH3*.csv \
	rm -fv $(EF_DATA)/non-combustion-emissions/C.NH3*.csv

clean-OC:
	rm -fv $(MED_OUT)/*OC*.csv \
	rm -fv $(EF_DATA)/non-combustion-emissions/C.OC*.csv

clean-BC:
	rm -fv $(MED_OUT)/*BC*.csv \
	rm -fv $(EF_DATA)/non-combustion-emissions/C.BC*.csv

clean-N2O:
	rm -fv $(MED_OUT)/*N2O*.csv \
	rm -fv $(EF_DATA)/non-combustion-emissions/C.N2O*.csv

clean-gridding:
	rm -fv $(MED_OUT)/gridded-emissions/*.csv


# --------------------------------------------------------------

# Syntax overview

# The first line in each paragraph (referred to as blocks or code
# blocks) is the target, and is followed by
# its dependencies. The target is the output of that portion of
# the system, and its dependencies are the scripts that create
# it and the inputs those scripts require.

# The $() prefix specifies the location of the script, input, or
# where the output should go. See the top of the Makefile for
# the full path mapping.

# The target is followed by a ":". All files listed on the same
# line ( after the ":" ) are considered dependencies. The "\"
# indicate that the "line" of dependencies continues on the
# following line. The last dependency is not followed by a "\"-
# any remaining lines in the block are commands.

# The Rscript commands instruct the Makefile to run the first
# dependency, or first several dependencies, as R scripts. This
# is why the R scripts that generate the output come first in
# the dependency list.

# A command beginning with "Rscript $<" runs the first
# dependency as an R script. A command beginning with "Rscript
# $(word 2,$^)" runs the second dependency, "$(word 3,$^)", runs
# the third, and so on. Parent scripts should be run with the
# Rscript $< command, while child scripts should be listed as
# dependencies (but not called with Rscript $(word 2,$^) )

# The Make file can only understand a single target, so if a single
# Rscript produces more than one output, multiple makefile code blocks
# are necessary. The first block specifies the first output as the
# target, all it's dependencies, and the Rscript $< command as
# usual. The second block specifies the second output as the target
# and the first output as its dependency without an Rscript $< command.

# --------------------------------------------------------------

# Makefile code block naming convention

# Code blocks do not span different modules, however they do not map
# to the numbering system within modules. CEDS Rscripts are
# generally labeled with the X#.# convention. Makefile code blocks
# are labeled with xx# or xx#-#, numerically from the beginning of
# the module. For example. The makefile code block running the first
# script from Module B is labeled 'bb1'. Notation bb1, does not refer
# to Module B1, simply the first code block of module 1. Rscripts
# that produce more than one output require more than 1 code bock,
# however only the first code block has the Rscript $> command. These
# are numbered with xx#-1 and x#-2.

# Adding code blocks to the makefile may require renumbering. However
# adding a single block to the makefile should (if at all) only require
# renumbering one module.

# --------------------------------------------------------------

# Adding new targets and dependencies

# When adding a new script to an existing block (for example, a
# new "add" script), simply add the path and name of the new
# script beneath the last existing R script in the dependency
# list, and add a new Rscript line with an incremented target to
# run it.

# When adding a new target entirely, ensure that you list all
# scripts that contribute to it (as well as Rscript commands to
# run each), and all inputs it requires. However, you do not
# need to list inputs that are dependencies of earlier outputs
# in that part of the chain. For example, several module B and C
# scripts require Master_Fuel_Sector_List.xlsx, but because it
# is listed as a dependency of a module A output which they
# depend on, they do not need to list it again.

# When a script has multiple outputs, create another target for
# each additional output and have them list the first output as
# their only dependency.

# When altering scripts or their inputs and outputs, even just
# their names, ensure that you make the change across all
# instances within the Makefile as well, or errors will occur.

# --------------------------------------------------------------

# aa1-1
# Produce population data as system input
$(MED_OUT)/A.UN_pop_master.csv: \
	$(MOD_A)/A1.1.UN_pop_WB_HYDE_extension.R \
	$(PARAMS)/common_data.R \
	$(PARAMS)/global_settings.R \
	$(PARAMS)/IO_functions.R \
	$(PARAMS)/data_functions.R \
	$(PARAMS)/analysis_functions.R \
	$(MAPPINGS)/Master_Country_List.csv \
	$(SOCIO_DATA)/WPP2015_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx \
	$(SOCIO_DATA)/WUP2014-F21-Proportion_Urban_Annual.xlsx \
	$(SOCIO_DATA)/WB_SP.POP.TOTL.csv \
	$(SOCIO_DATA)/WB_SP.URB.TOTL.csv \
	$(SOCIO_DATA)/urbanpop_2004Rev_tcm61-36007.xlsx
	Rscript $< $(EM) --nosave --no-restore

# aa1-2
# Process Fernandes biomass data
$(MED_OUT)/A.Fernandes_residential_biomass.csv: \
	$(MOD_A)/A1.2.Fernandes_biomass.R \
	$(MED_OUT)/A.UN_pop_master.csv \
	$(ENERGY_DATA)/Fernandes_Biofuels_9.xlsx \
	$(SOCIO_DATA)/biomass_heat_content.xlsx \
	$(MAPPINGS)/Fernandes_proxy_country_mapping.csv
	Rscript $< $(EM) --nosave --no-restore

$(MED_OUT)/A.Fernandes_biomass_conversion.csv: \
	$(MED_OUT)/A.Fernandes_residential_biomass.csv

# aa1-3
# Initial processing of IEA energy data
$(MED_OUT)/A.IEA_en_stat_ctry_hist.csv: \
	$(MOD_A)/A1.3.IEA_downscale_ctry.R \
	$(MED_OUT)/E.CO2_CDIAC_inventory.csv \
	$(MED_OUT)/A.UN_pop_master.csv \
	$(ENERGY_DATA)/OECD_and_NonOECD_E_Stat.csv
	Rscript $< $(EM) --nosave --no-restore

# aa1-4
# Write out IEA coal heat content
$(MED_OUT)/A.coal_heat_content.csv: \
	$(MOD_A)/A1.4.IEA_heat_content.R \
	$(MAPPINGS)/Master_Country_List.csv \
	$(EN_MAPPINGS)/IEA_product_fuel.csv \
	$(ENERGY_DATA)/OECD_and_NonOECD_Conversion_Factors_Full.csv
	Rscript $< $(EM) --nosave --no-restore

# aa2-1
# Converts IEA energy data to CEDS standard format
# Corrects inconsistencies in residential biomass consumption
$(MED_OUT)/A.en_stat_sector_fuel.csv: \
	$(MOD_A)/A2.1.IEA_en_bal.R \
	$(MED_OUT)/A.IEA_en_stat_ctry_hist.csv \
	$(EN_MAPPINGS)/IEA_flow_sector.csv \
	$(EN_MAPPINGS)/IEA_process_sectors.csv \
	$(EN_MAPPINGS)/IEA_product_fuel.csv \
	$(MAPPINGS)/Master_Fuel_Sector_List.xlsx \
	$(MED_OUT)/A.Fernandes_biomass_conversion.csv \
	$(ENERGY_DATA)/IEA_energy_balance_factor.csv
	Rscript $< $(EM) --nosave --no-restore

$(MED_OUT)/A.en_biomass_fix.csv: \
	$(MOD_A)/A2.2.IEA_biomass_fix.R \
	$(MED_OUT)/A.UN_pop_master.csv \
	$(MED_OUT)/A.en_stat_sector_fuel.csv \
	$(MED_OUT)/A.Fernandes_residential_biomass.csv \
	$(MED_OUT)/A.Fernandes_biomass_conversion.csv \
	$(ENERGY_DATA)/Europe_wooduse_Europe_TNO_4_Steve.xlsx \
	$(ENERGY_DATA)/EIA_Table_10.2a_Renewable_Energy_Consumption___Residential_and_Commercial_Sectors.xlsx \
	$(ENERGY_DATA)/IEA_biomass_double_counting.xlsx
	Rscript $< $(EM) --nosave --no-restore

$(MED_OUT)/A.en_biomass_fsu_fix.csv: \
	$(MOD_A)/A2.3.IEA_FSU_energy_fix.R \
	$(MED_OUT)/A.UN_pop_master.csv \
	$(MED_OUT)/A.en_biomass_fix.csv
	Rscript $< $(EM) --nosave --no-restore

$(MED_OUT)/A.IEA_CEDS_hard_coal_difference.csv: \
	$(MOD_A)/A2.3.write_IEA_diff.R \
	$(MED_OUT)/A.IEA_en_stat_ctry_hist.csv \
	$(MED_OUT)/A.en_biomass_fsu_fix.csv
	Rscript $< $(EM) --nosave --no-restore

$(MED_OUT)/A.comb_othertrans_activity.csv: \
	$(MOD_A)/A2.4.combine_combustion_and_other_energy.R \
	$(MED_OUT)/A.en_biomass_fsu_fix.csv \
	$(MED_OUT)/A.IEA_CEDS_hard_coal_difference.csv
	Rscript $< $(EM) --nosave --no-restore

# aa3-1
# Extends IEA data with BP data
$(MED_OUT)/A.IEA_BP_energy_ext.csv: \
	$(MOD_A)/A3.1.IEA_BP_data_extension.R \
	$(MED_OUT)/A.comb_othertrans_activity.csv \
	$(MAPPINGS)/Master_Fuel_Sector_List.xlsx \
	$(ENERGY_DATA)/bp-stats-review-2020-all-data.xlsx
	Rscript $< $(EM) --nosave --no-restore

# aa3-2
# Write out difference between IEA and CEDS coal
$(MED_OUT)/A.IEA_CEDS_coal_difference.csv: \
	$(MOD_A)/A2.3.write_IEA_diff.R \
	$(MED_OUT)/A.IEA_en_stat_ctry_hist.csv \
	$(MED_OUT)/A.en_biomass_fsu_fix.csv
	Rscript $< $(EM) --nosave --no-restore

# Write out difference between IEA and CEDS coal
#$(MED_OUT)/A.IEA_CEDS_coal_difference.csv: \
	$(MED_OUT)/A.IEA_BP_energy_ext.csv
#	Rscript $< $(EM) --nosave --no-restore

# aa3-3
# Process pig iron production  # AM: maybe comment this out? Doesn't need it since it's being called for creating A.NC.activity
 #$(EXT_DATA)/A.Pig_Iron_Production.csv: \
#	$(MOD_A)/A3.3.proc_pig_iron.R \
#	$(MED_OUT)/A.UN_pop_master.csv
	#(ACTIV)/metals/Blast_furnace_iron_production_1850-2014.xlsx \
	#(ACTIV)/metals/Pig_Iron_Production_US.csv \
	#(ACTIV)/metals/Pig_Iron_Production_Mitchell.csv
#	Rscript $< $(EM) --nosave --no-restore

# aa3-4
# Process sintering production #TODO: is this a good place for this in the makefile?
$(MED_OUT)/A.Sintering_production.csv: \
	$(MOD_A)/A3.5.proc_sintering.R
	Rscript $< $(EM) --nosave --no-restore

# aa4-1
# naming note: includes both module A3 and A4
# Expands energy data to include all possible id combinations
# Splits energy combustion data and energy activity data
$(MED_OUT)/A.default_comb_activity_with_other.csv: \
	$(MOD_A)/A4.1.default_modern_energy_data.R \
	$(MED_OUT)/A.IEA_BP_energy_ext.csv \
	$(MAPPINGS)/Master_Fuel_Sector_List.xlsx
	Rscript $< $(EM) --nosave --no-restore

$(MED_OUT)/A.NC_activity_energy.csv: \
	$(MED_OUT)/A.default_comb_activity_with_other.csv

$(MED_OUT)/A.other_IEA_energy_values.csv	: \
	$(MED_OUT)/A.default_comb_activity_with_other.csv

$(MED_OUT)/A.Other_transformation_fuel.csv: \
	$(MED_OUT)/A.default_comb_activity_with_other.csv

# aa5-1
# BRANCH BLOCK
# Generates the process activity database
$(MED_OUT)/A.NC_activity_db.csv: \
	$(MOD_A)/A5.1.base_NC_activity.R \
	$(MOD_A)/A5.2.add_NC_activity_smelting.R \
	$(MOD_A)/A5.2.add_NC_activity_pulp_paper_consumption.R \
	$(MOD_A)/A5.2.add_NC_activity_pulp_paper_production.R \
	$(MOD_A)/A5.2.add_NC_activity_gdp.R \
	$(MOD_A)/A5.2.add_NC_activity_population.R \
	$(MOD_A)/A5.2.add_NC_activity_energy.R \
	$(MOD_A)/A5.2.add_NC_activity_fossil_fuel_production.R \
	$(MOD_A)/A5.2.add_NC_activity_aluminum_production.R \
	$(MOD_A)/A3.3.proc_pig_iron.R \
	$(PARAMS)/common_data.R \
	$(PARAMS)/global_settings.R \
	$(PARAMS)/IO_functions.R \
	$(PARAMS)/data_functions.R \
	$(PARAMS)/analysis_functions.R \
	$(PARAMS)/timeframe_functions.R \
	$(PARAMS)/process_db_functions.R \
	$(MAPPINGS)/activity_input_mapping.csv \
	$(MED_OUT)/A.other_IEA_energy_values.csv \
	$(MED_OUT)/A.en_stat_sector_fuel.csv \
	$(MAPPINGS)/NC_EDGAR_sector_mapping.csv \
	$(MAPPINGS)/2011_NC_SO2_ctry.csv \
	$(MAPPINGS)/Master_Country_List.csv \
	$(MED_OUT)/A.UN_pop_master.csv \
	$(ACTIV)/Smelter-Feedstock-Sulfur.xlsx \
	$(ACTIV)/Wood_Pulp_Consumption.xlsx \
	$(ACTIV)/GDP.xlsx \
	$(ENERGY_DATA)/Hyde_oil_1800-1975.xls
	Rscript $< $(EM) --nosave --no-restore
	Rscript $(word 2,$^) $(EM) --nosave --no-restore
	Rscript $(word 3,$^) $(EM) --nosave --no-restore
	Rscript $(word 4,$^) $(EM) --nosave --no-restore
	Rscript $(word 5,$^) $(EM) --nosave --no-restore
	Rscript $(word 6,$^) $(EM) --nosave --no-restore
	Rscript $(word 7,$^) $(EM) --nosave --no-restore
	Rscript $(word 8,$^) $(EM) --nosave --no-restore
	Rscript $(word 9,$^) $(EM) --nosave --no-restore
	Rscript $(word 10,$^) $(EM) --nosave --no-restore
	Rscript $(word 11,$^) $(EM) --nosave --no-restore

$(MED_OUT)/A.pulp_paper_consumption_full.csv: \
	$(MED_OUT)/A.NC_activity_db.csv

# aa5-2a
# Make non-combustion activity data
$(MED_OUT)/A.NC_activity.csv: \
	$(MOD_A)/A5.3.proc_activity.R \
	$(MAPPINGS)/Master_Country_List.csv \
	$(MED_OUT)/A.NC_activity_db.csv
	Rscript $< $(EM) --nosave --no-restore

# aa6- Historical Energy extension
# Combustion data
$(MED_OUT)/A.full_default_sector_shares.csv: \
	$(MOD_A)/A6.1.extended_comb_sector_shares.R \
	$(EXT_IN)/CD.Bond_sector_percentages.csv \
	$(MED_OUT)/A.Other_transformation_fuel.csv \
	$(MED_OUT)/A.default_comb_activity_with_other.csv \
	$(EXT_IN)/ext_sector_breakdown_assumptions.csv \
	$(EXT_IN)/ext_sector_percents_start_assumptions.csv
	Rscript $< $(EM) --nosave --no-restore

$(MED_OUT)/A.comb_activity_extended_coal.csv: \
	$(MOD_A)/A6.2.extended_default_activity_coal.R \
	$(MED_OUT)/A.full_default_sector_shares.csv \
	$(MED_OUT)/A.Other_transformation_fuel.csv \
	$(MED_OUT)/A.default_comb_activity_with_other.csv \
	$(MED_OUT)/E.CO2_CDIAC_inventory.csv
	Rscript $< $(EM) --nosave --no-restore

$(MED_OUT)/A.comb_activity_extended_natural_gas.csv: \
	$(MOD_A)/A6.2.extended_default_activity_natural_gas.R \
	$(MED_OUT)/A.full_default_sector_shares.csv \
	$(MED_OUT)/A.Other_transformation_fuel.csv \
	$(MED_OUT)/A.default_comb_activity_with_other.csv \
	$(MED_OUT)/E.CO2_CDIAC_inventory.csv
	Rscript $< $(EM) --nosave --no-restore

$(MED_OUT)/A.comb_activity_extended_oil.csv: \
	$(MOD_A)/A6.2.extended_default_activity_oil.R \
	$(MED_OUT)/A.full_default_sector_shares.csv \
	$(MED_OUT)/A.Other_transformation_fuel.csv \
	$(MED_OUT)/A.default_comb_activity_with_other.csv \
	$(MED_OUT)/E.CO2_CDIAC_inventory.csv
	Rscript $< $(EM) --nosave --no-restore

$(MED_OUT)/A.industrial_biomass_extended.csv: \
	$(MOD_A)/A6.3.extend_industrial_biomass.R \
	$(EXT_IN)/CD.Bond_country_industrial_biomass.csv \
	$(MED_OUT)/A.default_comb_activity_with_other.csv
	Rscript $< $(EM) --nosave --no-restore

$(MED_OUT)/A.other_biomass_extended.csv: \
	$(MOD_A)/A6.3.extend_other_biomass.R \
	$(MED_OUT)/A.default_comb_activity_with_other.csv
	Rscript $< $(EM) --nosave --no-restore

# Combine all combustion extension data
$(MED_OUT)/A.comb_default_activity_extended.csv: \
	$(MOD_A)/A6.4.extended_default_comb_activity.R \
	$(MED_OUT)/A.comb_activity_extended_coal.csv \
	$(MED_OUT)/A.comb_activity_extended_oil.csv \
	$(MED_OUT)/A.comb_activity_extended_natural_gas.csv \
	$(MED_OUT)/A.other_biomass_extended.csv \
	$(MED_OUT)/A.industrial_biomass_extended.csv \
	$(MED_OUT)/A.residential_biomass_full.csv \
	$(MED_OUT)/A.default_comb_activity_with_other.csv \
	$(ENERGY_DATA)/IEA_iso_start_data.csv
	Rscript $< $(EM) --nosave --no-restore

# Non-Combustion activity data
$(MED_OUT)/A.NC_activity_extended_db.csv: \
	$(MOD_A)/A7.1.base_activity.R \
	$(MOD_A)/A7.2.add_activity.R \
	$(MOD_A)/A7.2.add_activity_CDIAC.R \
	$(MOD_A)/A7.2.add_activity_population.R \
	$(MOD_A)/A7.2.add_activity_pulp_paper_consumption.R \
	$(MOD_A)/A7.2.add_activity_fossil_fuel_production.R \
	$(MED_OUT)/E.CO2_CDIAC_inventory.csv \
	$(MED_OUT)/A.pulp_paper_consumption_full.csv \
	$(MED_OUT)/A.NC_activity.csv \
	$(MED_OUT)/A.crude_oil_production_data.csv \
	$(EXT_IN)/CEDS_historical_extension_drivers_activity.csv
	Rscript $< $(EM) --nosave --no-restore
	Rscript $(word 2,$^) $(EM) --nosave --no-restore
	Rscript $(word 3,$^) $(EM) --nosave --no-restore
	Rscript $(word 4,$^) $(EM) --nosave --no-restore

$(MED_OUT)/A.NC_default_activity_extended.csv: \
	$(MOD_A)/A7.3.proc_activity.R \
	$(MED_OUT)/A.NC_activity_extended_db.csv
	Rscript $< $(EM) --nosave --no-restore

# User-added combustion activity incorporated
$(MED_OUT)/A.comb_user_added.csv: \
	$(MOD_A)/A8.1.add_user-defined_data.R \
	$(MED_OUT)/A.comb_default_activity_extended.csv
	Rscript $< $(EM) --nosave --no-restore

# International shipping fix
$(MED_OUT)/A.comb_int_shipping_adjusted.csv: \
	$(MOD_A)/A8.2.Adjust_Shipping_Fuel_Cons.R \
	$(MED_OUT)/A.comb_user_added.csv \
	$(MED_OUT)/A.IEA_en_stat_ctry_hist.csv \
	$(EN_MAPPINGS)/IEA_product_fuel.csv \
	$(ENERGY_DATA)/Shipping_Fuel_Consumption.xlsx
	Rscript $< $(EM) --nosave --no-restore

# Final activity data
$(MED_OUT)/A.total_activity_extended.csv: \
	$(MOD_A)/A8.3.combine_extended_activity.R \
	$(MED_OUT)/A.NC_default_activity_extended.csv \
	$(MED_OUT)/A.comb_int_shipping_adjusted.csv
	Rscript $< $(EM) --nosave --no-restore

$(MED_OUT)/A.final_comb_activity_modern.csv: \
	$(MOD_A)/A8.3.combine_extended_activity.R \
	$(MED_OUT)/A.NC_default_activity_extended.csv \
	$(MED_OUT)/A.comb_int_shipping_adjusted.csv
	Rscript $< $(EM) --nosave --no-restore

$(MED_OUT)/A.total_activity_extended_coal.csv: \
	$(MED_OUT)/A.total_activity_extended.csv

$(MED_OUT)/A.total_activity_extended_natural_gas.csv: \
	$(MED_OUT)/A.total_activity_extended.csv

$(MED_OUT)/A.total_activity_extended_oil.csv: \
	$(MED_OUT)/A.total_activity_extended.csv

# bb1-1
#$(MED_OUT)/B.$(EM)_comb_EF_GAINS_EMF30.csv: \
	$(MOD_B)/B1.1.add_comb_GAINS_EMF-30.R.R \
	$(ENERGY_DATA)/OECD_and_NonOECD_Conversion_Factors.csv \
	$(INV_DATA)/GAINS/Global by region-detail_emf30_activity_wSE.csv \
	$(INV_DATA)/GAINS/Global by region-detail_emf30_BC_wSE.csv \
	$(INV_DATA)/GAINS/Global by region-detail_emf30_CH4_wSE.csv \
	$(INV_DATA)/GAINS/Global by region-detail_emf30_CO_wSE.csv \
	$(INV_DATA)/GAINS/Global by region-detail_emf30_CO2_wSE.csv \
	$(INV_DATA)/GAINS/Global by region-detail_emf30_NH3_wSE.csv \
	$(INV_DATA)/GAINS/Global by region-detail_emf30_NOx_wSE.csv \
	$(INV_DATA)/GAINS/Global by region-detail_emf30_OC_wSE.csv \
	$(INV_DATA)/GAINS/Global by region-detail_emf30_SO2_v2_wSE.csv \
	$(INV_DATA)/GAINS/Global by region-detail_emf30_VOC_wSE.csv \
	$(MAPPINGS)/GAINS/emf-30_ctry_map.csv \
	$(MAPPINGS)/GAINS/emf-30_fuel_sector_map.csv
#	Rscript $< $(EM) --nosave --no-restore

#$(MED_OUT)/B1.1.GAINS_heat_content.csv: \
	$(MED_OUT)/B.$(EM)_comb_EF_GAINS_EMF30.csv

# Generates the base file of combustion emissions factors
# by calling a daughter script for the relevant emissions type
$(MED_OUT)/B.$(EM)_comb_EF_db.csv: \
	$(MOD_B)/B1.1.base_comb_EF.R \
	$(MOD_B)/B1.2.add_comb_EF.R \
	$(MOD_B)/B1.1.base_BCOC_comb_EF.R \
	$(MOD_B)/B1.1.base_CO2_comb_EF.R \
	$(MOD_B)/B1.1.base_N2O_comb_EF.R \
	$(MOD_B)/B1.1.base_OTHER_comb_EF.R \
	$(MOD_B)/B1.1.base_comb_EF_control_percent.R \
	$(MOD_B)/B1.1.base_SO2_comb_EF_parameters.R \
	$(MOD_B)/B1.1.base_comb_GAINS_EMF-30.R \
	$(MOD_B)/B1.2.add_comb_control_percent.R \
	$(MOD_B)/B1.2.add_comb_default_EF.R \
	$(MOD_B)/B1.2.add_SO2_comb_diesel_sulfur_content.R \
	$(MOD_B)/B1.2.add_SO2_comb_GAINS_ash_ret.R \
	$(MOD_B)/B1.2.add_SO2_comb_GAINS_control_percent.R \
	$(MOD_B)/B1.2.add_SO2_recent_control_percent.R \
	$(MOD_B)/B1.2.add_SO2_comb_GAINS_s_content.R \
	$(MOD_B)/B1.2.add_SO2_comb_S_content_ash.R \
	$(MOD_B)/B1.3.proc_SO2_comb_EF_S_content_ash.R \
	$(MOD_B)/B1.3.proc_comb_EF_control_percent.R \
	$(PARAMS)/timeframe_functions.R \
	$(PARAMS)/process_db_functions.R \
	$(PARAMS)/analysis_functions.R \
	$(PARAMS)/interpolation_extension_functions.R \
	$(EF_DATA)/SO2_base_EF.csv \
	$(MED_OUT)/A.final_comb_activity_modern.csv \
	$(MAPPINGS)/Bond/Bond_country_map.csv \
	$(MAPPINGS)/Bond/Bond_fuel_map.csv \
	$(MAPPINGS)/Bond/Bond_sector_map.csv \
	$(EXT_IN)/CD.Bond_country_industrial_biomass.csv \
	$(MED_OUT)/A.coal_heat_content.csv \
	$(EF_DATA)/CO2_base_EF.xlsx
	Rscript $< $(EM) --nosave --no-restore
	Rscript $(word 2,$^) $(EM) --nosave --no-restore

# cc1-1
# BRANCH BLOCK
# Converts process emissions data into CEDS Standard and
# generates the default process emissions database.
$(MED_OUT)/C.$(EM)_NC_emissions_db.csv: \
	$(MOD_C)/C1.1.base_NC_emissions.R \
	$(MOD_C)/C1.2.add_NC_emissions.R \
	$(MOD_C)/C1.2.add_SO2_NC_emissions_all.R \
	$(MOD_C)/C1.2.GAINS_fugitive_petr_gas_emissions.R \
	$(MOD_C)/C1.2.add_NC_emissions_EDGAR.R \
	$(MOD_C)/C1.2.EPA_adipic_and_nitric_acid.R \
	$(MOD_C)/C1.2.Adipic_nitric_acid_default_process_emissions.R \
	$(MOD_C)/C1.2.add_CH4_NC_emissions_FAO.R \
	$(MOD_C)/C1.2.add_N2O_NC_emissions_FAO.R\
	$(MOD_C)/C1.2.ECLIPSE_flaring_emissions_extension.R \
	$(MOD_C)/C1.2.Fugitive-petr-and-gas_default_process_emissions.R \
	$(MOD_C)/C1.2.add_CO2_NC_emissions_Andrew.R \
	$(MAPPINGS)/NC_EDGAR_sector_mapping.csv \
	$(PARAMS)/common_data.R \
	$(PARAMS)/global_settings.R \
	$(PARAMS)/IO_functions.R \
	$(PARAMS)/data_functions.R \
	$(PARAMS)/analysis_functions.R \
	$(PARAMS)/timeframe_functions.R \
	$(PARAMS)/process_db_functions.R \
	$(MAPPINGS)/sector_input_mapping.xlsx \
	$(ACTIV)/Process_SO2_Emissions_to_2005.xlsx \
	$(MED_OUT)/E.CO2_Andrew_Cement.csv \
	$(MED_OUT)/E.CO2_CDIAC_inventory.csv
	Rscript $< $(EM) --nosave --no-restore
	Rscript $(word 2,$^) $(EM) --nosave --no-restore

# cc1-2
$(MED_OUT)/C.$(EM)_NC_emissions.csv: \
	$(MOD_C)/C1.3.proc_NC_emissions.R \
	$(MOD_C)/C1.3.proc_NC_emissions_user_added_waste.R \
	$(MOD_C)/C1.3.proc_NC_emissions_user_added_wastewater.R \
	$(MOD_C)/C1.3.proc_NC_emissions_user_added_inventories.R \
	$(MOD_C)/C1.3.proc_NC_emissions_user_added.R \
	$(MED_OUT)/C.$(EM)_NC_emissions_db.csv \
	$(MAPPINGS)/Master_Fuel_Sector_List.xlsx \
	$(MAPPINGS)/Master_Country_List.csv \
	$(MED_OUT)/A.NC_activity.csv \
	$(MED_OUT)/E.$(EM)_ARG_inventory.csv \
	$(MED_OUT)/E.$(EM)_CAN_to2011_inventory.csv \
	$(MED_OUT)/E.$(EM)_CAN_2018_inventory.csv \
	$(MED_OUT)/E.$(EM)_CHN_inventory.csv \
	$(MED_OUT)/E.$(EM)_CHN_2018_inventory.csv \
	$(MED_OUT)/E.$(EM)_EMEP_NFR09_inventory.csv \
	$(MED_OUT)/E.$(EM)_EMEP_NFR14_inventory.csv \
	$(MED_OUT)/E.$(EM)_Japan_inventory.csv \
	$(MED_OUT)/E.$(EM)_KOR2017_inventory.csv \
	$(MED_OUT)/E.$(EM)_REAS_inventory.csv \
	$(MED_OUT)/E.$(EM)_REAS32_inventory.csv \
	$(MED_OUT)/E.$(EM)_UNFCCC_inventory.csv \
	$(MED_OUT)/E.$(EM)_UNFCCC_update_inventory.csv \
	$(MED_OUT)/E.$(EM)_US_inventory.csv \
	$(MED_OUT)/E.$(EM)_US-EPA_inventory.csv \
	$(MED_OUT)/E.$(EM)_US_GHG_inventory.csv \
	$(MED_OUT)/A.UN_pop_master.csv \
	$(ACTIV)/wastewater/UN_Percentage_WW_Treatment.xlsx \
	$(ACTIV)/wastewater/OECD_Percentage_WW_Treatment.xlsx \
	$(ACTIV)/wastewater/auxiliary_ww_treatment_trend.csv \
	$(ACTIV)/wastewater/Per-Capita_Protein_and_WW_NH3.csv \
	$(MAPPINGS)/wastewater_proxy_mapping.csv
	Rscript $< $(EM) --nosave --no-restore
	Rscript $(word 2,$^) $(EM) --nosave --no-restore
	Rscript $(word 3,$^) $(EM) --nosave --no-restore
	Rscript $(word 4,$^) $(EM) --nosave --no-restore
	Rscript $(word 5,$^) $(EM) --nosave --no-restore

# cc2-1
$(MED_OUT)/C.$(EM)_NC_EF.csv: \
	$(MOD_C)/C2.1.base_NC_EF.R \
	$(MOD_C)/C1.2.add_NC_default_EF.R \
	$(MED_OUT)/A.NC_activity.csv \
	$(MED_OUT)/C.$(EM)_NC_emissions.csv
	Rscript $< $(EM) --nosave --no-restore
	Rscript $(word 2,$^) $(EM) --nosave --no-restore

# cc2-2 #TODO: is this a good place for this?
$(MED_OUT)/C.$(EM)_sintering_emissions.csv: \
	$(MOD_C)/C3.1.calc_sintering_emissions.R \
	$(MED_OUT)/C.$(EM)_NC_EF.csv \
	$(MED_OUT)/A.Sintering_production.csv
	Rscript $< $(EM) --nosave --no-restore



# dd1-1
# Calculates  NC and combustion emissions from activity data and
# emissions factors then combines to total EF and total emissions

$(MED_OUT)/D.$(EM)_default_total_emissions.csv: \
	$(MOD_D)/D1.1.default_emissions.R \
	$(MED_OUT)/A.final_comb_activity_modern.csv \
	$(MED_OUT)/A.NC_activity.csv \
	$(MED_OUT)/B.$(EM)_comb_EF_db.csv \
	$(MED_OUT)/C.$(EM)_sintering_emissions.csv \
	$(MED_OUT)/C.$(EM)_NC_EF.csv
	Rscript $< $(EM) --nosave --no-restore

# dd1-2
$(MED_OUT)/D.$(EM)_default_total_EF.csv: \
	$(MED_OUT)/D.$(EM)_default_total_emissions.csv
# dd1-3
$(MED_OUT)/D.$(EM)_default_comb_emissions.csv: \
	$(MED_OUT)/D.$(EM)_default_total_emissions.csv
# dd1-4
$(MED_OUT)/D.$(EM)_default_nc_emissions.csv: \
	$(MED_OUT)/D.$(EM)_default_total_emissions.csv

# ee1-1
# Creates formatted emissions inventory
$(MED_OUT)/E.$(EM)_UNFCCC_inventory.csv: \
	$(MOD_E)/E.UNFCCC_emissions.R
	Rscript $< $(EM) --nosave --no-restore

# ee1-2
$(MED_OUT)/E.$(EM)_UNFCCC_update_inventory.csv: \
	$(MOD_E)/E.UNFCCC_emissions_update.R
	Rscript $< $(EM) --nosave --no-restore

# ee1-2
$(MED_OUT)/E.$(EM)_EMEP_NFR09_inventory.csv: \
	$(MOD_E)/E.EMEP_emissions.R
	Rscript $< $(EM) NFR09 --nosave --no-restore

# ee1-2
$(MED_OUT)/E.$(EM)_EMEP_NFR14_inventory.csv: \
	$(MOD_E)/E.EMEP_emissions.R
	Rscript $< $(EM) NFR14 --nosave --no-restore

# ee1-2
$(MED_OUT)/E.CO2_CDIAC_inventory.csv: \
	$(MOD_E)/E.CDIAC_emissions.R \
	$(MED_OUT)/A.UN_pop_master.csv
	Rscript $< $(EM) --nosave --no-restore

# ee1-2
$(MED_OUT)/E.CO2_Andrew_Cement.csv: \
	$(MOD_E)/E.Andrew_emissions.R \
	$(MED_OUT)/A.UN_pop_master.csv
	Rscript $< $(EM) --nosave --no-restore

# ee1-2
$(MED_OUT)/E.$(EM)_REAS_inventory.csv: \
	$(MOD_E)/E.REAS_emissions.R
	Rscript $< $(EM) --nosave --no-restore

# ee1-2
$(MED_OUT)/E.$(EM)_REAS32_inventory.csv: \
	$(MOD_E)/E.REAS32_emissions.R
	Rscript $< $(EM) --nosave --no-restore

# ee1-2
$(MED_OUT)/E.$(EM)_ARG_inventory.csv: \
	$(MOD_E)/E.Argentina_emissions.R
	Rscript $< $(EM) --nosave --no-restore

# ee1-2
$(MED_OUT)/E.$(EM)_CAN_to2011_inventory.csv: \
	$(MOD_E)/E.CAN_emissions_olderData.R
	Rscript $< $(EM) --nosave --no-restore

# ee1-2
$(MED_OUT)/E.$(EM)_CAN_2018_inventory.csv: \
	$(MOD_E)/E.CAN_emissions_2018.R
	Rscript $< $(EM) --nosave --no-restore

# ee1-2
$(MED_OUT)/E.$(EM)_CHN_inventory.csv: \
	$(MOD_E)/E.China_emissions.R
	Rscript $< $(EM) --nosave --no-restore

# ee1-2
$(MED_OUT)/E.$(EM)_CHN_2018_inventory.csv: \
	$(MOD_E)/E.China_emissions_MEIC_2018.R
	Rscript $< $(EM) --nosave --no-restore

# ee1-2
$(MED_OUT)/E.$(EM)_Japan_inventory.csv: \
	$(MOD_E)/E.Japan_emissions.R
	Rscript $< $(EM) --nosave --no-restore

# ee1-2
$(MED_OUT)/E.$(EM)_KOR2017_inventory.csv: \
	$(MOD_E)/E.SKorea_emissions_2017.R
	Rscript $< $(EM) --nosave --no-restore

# ee1-2
$(MED_OUT)/E.$(EM)_US_inventory.csv: \
	$(MOD_E)/E.US_emissions.R
	Rscript $< $(EM) --nosave --no-restore

# ee1-2
$(MED_OUT)/E.$(EM)_US-EPA_inventory.csv: \
	$(MOD_E)/E.US-EPA_emissions.R
	Rscript $< $(EM) --nosave --no-restore

# ee1-2
$(MED_OUT)/E.$(EM)_US_GHG_inventory.csv: \
	$(MOD_E)/E.US-GHG_emissions.R
	Rscript $< $(EM) --nosave --no-restore

# ee1-2
$(MED_OUT)/E.$(EM)_AUS_inventory.csv: \
	$(MOD_E)/E.Australia_emissions_2018Update.R
	Rscript $< $(EM) --nosave --no-restore

# ee1-2
$(MED_OUT)/E.$(EM)_TWN_inventory.csv: \
	$(MOD_E)/E.Taiwan_emissions.R
	Rscript $< $(EM) --nosave --no-restore

# ff1-1a
# Creates scaled emissions and emissions factors
$(MED_OUT)/F.$(EM)_scaled_emissions.csv: \
	$(MOD_F)/F1.inventory_scaling.R \
	$(MOD_F)/F1.1.Argentina_scaling.R \
	$(MOD_F)/F1.1.CAN_scaling_olderData.R \
	$(MOD_F)/F1.1.CAN_scaling_2018.R \
	$(MOD_F)/F1.1.China_scaling.R \
	$(MOD_F)/F1.1.China_MEIC_2018_scaling.R \
	$(MOD_F)/F1.1.Edgar_scaling.R \
	$(MOD_F)/F1.1.EMEP_NFR09_scaling.R \
	$(MOD_F)/F1.1.EMEP_NFR14_scaling.R \
	$(MOD_F)/F1.1.Japan_scaling.R \
	$(MOD_F)/F1.1.REAS_scaling.R \
	$(MOD_F)/F1.1.REAS32_scaling.R \
	$(MOD_F)/F1.1.South_korea_scaling_2017.R \
	$(MOD_F)/F1.1.UNFCCC_scaling.R \
	$(MOD_F)/F1.1.US_scaling.R \
	$(MOD_F)/F1.1.US-EPA_scaling.R \
	$(MOD_F)/F1.1.US-GHG_scaling.R \
	$(MOD_F)/F1.1.Australia_scaling.R \
	$(MOD_F)/F1.1.Taiwan_scaling.R \
	$(PARAMS)/emissions_scaling_functions.R \
	$(MED_OUT)/E.$(EM)_ARG_inventory.csv \
	$(MED_OUT)/E.$(EM)_CAN_to2011_inventory.csv \
	$(MED_OUT)/E.$(EM)_CAN_2018_inventory.csv \
	$(MED_OUT)/E.$(EM)_CHN_inventory.csv \
	$(MED_OUT)/E.$(EM)_CHN_2018_inventory.csv \
	$(MED_OUT)/E.$(EM)_EMEP_NFR09_inventory.csv \
	$(MED_OUT)/E.$(EM)_EMEP_NFR14_inventory.csv \
	$(MED_OUT)/E.$(EM)_Japan_inventory.csv \
	$(MED_OUT)/E.$(EM)_KOR2017_inventory.csv \
	$(MED_OUT)/E.$(EM)_REAS_inventory.csv \
	$(MED_OUT)/E.$(EM)_REAS32_inventory.csv \
	$(MED_OUT)/E.$(EM)_UNFCCC_inventory.csv \
	$(MED_OUT)/E.$(EM)_US_inventory.csv \
	$(MED_OUT)/E.$(EM)_US-EPA_inventory.csv \
	$(MED_OUT)/E.$(EM)_US_GHG_inventory.csv \
	$(MED_OUT)/E.$(EM)_AUS_inventory.csv \
	$(MED_OUT)/E.$(EM)_TWN_inventory.csv \
	$(SC_MAPPINGS)/Argentina_scaling_mapping.csv \
	$(SC_MAPPINGS)/CAN_scaling_mapping.csv \
	$(SC_MAPPINGS)/CAN_NOx_scaling_mapping.csv \
	$(SC_MAPPINGS)/CAN_2018_scaling_mapping.csv \
	$(SC_MAPPINGS)/CAN_2018_NOx_scaling_mapping.csv \
	$(SC_MAPPINGS)/MEIC_scaling_mapping.csv \
	$(SC_MAPPINGS)/MEIC_2018_scaling_mapping.csv \
	$(SC_MAPPINGS)/Edgar_scaling_mapping.csv \
	$(SC_MAPPINGS)/Edgar_scaling_year.csv \
	$(SC_MAPPINGS)/EMEP_NFR09_scaling_mapping.csv \
	$(SC_MAPPINGS)/EMEP_NFR09_scaling_year.csv \
	$(SC_MAPPINGS)/EMEP_NFR09_SO2_scaling_mapping.csv \
	$(SC_MAPPINGS)/EMEP_NFR09_SO2_scaling_year.csv \
	$(SC_MAPPINGS)/EMEP_NFR09_SO2_scaling_method.csv \
	$(SC_MAPPINGS)/EMEP_NFR14_scaling_mapping.csv \
	$(SC_MAPPINGS)/EMEP_NFR14_scaling_year.csv \
	$(SC_MAPPINGS)/EMEP_NFR14_SO2_scaling_mapping.csv \
	$(SC_MAPPINGS)/EMEP_NFR14_SO2_scaling_year.csv \
	$(SC_MAPPINGS)/EMEP_NFR14_SO2_scaling_method.csv \
	$(SC_MAPPINGS)/jpn_scaling_mapping.csv \
	$(SC_MAPPINGS)/REAS_scaling_mapping.csv \
	$(SC_MAPPINGS)/REAS32_scaling_mapping.csv \
	$(SC_MAPPINGS)/S_Korea_scaling_mapping.csv \
	$(SC_MAPPINGS)/UNFCCC_scaling_mapping.csv \
	$(SC_MAPPINGS)/UNFCCC_scaling_year.csv \
	$(SC_MAPPINGS)/UNFCCC_GHG_scaling_mapping.csv \
	$(SC_MAPPINGS)/US_scaling_mapping.csv \
	$(SC_MAPPINGS)/US-EPA_scaling_mapping.csv \
	$(SC_MAPPINGS)/US-GHG_scaling_mapping.csv \
	$(SC_MAPPINGS)/Australia_scaling_mapping.csv \
	$(SC_MAPPINGS)/Taiwan_scaling_mapping.csv \
	$(SC_MAPPINGS)/Taiwan_scaling_method.csv \
	$(MED_OUT)/D.$(EM)_default_total_EF.csv \
	$(MED_OUT)/D.$(EM)_default_total_emissions.csv
	Rscript $< $(EM) --nosave --no-restore

# ff1-1b
$(MED_OUT)/F.$(EM)_scaled_EF.csv: \
	$(MED_OUT)/F.$(EM)_scaled_emissions.csv

# Module H

$(MED_OUT)/H.$(EM)_total_EFs_adjusted-sector.csv: \
	$(MOD_H)/H1.1.replace_EF_sectors.R \
	$(MED_OUT)/F.$(EM)_scaled_EF.csv
	Rscript $< $(EM) --nosave --no-restore

$(MED_OUT)/H.$(EM)_total_EFs_extended.csv: \
	$(MOD_H)/H2.1.base_EFs.R \
	$(MOD_H)/H2.2.add_EFs.R \
	$(MOD_H)/H2.3.proc_EFs.R \
	$(MOD_H)/H2.2.add_EFs_constant.R \
	$(MOD_H)/H2.2.add_EFs_default.R \
	$(MOD_H)/H2.2.add_EFs_EF-converge.R \
	$(MOD_H)/H2.2.add_EFs_EF-trend.R \
	$(MOD_H)/H2.2.add_EFs_Emissions-trend.R \
	$(EXT_IN)/CEDS_historical_extension_methods_EF.csv \
	$(EXT_IN)/extension-data/A.Pig_Iron_Production.csv \
	$(MED_OUT)/H.$(EM)_total_EFs_adjusted-sector.csv \
	$(MED_OUT)/A.total_activity_extended.csv
	Rscript $< $(EM) --nosave --no-restore
	Rscript $(word 2,$^) $(EM) --nosave --no-restore
	Rscript $(word 3,$^) $(EM) --nosave --no-restore

$(MED_OUT)/H.$(EM)_total_EFs_extended_adjusted-pathway.csv: \
	$(MOD_H)/H3.1.apply_EF_pathway.R \
	$(MAPPINGS)/Master_Country_List.csv \
	$(MED_OUT)/H.$(EM)_total_EFs_extended.csv
	Rscript $< $(EM) --nosave --no-restore

$(MED_OUT)/$(EM)_total_CEDS_emissions.csv: \
	$(MOD_H)/H3.2.proc_Extended_Emissions.R \
	$(MED_OUT)/H.$(EM)_total_EFs_extended_adjusted-pathway.csv \
	$(MED_OUT)/A.total_activity_extended.csv \
	$(MOD_H)/H3.3.add_emissions_SO2_other_transformation.R \
	$(MOD_H)/H3.3.add_emissions_CO2_other_transformation.R
	Rscript $< $(EM) --nosave --no-restore

$(FINAL_OUT)/current-versions/CEDS_$(EM)_emissions_by_country_sector_%.csv: \
	$(MOD_S)/S1.1.write_summary_data.R \
	$(MED_OUT)/$(EM)_total_CEDS_emissions.csv
	Rscript $< $(EM) --nosave --no-restore



# Gridded emissions scripts --------------------------------------------------

$(FINAL_OUT)/current-versions/CEDS_$(EM)_emissions_by_country_CEDS_sector_%.csv: \
	$(FINAL_OUT)/current-versions/CEDS_$(EM)_emissions_by_country_sector_%.csv
	@echo "Gridding from output file:\n$$(ls $(FINAL_OUT)/current-versions/CEDS_$(EM)_emissions_by_country_CEDS_sector_*.csv)\n"

# Each block below first generates yearly grids (and .csv totals) as
# intermediate output, then chunks them together into larger final files.

# Bulk emissions
$(MED_OUT)/gridded-emissions/CEDS_$(EM)_anthro_%.csv: \
	$(MOD_G)/G1.1.grid_bulk_emissions.R \
	$(PARAMS)/gridding_functions.R \
	$(PARAMS)/nc_generation_functions.R \
	$(FINAL_OUT)/current-versions/CEDS_$(EM)_emissions_by_country_CEDS_sector_*.csv
	Rscript $< $(EM) --nosave --no-restore

ifeq ($(EM),NMVOC)
	Rscript $(MOD_G)/G1.2.grid_subVOC_emissions.R VOC01 --nosave --no-restore
	Rscript $(MOD_G)/G1.2.grid_subVOC_emissions.R VOC02 --nosave --no-restore
	Rscript $(MOD_G)/G1.2.grid_subVOC_emissions.R VOC03 --nosave --no-restore
	Rscript $(MOD_G)/G1.2.grid_subVOC_emissions.R VOC04 --nosave --no-restore
	Rscript $(MOD_G)/G1.2.grid_subVOC_emissions.R VOC05 --nosave --no-restore
	Rscript $(MOD_G)/G1.2.grid_subVOC_emissions.R VOC06 --nosave --no-restore
	Rscript $(MOD_G)/G1.2.grid_subVOC_emissions.R VOC07 --nosave --no-restore
	Rscript $(MOD_G)/G1.2.grid_subVOC_emissions.R VOC08 --nosave --no-restore
	Rscript $(MOD_G)/G1.2.grid_subVOC_emissions.R VOC09 --nosave --no-restore
	Rscript $(MOD_G)/G1.2.grid_subVOC_emissions.R VOC12 --nosave --no-restore
	Rscript $(MOD_G)/G1.2.grid_subVOC_emissions.R VOC13 --nosave --no-restore
	Rscript $(MOD_G)/G1.2.grid_subVOC_emissions.R VOC14 --nosave --no-restore
	Rscript $(MOD_G)/G1.2.grid_subVOC_emissions.R VOC15 --nosave --no-restore
	Rscript $(MOD_G)/G1.2.grid_subVOC_emissions.R VOC16 --nosave --no-restore
	Rscript $(MOD_G)/G1.2.grid_subVOC_emissions.R VOC17 --nosave --no-restore
	Rscript $(MOD_G)/G1.2.grid_subVOC_emissions.R VOC18 --nosave --no-restore
	Rscript $(MOD_G)/G1.2.grid_subVOC_emissions.R VOC19 --nosave --no-restore
	Rscript $(MOD_G)/G1.2.grid_subVOC_emissions.R VOC20 --nosave --no-restore
	Rscript $(MOD_G)/G1.2.grid_subVOC_emissions.R VOC21 --nosave --no-restore
	Rscript $(MOD_G)/G1.2.grid_subVOC_emissions.R VOC22 --nosave --no-restore
	Rscript $(MOD_G)/G1.2.grid_subVOC_emissions.R VOC23 --nosave --no-restore
	Rscript $(MOD_G)/G1.2.grid_subVOC_emissions.R VOC24 --nosave --no-restore
	Rscript $(MOD_G)/G1.2.grid_subVOC_emissions.R VOC25 --nosave --no-restore
endif

$(FINAL_OUT)/gridded-emissions/$(EM)-em-anthro*: \
	$(MOD_G)/G2.1.chunk_bulk_emissions.R \
	$(MED_OUT)/gridded-emissions/CEDS_$(EM)_anthro_*.csv
	rm -fv $(FINAL_OUT)/gridded-emissions/$(EM)-em-anthro*
	Rscript $< $(EM) --nosave --no-restore

ifeq ($(EM),NMVOC)
	Rscript $(MOD_G)/G2.2.chunk_subVOC_emissions.R VOC01 --nosave --no-restore
	Rscript $(MOD_G)/G2.2.chunk_subVOC_emissions.R VOC02 --nosave --no-restore
	Rscript $(MOD_G)/G2.2.chunk_subVOC_emissions.R VOC03 --nosave --no-restore
	Rscript $(MOD_G)/G2.2.chunk_subVOC_emissions.R VOC04 --nosave --no-restore
	Rscript $(MOD_G)/G2.2.chunk_subVOC_emissions.R VOC05 --nosave --no-restore
	Rscript $(MOD_G)/G2.2.chunk_subVOC_emissions.R VOC06 --nosave --no-restore
	Rscript $(MOD_G)/G2.2.chunk_subVOC_emissions.R VOC07 --nosave --no-restore
	Rscript $(MOD_G)/G2.2.chunk_subVOC_emissions.R VOC08 --nosave --no-restore
	Rscript $(MOD_G)/G2.2.chunk_subVOC_emissions.R VOC09 --nosave --no-restore
	Rscript $(MOD_G)/G2.2.chunk_subVOC_emissions.R VOC12 --nosave --no-restore
	Rscript $(MOD_G)/G2.2.chunk_subVOC_emissions.R VOC13 --nosave --no-restore
	Rscript $(MOD_G)/G2.2.chunk_subVOC_emissions.R VOC14 --nosave --no-restore
	Rscript $(MOD_G)/G2.2.chunk_subVOC_emissions.R VOC15 --nosave --no-restore
	Rscript $(MOD_G)/G2.2.chunk_subVOC_emissions.R VOC16 --nosave --no-restore
	Rscript $(MOD_G)/G2.2.chunk_subVOC_emissions.R VOC17 --nosave --no-restore
	Rscript $(MOD_G)/G2.2.chunk_subVOC_emissions.R VOC18 --nosave --no-restore
	Rscript $(MOD_G)/G2.2.chunk_subVOC_emissions.R VOC19 --nosave --no-restore
	Rscript $(MOD_G)/G2.2.chunk_subVOC_emissions.R VOC20 --nosave --no-restore
	Rscript $(MOD_G)/G2.2.chunk_subVOC_emissions.R VOC21 --nosave --no-restore
	Rscript $(MOD_G)/G2.2.chunk_subVOC_emissions.R VOC22 --nosave --no-restore
	Rscript $(MOD_G)/G2.2.chunk_subVOC_emissions.R VOC23 --nosave --no-restore
	Rscript $(MOD_G)/G2.2.chunk_subVOC_emissions.R VOC24 --nosave --no-restore
	Rscript $(MOD_G)/G2.2.chunk_subVOC_emissions.R VOC25 --nosave --no-restore
endif


# Biofuel emissions
$(MED_OUT)/gridded-emissions/CEDS_$(EM)_solidbiofuel_anthro_%.csv: \
	$(MOD_G)/G1.4.grid_solidbiofuel_emissions.R \
	$(PARAMS)/gridding_functions.R \
	$(PARAMS)/nc_generation_functions.R \
	$(MED_OUT)/$(EM)_total_CEDS_emissions.csv
	Rscript $< $(EM) --nosave --no-restore

$(FINAL_OUT)/gridded-emissions/$(EM)-em-SOLID-BIOFUEL-anthro*: \
	$(MOD_G)/G2.4.chunk_solidbiofuel_emissions.R \
	$(MED_OUT)/gridded-emissions/CEDS_$(EM)_solidbiofuel_anthro_*.csv
	rm -fv $(FINAL_OUT)/gridded-emissions/$(EM)-em-SOLID-BIOFUEL-anthro*
	Rscript $< $(EM) --nosave --no-restore


# Aircraft emissions
$(MED_OUT)/gridded-emissions/CEDS_$(EM)_AIR_anthro_%.csv: \
	$(MOD_G)/G1.3.grid_aircraft_emissions.R \
	$(PARAMS)/gridding_functions.R \
	$(PARAMS)/nc_generation_functions.R \
	$(FINAL_OUT)/current-versions/CEDS_$(EM)_emissions_by_country_CEDS_sector_*.csv
	Rscript $< $(EM) --nosave --no-restore

$(FINAL_OUT)/gridded-emissions/$(EM)-em-AIR-anthro*: \
	$(MOD_G)/G2.3.chunk_aircraft_emissions.R \
	$(MED_OUT)/gridded-emissions/CEDS_$(EM)_AIR_anthro_*.csv
	rm -fv $(FINAL_OUT)/gridded-emissions/$(EM)-em-AIR-anthro*
	Rscript $< $(EM) --nosave --no-restore


# Special case for CH4 emissions
ifeq ($(EM),CH4)
$(MED_OUT)/H.CH4_RCP_Back-Extended.csv: \
	code/supplement/H.Extend_CH4_RCP.R \
	$(MED_OUT)/CH4_total_CEDS_emissions.csv
	Rscript $< $(EM) --nosave --no-restore

$(MED_OUT)/gridded-emissions/CEDS_CH4_anthro_%.csv: \
	$(MOD_G)/G3.1.grid_extended_CH4_bulk.R \
	$(MOD_G)/G3.2.grid_extended_CH4_air.R \
	$(PARAMS)/gridding_functions.R \
	$(PARAMS)/nc_generation_functions.R \
	$(MED_OUT)/$(EM)_total_CEDS_emissions.csv \
	$(MED_OUT)/H.CH4_RCP_Back-Extended.csv
	Rscript $< $(EM) --nosave --no-restore
	Rscript $(word 2,$^) $(EM) --nosave --no-restore

$(FINAL_OUT)/gridded-emissions/CH4-em-anthro*: \
	$(MOD_G)/G3.3.chunk_CH4_extended_bulk_emissions.R \
	$(MED_OUT)/gridded-emissions/CEDS_CH4_anthro_*.csv
	rm -fv $(FINAL_OUT)/gridded-emissions/CH4-em-anthro*
	Rscript $< $(EM) --nosave --no-restore

$(FINAL_OUT)/gridded-emissions/CH4-em-AIR-anthro*: \
	$(MOD_G)/G3.4.chunk_CH4_extended_aircraft_emissions.R \
	$(MED_OUT)/gridded-emissions/CEDS_CH4_AIR_anthro_*.csv
	rm -fv $(FINAL_OUT)/gridded-emissions/CH4-em-AIR-anthro*
	Rscript $< $(EM) --nosave --no-restore
endif
