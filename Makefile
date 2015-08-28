# Specify directories
MOD_A = code/module-A
MOD_B = code/module-B
MOD_C = code/module-C
MOD_D = code/module-D
MOD_E = code/module-E
PARAMS = code/parameters
SOCIO_DATA = input/general
ENERGY_DATA = input/energy
EF_DATA = input/default-emissions-data
MAPPINGS = input/mappings
ACTIV = input/activity
INV_DATA = input/emissions-inventories
MED_OUT = intermediate-output
DIAG_OUT = diagnostic-output
FINAL_OUT = final-emissions
LOGS = code/logs
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
%-emissions:  export EM = $*

# Prints out the selected emissions type
# and calls the recursive Make
%-emissions:
	@echo "Selected gas is " $(EM)
	@$(MAKE) emissions

else

# This target is only visible to the Makefile during the
# recursive call, once EM has been specified, and triggers the
# build of the entire system. This target must be the final
# outputs of the system.
emissions : $(MED_OUT)/D.$(EM)_default_total_EF.csv \
	$(MED_OUT)/D.$(EM)_default_total_emissions.csv

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

all: SO2-emissions BC-emissions

# --------------------------------------------------------------

# Targets used to remove output files for a fresh run
clean-all : \
	clean-intermediate clean-diagnostic clean-final clean-logs clean-io

clean-intermediate :
	rm -fv $(MED_OUT)/*.csv

clean-diagnostic :
	rm -fv $(DIAG_OUT)/*.csv

clean-final :
	rm -fv $(FINAL_OUT)/*

clean-logs :
	rm -fv $(LOGS)/*

clean-io :
	rm -fv $(DOCS)/IO_documentation.csv

clean-modBCD :
	rm -fv $(MED_OUT)/B* \
	rm -fv $(MED_OUT)/C* \
	rm -fv $(MED_OUT)/D*


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
$(MED_OUT)/A.UN_pop_master.csv : \
	$(MOD_A)/A1.1.UN_pop_WB_extension.R \
	$(PARAMS)/common_data.R \
	$(PARAMS)/global_settings.R \
	$(PARAMS)/IO_functions.R \
	$(PARAMS)/data_functions.R \
	$(PARAMS)/analysis_functions.R \
	$(MAPPINGS)/IEA_ctry.csv \
	$(SOCIO_DATA)/UN_pop_raw.xlsx
	Rscript $< $(EM) --nosave --no-restore

# aa1-2
# Initial processing of IEA energy data
$(MED_OUT)/A.IEA_en_stat_ctry_hist.csv : \
	$(MOD_A)/A1.2.IEA_downscale_ctry.R \
	$(MED_OUT)/A.UN_pop_master.csv \
	$(MAPPINGS)/IEA_ctry.csv \
	$(ENERGY_DATA)/OECD_E_stat.csv \
	$(ENERGY_DATA)/NonOECD_E_stat.csv
	Rscript $< $(EM) --nosave --no-restore

# aa2-1
# Converts IEA energy data to CEDS standard format
# Corrects inconsistencies in residential biomass consumption
$(MED_OUT)/A.en_stat_sector_fuel.csv : \
	$(MOD_A)/A2.1.IEA_en_bal.R \
	$(MOD_A)/A2.2.fix_IEA_biomass.R \
	$(MAPPINGS)/IEA_product_fuel.csv \
	$(MED_OUT)/A.UN_pop_master.csv \
	$(MAPPINGS)/Master_Fuel_Sector_List.xlsx \
	$(MED_OUT)/A.IEA_en_stat_ctry_hist.csv \
	$(ENERGY_DATA)/IEA_flow_sector.csv
	Rscript $< $(EM) --nosave --no-restore
	Rscript $(word 2,$^) $(EM) --nosave --no-restore

# aa3-1
# naming note: includes both module A3 and A4
# Extends IEA energy data to include additional years in BP data
# Expands energy data to include all possible id combinations
$(MED_OUT)/A.comb_activity.csv : \
	$(MOD_A)/A3.1.IEA_BP_data_extension.R \
	$(MOD_A)/A4.1.complete_energy_data.R \
	$(MAPPINGS)/IEA_BP_mapping.csv \
	$(MED_OUT)/A.en_stat_sector_fuel.csv \
	$(MAPPINGS)/Master_Fuel_Sector_List.xlsx \
	$(ENERGY_DATA)/BP_energy_data.xlsx
	Rscript $< $(EM) --nosave --no-restore
	Rscript $(word 2,$^) $(EM) --nosave --no-restore

# aa5-1
# BRANCH BLOCK
# Generates the process activity database
$(MED_OUT)/A.NC_activity_db.csv : \
	$(MOD_A)/A5.1.base_NC_activity.R \
	$(MOD_A)/A5.2.add_NC_activity_smelting.R \
	$(MOD_A)/A5.2.add_NC_activity_pulp_paper.R \
	$(MOD_A)/A5.2.add_NC_activity_gdp.R \
	$(PARAMS)/common_data.R \
	$(PARAMS)/global_settings.R \
	$(PARAMS)/IO_functions.R \
	$(PARAMS)/data_functions.R \
	$(PARAMS)/analysis_functions.R \
	$(PARAMS)/timeframe_functions.R \
	$(PARAMS)/process_db_functions.R \
	$(MAPPINGS)/activity_input_mapping.xlsx \
	$(MAPPINGS)/2011_NC_SO2_ctry.csv \
	$(ACTIV)/Smelter-Feedstock-Sulfur.xlsx \
	$(ACTIV)/Wood_Pulp_Consumption.xlsx \
	$(ACTIV)/GDP.xlsx
	Rscript $< $(EM) --nosave --no-restore
	Rscript $(word 2,$^) $(EM) --nosave --no-restore
	Rscript $(word 3,$^) $(EM) --nosave --no-restore
	Rscript $(word 4,$^) $(EM) --nosave --no-restore

# aa5-2a
# Converts activity database into CEDS Standard and combines
# with combustion activity data
$(MED_OUT)/A.total_activity.csv : \
	$(MOD_A)/A5.3.proc_activity.R \
	$(MAPPINGS)/Master_Country_List.csv \
	$(MED_OUT)/A.comb_activity.csv \
	$(MED_OUT)/A.NC_activity_db.csv
	Rscript $< $(EM) --nosave --no-restore

# aa5-2b
$(MED_OUT)/A.NC_activity.csv : \
	$(MED_OUT)/A.total_activity.csv

# bb1-1
# Generates the base file of combustion emissions factors
# by calling a daughter script for the relevant emissions type
$(MED_OUT)/B.$(EM)_comb_EF_db.csv : \
	$(MOD_B)/B1.1.base_comb_EF.R \
	$(MOD_B)/B1.1.base_SO2_comb_EF.R \
	$(MOD_B)/B1.1.base_BC_comb_EF.R \
	$(PARAMS)/timeframe_functions.R \
	$(PARAMS)/process_db_functions.R \
	$(MAPPINGS)/SO2_base_EF.csv \
	$(MED_OUT)/A.comb_activity.csv \
	$(MAPPINGS)/Bond_ctry_mapping.csv \
	$(MAPPINGS)/Bond_fuel_mapping.csv \
	$(MAPPINGS)/Bond_sector_mapping.csv \
	$(INV_DATA)/Bond_BC1-Central_1990.csv \
	$(INV_DATA)/Bond_BC1-Central_1996.csv \
	$(INV_DATA)/Bond_OC1-Central_1990.csv \
	$(INV_DATA)/Bond_OC1-Central_1996.csv \
	$(INV_DATA)/Bond_Fuel-Central_1990.csv \
	$(INV_DATA)/Bond_Fuel-Central_1996.csv
	Rscript $< $(EM) --nosave --no-restore

# cc1-1
# BRANCH BLOCK
# Converts process emissions data into CEDS Standard and
# generates the default process emissions database.
$(MED_OUT)/C.$(EM)_NC_emissions_db.csv : \
	$(MOD_C)/C1.1.base_NC_emissions.R \
	$(MOD_C)/C1.2.add_NC_emissions.R \
	$(MOD_C)/C1.2.add_SO2_NC_emissions_all.R \
	$(PARAMS)/common_data.R \
	$(PARAMS)/global_settings.R \
	$(PARAMS)/IO_functions.R \
	$(PARAMS)/data_functions.R \
	$(PARAMS)/analysis_functions.R \
	$(PARAMS)/timeframe_functions.R \
	$(PARAMS)/process_db_functions.R \
	$(MAPPINGS)/sector_input_mapping.xlsx \
	$(ACTIV)/Process_SO2_Emissions_to_2005.xlsx
	Rscript $< $(EM) --nosave --no-restore
	Rscript $(word 2,$^) $(EM) --nosave --no-restore

# cc1-2
$(MED_OUT)/C.$(EM)_NC_emissions.csv : \
	$(MOD_C)/C1.3.proc_NC_emissions.R \
	$(MED_OUT)/C.$(EM)_NC_emissions_db.csv \
	$(MAPPINGS)/Master_Fuel_Sector_List.xlsx \
	$(MED_OUT)/A.NC_activity.csv
	Rscript $< $(EM) --nosave --no-restore

# cc2-1
$(MED_OUT)/C.$(EM)_NC_EF_db.csv : \
	$(MOD_C)/C2.1.base_NC_EF.R \
	$(MED_OUT)/A.NC_activity.csv \
	$(MED_OUT)/C.$(EM)_NC_emissions.csv
	Rscript $< $(EM) --nosave --no-restore

# dd1-1
# Calculates combustion emissions from activity data and
# emissions factors
$(MED_OUT)/D.$(EM)_default_comb_emissions.csv : \
	$(MOD_D)/D1.1.default_comb_emissions.R \
	$(MED_OUT)/A.comb_activity.csv \
	$(MED_OUT)/B.$(EM)_comb_EF_db.csv
	Rscript $< $(EM) --nosave --no-restore

# dd2-1
# Generates process emissions factors and combines them with
# combustion emissions and emissions factors
$(MED_OUT)/D.$(EM)_default_total_emissions.csv : \
	$(MOD_D)/D2.1.default_total_emissions.R \
	$(MED_OUT)/C.$(EM)_NC_emissions.csv \
	$(MED_OUT)/D.$(EM)_default_comb_emissions.csv
	Rscript $< $(EM) --nosave --no-restore

# dd3-1
$(MED_OUT)/D.$(EM)_default_total_EF.csv : \
	$(MOD_D)/D3.1.default_total_EF.R \
	$(MED_OUT)/B.$(EM)_comb_EF_db.csv \
	$(MED_OUT)/C.$(EM)_NC_EF_db.csv
	Rscript $< $(EM) --nosave --no-restore

# ee1-1a
# Creates scaled emissions and emissions factors for US data
# $(MED_OUT)/E.$(EM)_total_scaled_emissions.csv : \
	# $(MOD_E)/E1.1.US_scaling.R \
	# $(PARAMS)/E.emissions_scaling_function.R.R \
	# $(MAPPINGS)/US_sector_mapping.csv \
	# $(MED_OUT)/C.$(EM)_total_default_emissions.csv \
	# $(MED_OUT)/C.$(EM)_total_default_EF.csv \
	# $(INV_DATA)/national_tier1_caps.xlsx
	# Rscript $< $(EM) --nosave --no-restore

# ee1-1b
# $(MED_OUT)/E.$(EM)_total_scaled_EF.csv : \
            # $(MED_OUT)/E.$(EM)_total_scaled_emissions.csv
