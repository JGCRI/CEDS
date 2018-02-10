#------------------------------------------------------------------------------
# Program Name: add_user_data_tests.R
# Author: Caleb Braun
# Date Last Updated: 18 December 2017
# Program Purpose: Rigorously test part 2 of the energy extension
# Input Files: H.TEST_total_activity_extended_db.csv
# Output Files: None
# Functions Defined:
# Notes: You must add a custom test file for the default data
# -----------------------------------------------------------------------------

# Total tests to cover:
#
# All 16 possibilites for aggregation level:
#   - 4 should fail
#   - 6 should pass
#   - 4 should be identical to already passed ones
#
# For each aggregation level:
#   - Priority 1 should beat priority 2 should beat no priority
#   - override_normalization works
#   - start_continuity
#   - end_continuity


# Tests if the aggregation level is the one specified by the name of the file
testlvl <- function(fname, expected_lvl) {
    df <- openxlsx::read.xlsx(fname, sheet = "Trend_instructions")
    lvl <- identifyLevel(df)
    if (lvl != expected_lvl)
        warning(paste("Level mismatch! Expected", expected_lvl, "but got", lvl))
}

# Tests if a normal run matches the normal expected output
testrun <- function(files, tnum) {
    runscript()

    result <- readData( "MED_OUT", paste0("H.TEST-total-activity-TEST"), meta = F)
    expected <- read.csv(paste0('tests/', files[1] ), stringsAsFactors = F)

    if (!isTRUE(all.equal(result, expected))) {
        warning(paste("Test", tnum, "failed!"))
    } else {
        message(paste("Test", tnum, "passed."))
    }
}

# Tests if an instruction with higher priority overrides one with lower
testpriority <- function(iname) {
    # Create duplicate instructions with a higher priority that refers to a
    # dataset that is double the original.
    dname <- gsub('-instructions.xlsx', '.csv', iname) # data file name
    mname <- gsub('-instructions', '-mapping', iname) # mapping file name
    new_iname <- 'extension/user-defined-energy/PRIORITY-instructions.xlsx'
    new_mname <- 'extension/user-defined-energy/PRIORITY-mapping.xlsx'
    new_dname <- 'extension/user-defined-energy/PRIORITY.csv'

    orig_inst <- openxlsx::loadWorkbook(iname)
    orig_trnd <- openxlsx::readWorkbook(iname, sheet = "Trend_instructions")
    orig_data <- read.csv(dname, stringsAsFactors = F)

    new_trnd <- dplyr::mutate(orig_trnd, priority = priority - 1)
    new_data <- dplyr::mutate_if(orig_data, is.numeric, prod, 2)

    openxlsx::writeData(orig_inst, "Trend_instructions", new_trnd)
    openxlsx::saveWorkbook(orig_inst, new_iname)
    write.csv(new_data, new_dname)
    file.copy(from = mname, to = new_mname)

    # Run script with new files, which should be the only ones used
    runscript()

    al <- if(agg_level %in% c(1, 5, 6)) "agg_fuel" else "CEDS_fuel"
    result <- readData( "MED_OUT", paste0("H.TEST-total-activity-TEST"), meta = F)
    ename <- gsub('-instructions.xlsx', '-expected.csv', iname) # expected file
    expected <- read.csv(ename, stringsAsFactors = F)
    new_rows <- expected[[al]] %in% openxlsx::read.xlsx(mname, sheet=al)
    Xyears <- grep('X\\d{4}', names(new_data), value = T)
    expected[new_rows , Xyears] <- expected[new_rows , Xyears] * 2

    if (!isTRUE(all.equal(result[new_rows, Xyears], expected[new_rows, Xyears]))) {
        warning("PRIORITY TEST FAILED")
    }

    # Remove priority test files
    file.remove(c(new_iname, new_mname, new_dname))
}

# Calls add_user_defined_data.R with a clean environment
runscript <- function() {
    # Clear global environment for a clean test
    rm(list = ls())

    # Manipulate command line arguments for our test input file
    commandArgsOrig <- commandArgs
    assign("commandArgs", function(bool) { return(c(em = 'TEST')) }, envir = .GlobalEnv )

    debugSource('../code/module-H/add_user-defined_data.R')

    # Reset command line args function
    commandArgs <<- commandArgsOrig
}

runtests <- function(limit = NULL) {
    tfiles <- grep('TEST_lvl\\d.csv', dir('tests'), value = T)
    ntests <- max(as.integer(gsub("[^0-9]", "", tfiles)))
    ntests <- if(is.null(limit)) 1:ntests else limit

    for (tnum in ntests) {
        # Move the files to the folder where they will get run
        files <- grep(paste0('^(?!~\\$)TEST_lvl', tnum), dir('tests'), perl = T, value = T)
        fpaths <- paste0('tests/', files)

        if (length(files) < 4) {
            warning(paste("Not all files present for test level", tnum))
            next
        }
        file.copy(from=fpaths, to='extension/user-defined-energy/')

        instr_file <- fpaths[grep(paste0(tnum, '-instr'), fpaths)]

        testrun(files, tnum)
        # testlvl(instr_file, tnum)
        # testpriority(instr_file)
        # testcontinuity()
        # testnormalization()
        # testbreakdowns()

        # Remove the test files after running
        file.remove(paste0('extension/user-defined-energy/', files))
    }
}

if(rev(strsplit(getwd(), '/')[[1]])[1] != 'input') stop("Go to input directory")
initialize("add_user_data_tests.R", "", NULL)
runtests(1)

for (i in 1:GCAM_SOURCE_RD) logStop()