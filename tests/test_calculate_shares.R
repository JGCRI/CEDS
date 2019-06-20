# test_calculate_shares.R
#
# Tests for the function calculate_shares, in data_functions.R
#
# Caleb Braun
# 6/20/19
#
# To run test interactively, use testthat::test_dir('tests') from the root
# level directory.
#
# To run test from command line, run:
#     Rscript -e "testthat::test_dir('tests')"
source("../code/parameters/data_functions.R")

context("Calculate shares")

df <- dplyr::tribble(
     ~iso,  ~fuel,        ~ext_sector, ~X2000, ~X2001, ~X2002,
     "usa", "diesel_oil", "Industry",  5,      6,      3,
     "usa", "diesel_oil", "RCO",       10,     2,      3,
     "usa", "light_oil",  "Transport", 5,      6,      3,
     "usa", "light_oil",  "Energy",    10,     2,      3
)

test_that("calculate_shares keeps data shape", {
    res <- calculate_shares(df, c('iso', 'fuel'), 'ext_sector')
    resdf <- calculate_shares(as.data.frame(df), c('iso', 'fuel'), 'ext_sector')

    expect_identical(dim(df), dim(res))
    expect_identical(class(df), class(res))
    expect_identical('data.frame', class(resdf))
    expect_identical(names(df), names(res))
})

test_that("calculate_shares returns frame equal to input", {
    res <- calculate_shares(df, 'ext_sector', 'iso')
    df_ones <- df
    df_ones[4:6] <- 1
    expect_true(dplyr::all_equal(res, df_ones))
})

test_that("replace_with_zeros works", {
    df_zeros <- df
    df_zeros[df_zeros$fuel == 'light_oil', 4:6] <- 0

    res_zF <- calculate_shares(df_zeros, c('iso', 'fuel'), 'ext_sector', replace_with_zeros = F)
    res_zT <- calculate_shares(df_zeros, c('iso', 'fuel'), 'ext_sector', replace_with_zeros = T)

    expect_true(all(res_zT[res_zT$fuel == 'light_oil', 4:6] == 0))
    expect_true(all(is.na(res_zF[res_zF$fuel == 'light_oil', 4:6])))
})