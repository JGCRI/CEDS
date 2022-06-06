## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(readr)

## ---- eval = FALSE------------------------------------------------------------
#  read_csv("path/to/your/file", ..., guess_max = Inf)

## -----------------------------------------------------------------------------
tricky_dat <- tibble::tibble(
  x = rep(c("", "2"), c(1000, 1)),
  y = "y"
)
tfile <- tempfile("tricky-column-type-guessing-", fileext = ".csv")
write_csv(tricky_dat, tfile)

## -----------------------------------------------------------------------------
tail(read_csv(tfile))

## -----------------------------------------------------------------------------
with_edition(
  1,
  tail(read_csv(tfile))
)

## -----------------------------------------------------------------------------
with_edition(
  1,
  tail(read_csv(tfile, guess_max = Inf))
)

## -----------------------------------------------------------------------------
with_edition(
  1,
  tail(read_csv(tfile, guess_max = 1200))
)

## -----------------------------------------------------------------------------
dat_chr <- with_edition(
  1,
  read_csv(tfile, col_types = cols(.default = col_character()))
)
tail(dat_chr)

dat <- type_convert(dat_chr)
tail(dat)

## -----------------------------------------------------------------------------
file.remove(tfile)

