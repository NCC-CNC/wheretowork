# load packages
library(testthat)
library(wheretowork)

# enable parallel testing
Sys.unsetenv("R_TESTS")

# Set RandomFields options


assertthat::assert_that(
  requireNamespace("RandomFields"), 
  msg = "please install the \"RandomFields\" package"
)

RandomFields::RFoptions(cPrintlevel = 0)

# run tests
test_check("wheretowork")
