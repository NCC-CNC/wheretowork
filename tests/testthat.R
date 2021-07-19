# load packages
library(testthat)
library(wheretowork)

# enable parallel testing
Sys.unsetenv("R_TESTS")

# Set RandomFields options
RandomFields::RFoptions(cPrintlevel = 0)

# run tests
test_check("wheretowork")
