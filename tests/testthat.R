# load packages
library(testthat)
library(locationmisc)

# enable parallel testing
Sys.unsetenv("R_TESTS")

# Set RandomFields options
RandomFields::RFoptions(cPrintlevel = 0)

# run tests
test_check("locationmisc")
