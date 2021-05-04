# load packages
library(testthat)
library(locationmisc)

# enable parallel testing
Sys.unsetenv("R_TESTS")

# run tests
test_check("locationmisc")
