# load packages
library(testthat)
library(wheretowork)

# enable parallel testing
Sys.unsetenv("R_TESTS")

# run tests
test_check("wheretowork")
