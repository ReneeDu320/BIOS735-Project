library(testthat)
load_all('./final')
#library(final)

#test_check("final")


test_file("final/tests/testthat/test-test-logistic.R")

test_package("final")