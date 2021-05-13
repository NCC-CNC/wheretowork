context("simulate legends")

test_that("simulate_continuous_legend", {
  expect_is(simulate_continuous_legend(), "ContinuousLegend")
})

test_that("simulate_categorical_legend", {
  expect_is(simulate_categorical_legend(), "CategoricalLegend")
})
