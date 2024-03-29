context("simulate legends")

test_that("simulate_continuous_legend", {
  expect_is(simulate_continuous_legend(), "ContinuousLegend")
})

test_that("simulate_categorical_legend", {
  expect_is(simulate_categorical_legend(), "CategoricalLegend")
})

test_that("simulate_solution_legend", {
  expect_is(simulate_solution_legend(), "ManualLegend")
})

test_that("simulate_include_legend", {
  expect_is(simulate_include_legend(), "ManualLegend")
})
