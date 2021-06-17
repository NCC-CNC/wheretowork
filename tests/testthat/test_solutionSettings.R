context("solutionSettings")

test_that("single theme status button", {
  # run app
  app <- ShinyDriver$new("apps/solutionSettings")
  # click status button for single theme
  app$findElement(".single-theme-setting .status-checkbox")
  # verify that widget sends correct R message
  app$takeScreenshot("test.png", "main-body")


})

  # run test
  expect_equal(, 2)
})


testServer(
  server, {

  }
})
