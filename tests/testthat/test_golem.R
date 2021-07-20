context("golem tests")

library(golem)

test_that("app ui", {
  ui <- app_ui()
  expect_shinytaglist(ui)
})

test_that("app server", {
  server <- app_server
  expect_is(server, "function")
})

test_that("app global", {
  global <- app_global
  expect_is(global, "{")
})

# Configure this test to fit your need
test_that(
  "app launches",
  {
    skip_on_cran()
    skip_on_travis()
    skip_on_appveyor()
    skip_if_not(interactive())
    x <- processx::process$new(
      "R",
      c(
        "-e",
        "pkgload::load_all(here::here());run_app()"
      )
    )
    Sys.sleep(5)
    expect_true(x$is_alive())
    x$kill()
  }
)
