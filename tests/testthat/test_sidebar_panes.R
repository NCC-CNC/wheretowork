context("sidebar panes")

test_that("newSolutionSidebarPane", {
  # create object
  x <-
    newSolutionSidebarPane(
      id = "sidebarid", solutionSettingsId = "ssId", nameId = "textId",
      buttonId = "btnId")
  # run tests
  h <- rvest::read_html(as.character(x))
  ## sidebar
  ## solution settings
  expect_length(rvest::html_elements(h, "#ssId"), 1)
  expect_identical(
    xml2::xml_name(rvest::html_elements(h, "#ssId")),
    "div")
  expect_identical(
    xml2::xml_attr(
      attr = "class",
      x = rvest::html_elements(h, "#ssId")),
    "solutionSettings html-widget html-widget-output")
  ## text input
  expect_length(rvest::html_elements(h, "#textId"), 1)
  expect_identical(
    xml2::xml_name(rvest::html_elements(h, "#textId")),
    "input")
  ## button
  expect_length(rvest::html_elements(h, "#btnId"), 1)
  expect_identical(
    xml2::xml_name(rvest::html_elements(h, "#btnId")),
    "button")
})
