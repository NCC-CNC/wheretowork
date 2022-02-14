context("enc2ascii")

test_that("character vector input", {
  expect_equal(enc2ascii(c("asdf", "dfg")), c("asdf", "dfg"))
  expect_equal(enc2ascii(c("a.sdf", "dfg")), c("a.sdf", "dfg"))
  expect_equal(enc2ascii(c("a.sdf", "pèlerin")), c("a.sdf", "plerin"))
})

test_that("list input", {
  expect_equal(
    enc2ascii(list(c("asdf", "dfg"), 5, c("a.sdf", "pèlerin"))),
    list(c("asdf", "dfg"), 5, c("a.sdf", "plerin"))
  )
})

test_that("nested input", {
  expect_equal(
    enc2ascii(list(c("asdf", "dfg"), list(4, c("a.sdf", "pèlerin")))),
    list(c("asdf", "dfg"), list(4, c("a.sdf", "plerin")))
  )
})

test_that("non-character input", {
  expect_equal(enc2ascii(c(5, 1)), c(5, 1))
})
