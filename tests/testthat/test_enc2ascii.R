context("enc2ascii")

test_that("character vector input", {
  expect_equal(enc2ascii(c("asdf", "dfg")), c("asdf", "dfg"))
  expect_equal(enc2ascii(c("a.sdf", "dfg")), c("a.sdf", "dfg"))
  expect_equal(enc2ascii(c("a.sdf", "pèlerin")), c("a.sdf", "pelerin"))
})

test_that("list input", {
  expect_equal(
    enc2ascii(list(c("asdf", "dfg"), 5, c("a.sdf", "pèlerin"))),
    list(c("asdf", "dfg"), 5, c("a.sdf", "pelerin"))
  )
})

test_that("nested input", {
  expect_equal(
    enc2ascii(list(c("asdf", "dfg"), list(4, c("a.sdf", "pèlerin")))),
    list(c("asdf", "dfg"), list(4, c("a.sdf", "pelerin")))
  )
})

test_that("special characters are transliterated", {
  expect_equal(
    enc2ascii("àâçéèêëîïôùûüÿæœÀÂÇÉÈÊËÎÏÔÙÛÜŸÆŒ"),
    "aaceeeeiiouuuyaeoeAACEEEEIIOUUUYAEOE"
  )
})

test_that("Indigenous language characters are transliterated", {
  expect_equal(enc2ascii("Nêhiyawêwin"), "Nehiyawewin")
  expect_equal(enc2ascii("whùts'ì"), "whuts'i")
})

test_that("non-character input", {
  expect_equal(enc2ascii(c(5, 1)), c(5, 1))
})
