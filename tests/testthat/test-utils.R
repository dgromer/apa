context("utils")

test_that("Formatting of statistics", {
  expect_match(fmt_stat(12.345), "= 12\\.35")
  expect_match(fmt_stat(-12.345), "= -12\\.35")
  expect_match(fmt_stat(12.345, equal_sign = FALSE), "12\\.35")
  expect_match(fmt_stat(.004), "0\\.00")
  expect_match(fmt_stat(.004, negative_values = FALSE), "< 0\\.01")
  expect_match(fmt_stat(.004, leading_zero = FALSE, negative_values = FALSE),
               "< \\.01")
  expect_match(fmt_stat(-.93, equal_sign = FALSE, leading_zero = FALSE),
               "-\\.93")
})

test_that("Formatting of p-values", {
  expect_match(fmt_pval(0.12345), "^= \\.123$")
  expect_match(fmt_pval(0.12345, equal_sign = FALSE), "^\\.123$")
  expect_match(fmt_pval(0.00012), "^< \\.001$")
  expect_match(fmt_pval(1), "^> \\.999$")
})

test_that("Formatting significance as symbols", {
  expect_match(p_to_symbol(.5), "")
  expect_match(p_to_symbol(.1), "")
  expect_match(p_to_symbol(.09), "\\.")
  expect_match(p_to_symbol(.05), "\\.")
  expect_match(p_to_symbol(.049), "\\*")
  expect_match(p_to_symbol(.01), "\\*")
  expect_match(p_to_symbol(.009), "\\*\\*")
  expect_match(p_to_symbol(.001), "\\*\\*")
  expect_match(p_to_symbol(.0009), "\\*\\*\\*")
})

test_that("Formatting of effect sizes", {
  expect_match(fmt_es(1.234), "= 1.23")
  expect_match(fmt_es(1.234, equal_sign = FALSE), "1.23")
  expect_match(fmt_es(0.234), "= 0.23")
  expect_match(fmt_es(0.234, leading_zero = FALSE), "= .23")
  expect_match(fmt_es(0.00234, leading_zero = FALSE), "< .01")
})
