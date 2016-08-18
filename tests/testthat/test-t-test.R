context("t_test")

test_that("t_test equals to t.test", {
  expect_equal(t.test(1:10, y = c(7:20))[], t_test(1:10, y = c(7:20))[-10])
})

test_that("t_test returns input data", {
  expect_equal(t_test(1:10, y = c(7:20))[["data"]],
               list(x = 1:10, y = c(7:20)))
})
