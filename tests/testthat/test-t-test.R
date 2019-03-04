context("t_test")

test_that("t_test equals to t.test", {

  x <- t.test(1:10, y = c(7:20))[]

  y <- t_test(1:10, y = c(7:20))
  # Remove 'data' entry
  y <- y[!(names(y) == "data")]

  expect_equal(x, y)
})

test_that("t_test returns input data", {
  expect_equal(t_test(1:10, y = c(7:20))[["data"]],
               list(x = 1:10, y = c(7:20)))
})
