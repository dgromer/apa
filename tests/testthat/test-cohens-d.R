context("cohens_d")

# Example data from Lakens, D. (2013). Calculating and reporting effect sizes to
# facilitate cumulative science: a practical primer for t-tests and ANOVAs.
# Frontiers in Psychology, 4, 863. doi:10.3389/fpsyg.2013.00863

df <- data.frame(movie_1 = c(9, 7, 8, 9, 8, 9, 9, 10, 9, 9),
                 movie_2 = c(9, 6, 7, 8, 7, 9, 8, 8, 8, 7))

df_long <- data.frame(movie = rep(names(df), each = 10),
                      rating = c(df$movie_1, df$movie_2))

test_that("Between group cohen's d", {
  expect_equal(round(cohens_d(df$movie_1, df$movie_2), 2), 1.13)
  expect_equal(round(cohens_d(df_long, dv = "rating", iv = "movie"), 2), 1.13)
  expect_equal(round(cohens_d(rating ~ movie, df_long), 2), 1.13)
  expect_equal(round(cohens_d(t_test(rating ~ movie, df_long)), 2), 1.13)
  expect_equal(round(cohens_d(t.test(rating ~ movie, df_long,
                                     var.equal = TRUE)), 2), 1.13)
  expect_equal(round(cohens_d_(m1 = 8.7, m2 = 7.7, sd1 = .82, sd2 = .95,
                              n1 = 10, n2 = 10), 2), 1.13)
  expect_equal(round(cohens_d_(t = 2.52, n1 = 10, n2 = 10), 2), 1.13)
  expect_equal(round(cohens_d_(t = 2.52, n = 20), 2), 1.13)
})

test_that("Between group cohen's d, hedges correction", {
  expect_equal(round(cohens_d(df$movie_1, df$movie_2, corr = "hedges_g"), 2),
               1.08)
})

test_that("Within group cohen's d", {
  expect_equal(round(cohens_d(df$movie_1, df$movie_2, paired = TRUE), 2), 1.5)
})
