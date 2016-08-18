context("t_apa")

# Example data from Lakens, D. (2013). Calculating and reporting effect sizes to
# facilitate cumulative science: a practical primer for t-tests and ANOVAs.
# Frontiers in Psychology, 4, 863. doi:10.3389/fpsyg.2013.00863

df <- data.frame(movie_1 = c(9, 7, 8, 9, 8, 9, 9, 10, 9, 9),
                 movie_2 = c(9, 6, 7, 8, 7, 9, 8, 8, 8, 7))

test_that("Output for t_apa between subject", {
  expect_identical(t_apa(t_test(df$movie_1, df$movie_2, var.equal = TRUE),
                     print = FALSE),
               "t(18) = 2.52, p = .022, d = 1.13")
})

test_that("Output for t_apa within subject", {
  expect_identical(t_apa(t_test(df$movie_1, df$movie_2, paired = TRUE),
                     print = FALSE),
               "t(9) = 4.74, p = .001, d = 1.50")
})

test_that("Formal structure of t_apa output)", {
  expect_match(t_apa(t_test(df$movie_1, df$movie_2, var.equal = TRUE),
                     print = FALSE),
               paste0("t\\([[:digit:]]+\\) [=<] [[:digit:]]+\\.[[:digit:]]{2},",
                      " p [=<] \\.[[:digit:]]{3}, d [=<] [[:digit:]]+\\.",
                      "[[:digit:]]{2}"))
  expect_match(t_apa(t_test(df$movie_1, df$movie_2), print = FALSE),
               paste0("t\\([[:digit:]]+\\.[[:digit:]]{2}\\) [=<] ",
                      "[[:digit:]]+\\.[[:digit:]]{2}, p [=<] \\.[[:digit:]]{3}",
                      ", d [=<] [[:digit:]]+\\.[[:digit:]]{2}"))
})

# Output formats ---------------------------------------------------------------

test_that("t_apa: markdown format", {
  expect_identical(t_apa(t_test(df$movie_1, df$movie_2, var.equal = TRUE),
                     format = "markdown", print = FALSE),
               "*t*(18) = 2.52, *p* = .022, *d* = 1.13")
})

test_that("t_apa: rmarkdown format", {
  expect_identical(t_apa(t_test(df$movie_1, df$movie_2, var.equal = TRUE),
                     format = "rmarkdown", print = FALSE),
               "*t*(18) = 2.52, *p* = .022, *d* = 1.13")
})

test_that("t_apa: html format", {
  expect_identical(t_apa(t_test(df$movie_1, df$movie_2, var.equal = TRUE),
                     format = "html", print = FALSE),
               "<i>t</i>(18) = 2.52, <i>p</i> = .022, <i>d</i> = 1.13")
})

test_that("t_apa: latex format", {
  expect_identical(t_apa(t_test(df$movie_1, df$movie_2, var.equal = TRUE),
                     format = "latex", print = FALSE),
               paste0("\\textit{t}(18)~=~2.52, \\textit{p}~=~.022, ",
                      "\\textit{d}~=~1.13"))
})

test_that("t_apa: plotmath format", {
  expect_identical(
    as.character(t_apa(t_test(df$movie_1, df$movie_2, var.equal = TRUE),
                       format = "plotmath", print = FALSE)),
    paste0("paste(italic(\"t\"), \"(18) = 2.52, \", ",
                      "italic(\"p\"), \" = .022, \", ",
                      "italic(\"d\"), \" = 1.13\")")
  )
})

