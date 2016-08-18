context("cor_apa")

# Example data from Hollander, M. & Wolfe, D. A. (1973). Nonparametric
# Statistical Methods. New York: John Wiley & Sons. Pages 185–194.

x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)

test_that("Output for cor_apa", {
  # Pearson's r
  expect_identical(
    cor_apa(cor.test(x, y, alternative = "greater"), print = FALSE),
    "r(7) = .57, p = .054"
  )
  # Kendall's tau
  expect_identical(
    cor_apa(cor.test(x, y, method = "kendall", alternative = "greater"),
            print = FALSE),
     "r_tau = .44, p = .060"
  )
  # Spearman's rho
  expect_identical(
    cor_apa(cor.test(x, y, method = "spearman", alternative = "greater"),
            print = FALSE),
    "r_s = .60, p = .048"
  )
})

test_that("Formal structure of cor_apa output", {
  expect_match(
    cor_apa(cor.test(x, y), print = FALSE),
    "r\\([[:digit:]]+\\) [=<] \\.[[:digit:]]{2}, p [=<] \\.[[:digit:]]{3}"
  )
})

# Output formats ---------------------------------------------------------------

test_that("cor_apa: markdown format", {
  expect_identical(
    cor_apa(cor.test(x, y, alternative = "greater"), print = FALSE,
            format = "markdown"),
    "*r*(7) = .57, *p* = .054"
  )
  expect_identical(
    cor_apa(cor.test(x, y, method = "kendall", alternative = "greater"),
            print = FALSE, format = "markdown"),
    "*r_tau* = .44, *p* = .060"
  )
  expect_identical(
    cor_apa(cor.test(x, y, method = "spearman", alternative = "greater"),
            print = FALSE, format = "markdown"),
    "*r_s* = .60, *p* = .048"
  )
})

test_that("cor_apa: rmarkdown format", {
  expect_identical(
    cor_apa(cor.test(x, y, alternative = "greater"), print = FALSE,
            format = "rmarkdown"),
    "*r*(7) = .57, *p* = .054"
  )
  expect_identical(
    cor_apa(cor.test(x, y, method = "kendall", alternative = "greater"),
            print = FALSE, format = "rmarkdown"),
    "$r_\\tau$ = .44, *p* = .060"
  )
  expect_identical(
    cor_apa(cor.test(x, y, method = "spearman", alternative = "greater"),
            print = FALSE, format = "rmarkdown"),
    "$r_s$ = .60, *p* = .048"
  )
})

test_that("cor_apa: html format", {
  expect_identical(
    cor_apa(cor.test(x, y, alternative = "greater"), print = FALSE,
            format = "html"),
    "<i>r</i>(7) = .57, <i>p</i> = .054"
  )
  expect_identical(
    cor_apa(cor.test(x, y, method = "kendall", alternative = "greater"),
            print = FALSE, format = "html"),
    "<i>r<sub>&tau;</sub></i> = .44, <i>p</i> = .060"
  )
  expect_identical(
    cor_apa(cor.test(x, y, method = "spearman", alternative = "greater"),
            print = FALSE, format = "html"),
    "<i>r<sub>s</sub></i> = .60, <i>p</i> = .048"
  )
})

test_that("cor_apa: latex format", {
  expect_identical(
    cor_apa(cor.test(x, y, alternative = "greater"), print = FALSE,
            format = "latex"),
    "\\textit{r}(7)~=~.57, \\textit{p}~=~.054"
  )
  expect_identical(
    cor_apa(cor.test(x, y, method = "kendall", alternative = "greater"),
            print = FALSE, format = "latex"),
    "$r_\\tau$~=~.44, \\textit{p}~=~.060"
  )
  expect_identical(
    cor_apa(cor.test(x, y, method = "spearman", alternative = "greater"),
            print = FALSE, format = "latex"),
    "$r_s$~=~.60, \\textit{p}~=~.048"
  )
})

test_that("cor_apa: plotmath format", {
  expect_identical(
    as.character(cor_apa(cor.test(x, y, alternative = "greater"), print = FALSE,
                         format = "plotmath")),
    "paste(italic(\"r\"), \"(7)\", , \" = .57, \", italic(\"p\"), \" = .054\")"
  )
  expect_identical(
    as.character(cor_apa(cor.test(x, y, method = "kendall",
                                  alternative = "greater"),
                         print = FALSE, format = "plotmath")),
    "paste(italic(r)[tau], \" = .44, \", italic(\"p\"), \" = .060\")"
  )
  expect_identical(
    as.character(cor_apa(cor.test(x, y, method = "spearman",
                                  alternative = "greater"),
                         print = FALSE, format = "plotmath")),
    "paste(italic(r)[s], \" = .60, \", italic(\"p\"), \" = .048\")"
  )
})
