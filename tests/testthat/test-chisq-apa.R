context("chisq_apa")

# Example data from Agresti, A. (2007) An Introduction to Categorical Data
# Analysis, 2nd ed., New York: John Wiley & Sons. Page 38.

m <- matrix(c(762, 327, 468, 484, 239, 477), nrow = 2)
dimnames(m) <- list(gender = c("F", "M"),
                    party = c("Democrat","Independent", "Republican"))

test_that("Output for chisq_apa", {
  expect_identical(chisq_apa(chisq.test(m), print = FALSE),
                   "chi^2(2) = 242.30, p < .001")
  expect_identical(chisq_apa(chisq.test(m), print_n = TRUE, print = FALSE),
                   "chi^2(2, n = 2757) = 242.30, p < .001")
})

test_that("Formal structure for chisq_apa output", {
  expect_match(chisq_apa(chisq.test(m), print = FALSE),
               paste0("chi\\^2\\([[:digit:]]+\\) = [[:digit:]]+\\.",
                      "[[:digit:]]{2}, p [=<] \\.[[:digit:]]{3}"))
})

# Output formats ---------------------------------------------------------------

test_that("chisq_apa: markdown format", {
  expect_identical(chisq_apa(chisq.test(m), format = "markdown", print = FALSE),
                   "*chi^2*(2) = 242.30, *p* < .001")
})

test_that("chisq_apa: rmarkdown format", {
  expect_identical(chisq_apa(chisq.test(m), format = "rmarkdown",
                             print = FALSE),
                   "$\\chi^2$(2) = 242.30, *p* < .001")
})

test_that("chisq_apa: html format", {
  expect_identical(chisq_apa(chisq.test(m), format = "html", print = FALSE),
                   "<i>&chi;</i><sup>2</sup>(2) = 242.30, <i>p</i> < .001")
})

test_that("chisq_apa: latex format", {
  expect_identical(chisq_apa(chisq.test(m), format = "latex", print = FALSE),
                   "$chi^2$(2)~=~242.30, \\textit{p}~<~.001")
})

test_that("chisq_apa: plotmath format", {
  expect_identical(
    as.character(chisq_apa(chisq.test(m), format = "plotmath", print = FALSE)),
    "paste(chi^2, \"(2) = 242.30, \", italic(\"p\"), \" < .001\")"
  )
})
