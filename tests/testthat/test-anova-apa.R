context("anova_apa")

library(dplyr, warn.conflicts = FALSE)
library(magrittr, warn.conflicts = FALSE)

test_that("Formal structure for anova_apa output", {

  library(ez)
  data(ANT)

  data <-
    ANT %>%
    filter(error == 0) %>%
    group_by(subnum, group, cue, flank) %>%
    summarise(rt = mean(rt)) %>%
    as.data.frame # ezANOVA does not support tbl_df

  anova <- anova_apa(
    ezANOVA(data, dv = rt, wid = subnum, within = c(cue, flank),
            between = group, detailed = TRUE),
    print = FALSE
  )

  # Intercept, three main effects, three two-way interactions, one three way
  # interactions
  expect_equal(nrow(anova), 1 + 3 + 3 + 1)
  expect_match(`[.data.frame`(anova, anova$effect == "group", "text"),
               paste0("F\\([[:digit:]]+, [[:digit:]]+\\) = [[:digit:]]+\\.",
                      "[[:digit:]]{2}, p [=<] \\.[[:digit:]]{3}, petasq ",
                      "[=<] \\.[[:digit:]]{2}"))

})

test_that("Output for anova_apa: oneway between ANOVA", {

  # Example data from Field, A., Miles, J. & Field, Z. (2012). Discovering
  # statistics using R. London: Sage Publications. Page 434.
  data <- data.frame(id = factor(1:15),
                     dose = rep(c("placebo", "low dose", "high dose"),
                                each = 5),
                     libido = c(3, 2, 1, 1, 4, 5, 2, 4, 2, 3, 7, 4, 5, 3, 6))

  # Build ANOVA with afex
  anova_afex <- anova_apa(
    afex::aov_ez(id = "id", dv = "libido", data = data, between = "dose"),
    print = FALSE
  )

  expect_identical(`[.data.frame`(anova_afex, anova_afex$effect == "dose",
                                  "text"),
                   "F(2, 12) = 5.12, p = .025, petasq = .46")

  # Build ANOVA with ez
  anova_ez <- anova_apa(
    ez::ezANOVA(data, dv = libido, wid = id, between = dose, detailed = TRUE),
    print = FALSE
  )

  expect_identical(`[.data.frame`(anova_ez, anova_ez$effect == "dose", "text"),
                   "F(2, 12) = 5.12, p = .025, petasq = .46")

})

test_that("Output for anova_apa: factorial between ANOVA", {

  # Example data from Field, A., Miles, J. & Field, Z. (2012). Discovering
  # statistics using R. London: Sage Publications. Page 513f.

  data <- data.frame(
    id = factor(1:48),
    gender = rep(c("female", "male"), each = 24),
    alcohol = rep(c("none", "2 pints", "4 pints"), each = 8, times = 2),
    attractiveness = c(65, 70, 60, 60, 60, 55, 60, 55, 70, 65, 60, 70, 65, 60,
                       60, 50, 55, 65, 70, 55, 55, 60, 50, 50, 50, 55, 80, 65,
                       70, 75, 75, 65, 45, 60, 85, 65, 70, 70, 80, 60, 30, 30,
                       30, 55, 35, 20, 45, 40)
  )

  # Build ANOVA with afex
  anova_afex <- anova_apa(
    afex::aov_ez(id = "id", dv = "attractiveness", data = data,
                 between = c("gender", "alcohol")),
    print = FALSE
  )

  expect_identical(`[.data.frame`(anova_afex, anova_afex$effect == "gender",
                                  "text"),
                   "F(1, 42) = 2.03, p = .161, petasq = .05")

  expect_identical(`[.data.frame`(anova_afex, anova_afex$effect == "alcohol",
                                  "text"),
                   "F(2, 42) = 20.07, p < .001, petasq = .49")

  expect_identical(`[.data.frame`(anova_afex,
                                  anova_afex$effect == "gender:alcohol",
                                  "text"),
                   "F(2, 42) = 11.91, p < .001, petasq = .36")

  # Build ANOVA with ez
  anova_ez <- anova_apa(
    ez::ezANOVA(data, dv = attractiveness, wid = id,
                between = c(gender, alcohol), detailed = TRUE, type = 3),
    print = FALSE
  )

  expect_identical(`[.data.frame`(anova_ez, anova_ez$effect == "gender",
                                  "text"),
                   "F(1, 42) = 2.03, p = .161, petasq = .05")

  expect_identical(`[.data.frame`(anova_ez, anova_ez$effect == "alcohol",
                                  "text"),
                   "F(2, 42) = 20.07, p < .001, petasq = .49")

  expect_identical(`[.data.frame`(anova_ez, anova_ez$effect == "gender:alcohol",
                                  "text"),
                   "F(2, 42) = 11.91, p < .001, petasq = .36")

})

test_that("Output for anova_apa: repeated-measures ANOVA", {

  # Example data from Field, A., Miles, J. & Field, Z. (2012). Discovering
  # statistics using R. London: Sage Publications. Page 513f.

  data <- data.frame(
    id = factor(rep(1:8, each = 4)),
    animal = rep(c("stick insect", "kangaroo testicle", "fish eye",
                   "witchetty grub"), times = 8),
    retch = c(8, 7, 1, 6, 9, 5, 2, 5, 6, 2, 3, 8, 5, 3, 1, 9, 8, 4, 5, 8, 7, 5,
              6, 7, 10, 2, 7, 2, 12, 6, 8, 1)
  )

  # Build ANOVA with afex
  anova_afex <- anova_apa(
    afex::aov_ez(id = "id", dv = "retch", data = data, within = "animal"),
    print = FALSE
  )

  expect_identical(`[.data.frame`(anova_afex, anova_afex$effect == "animal",
                                  "text"),
                   "F(1.60, 11.19) = 3.79, p = .063, petasq = .35")

  # Build ANOVA with ez
  anova_ez <- anova_apa(
    ez::ezANOVA(data, dv = retch, wid = id, within = animal, detailed = TRUE),
    print = FALSE
  )

  expect_identical(`[.data.frame`(anova_ez, anova_ez$effect == "animal",
                                  "text"),
                   "F(1.60, 11.19) = 3.79, p = .063, petasq = .35")

})

test_that("Output for anova_apa: factorial repeated-measures ANOVA", {

  # Example data from Field, A., Miles, J. & Field, Z. (2012). Discovering
  # statistics using R. London: Sage Publications. Page 583.

  data <- data.frame(
    id = factor(rep(1:20, each = 9)),
    gender = rep(c("male", "female"), each = 10 * 9),
    imagery = rep(c("positive", "negative", "neutral"), times = 60),
    drink = rep(c("beer", "wine", "water"), each = 3, times = 20),
    attitude = c(1, 6, 5, 38, -5, 4, 10, -14, -2, 43, 30, 8, 20, -12, 4, 9, -10,
                 -13, 15, 15, 12, 20, -15, 6, 6, -16, 1, 40, 30, 19, 28, -4, 0,
                 20, -10, 2, 8, 12, 8, 11, -2, 6, 27, 5, -5, 17, 17, 15, 17, -6,
                 6, 9, -6, -13, 30, 21, 21, 15, -2, 16, 19, -20, 3, 34, 23, 28,
                 27, -7, 7, 12, -12, 2, 34, 20, 26, 24, -10, 12, 12, -9, 4, 26,
                 27, 27, 23, -15, 14, 21, -6, 0, 1, -19, -10, 28, -13, 13, 33,
                 -2, 9, 7, -18, 6, 26, -16, 19, 23, -17, 5, 22, -8, 4, 34, -23,
                 14, 21, -19, 0, 30, -6, 3, 32, -22, 21, 17, -11, 4, 40, -6, 0,
                 24, -9, 19, 15, -10, 2, 15, -9, 4, 29, -18, 7, 13, -17, 8, 20,
                 -17, 9, 30, -17, 12, 16, -4, 10, 9, -12, -5, 24, -15, 18, 17,
                 -4, 8, 14, -11, 7, 34, -14, 20, 19, -1, 12, 15, -6, 13, 23,
                 -15, 15, 29, -1, 10)
  )

  # Build ANOVA with afex
  anova_afex <- anova_apa(
    afex::aov_ez(id = "id", dv = "attitude", data = data,
                 within = c("drink", "imagery")),
    print = FALSE
  )

  expect_identical(`[.data.frame`(anova_afex, anova_afex$effect == "drink",
                                  "text"),
                   "F(1.15, 21.93) = 5.11, p = .030, petasq = .21")

  expect_identical(`[.data.frame`(anova_afex, anova_afex$effect == "imagery",
                                  "text"),
                   "F(1.49, 28.40) = 122.56, p < .001, petasq = .87")

  expect_identical(`[.data.frame`(anova_afex,
                                  anova_afex$effect == "drink:imagery",
                                  "text"),
                   "F(4, 76) = 17.15, p < .001, petasq = .47")

  # Build ANOVA with ez
  anova_ez <- anova_apa(
    ez::ezANOVA(data, dv = attitude, wid = id, within = c(drink, imagery),
                type = 3, detailed = TRUE),
    print = FALSE
  )

  expect_identical(`[.data.frame`(anova_ez, anova_ez$effect == "drink", "text"),
                   "F(1.15, 21.93) = 5.11, p = .030, petasq = .21")

  expect_identical(`[.data.frame`(anova_ez, anova_ez$effect == "imagery",
                                  "text"),
                   "F(1.49, 28.40) = 122.56, p < .001, petasq = .87")

  expect_identical(`[.data.frame`(anova_ez, anova_ez$effect == "drink:imagery",
                                  "text"),
                   "F(4, 76) = 17.15, p < .001, petasq = .47")

})

test_that("Output for anova_apa: mixed ANOVA", {

  # Example data from Field, A., Miles, J. & Field, Z. (2012). Discovering
  # statistics using R. London: Sage Publications. Page 607.

  data <- data.frame(
    id = factor(rep(1:20, each = 9)),
    gender = rep(c("male", "female"), each = 10 * 9),
    looks = rep(c("attractive", "average", "ugly"), times = 60),
    personality = rep(c("high carisma", "some charisma", "dullard"), each = 3,
                      times = 20),
    rating = c(86, 84, 67, 88, 69, 50, 97, 48, 47, 91, 83, 53, 83, 74, 48, 86,
               50, 46, 89, 88, 48, 99, 70, 48, 90, 45, 48, 89, 69, 58, 86, 77,
               40, 87, 47, 53, 80, 81, 57, 88, 71, 50, 82, 50, 45, 80, 84, 51,
               96, 63, 42, 92, 48, 43, 89, 85, 61, 87, 79, 44, 86, 50, 45, 100,
               94, 56, 86, 71, 54, 84, 54, 47, 90, 74, 54, 92, 71, 58, 78, 38,
               45, 89, 86, 63, 80, 73, 49, 91, 48, 39, 89, 91, 93, 88, 65, 54,
               55, 48, 52, 84, 90, 85, 95, 70, 60, 50, 44, 45, 99, 100, 89, 80,
               79, 53, 51, 48, 44, 86, 89, 83, 86, 74, 58, 52, 48, 47, 89, 87,
               80, 83, 74, 43, 58, 50, 48, 80, 81, 79, 86, 59, 47, 51, 47, 40,
               82, 92, 85, 81, 66, 47, 50, 45, 47, 97, 69, 87, 95, 72, 51, 45,
               48, 46, 95, 92, 90, 98, 64, 53, 54, 53, 45, 95, 93, 96, 79, 66,
               46, 52, 39, 47)
  )

  # Build ANOVA with afex
  anova_afex <- anova_apa(
    afex::aov_ez(id = "id", dv = "rating", data = data,
                 between = "gender",
                 within = c("looks", "personality")),
    print = FALSE
  )

  expect_identical(`[.data.frame`(anova_afex, anova_afex$effect == "gender",
                                  "text"),
                   "F(1, 18) = 0.00, p = .946, petasq < .01")

  expect_identical(`[.data.frame`(anova_afex, anova_afex$effect == "looks",
                                  "text"),
                   "F(2, 36) = 423.73, p < .001, petasq = .96")

  expect_identical(`[.data.frame`(anova_afex,
                                  anova_afex$effect == "personality", "text"),
                   "F(2, 36) = 328.25, p < .001, petasq = .95")

  expect_identical(`[.data.frame`(anova_afex,
                                  anova_afex$effect == "gender:looks", "text"),
                   "F(2, 36) = 80.43, p < .001, petasq = .82")

  expect_identical(`[.data.frame`(anova_afex,
                                  anova_afex$effect == "gender:personality",
                                  "text"),
                   "F(2, 36) = 62.45, p < .001, petasq = .78")

  expect_identical(`[.data.frame`(anova_afex,
                                  anova_afex$effect == "looks:personality",
                                  "text"),
                   "F(4, 72) = 36.63, p < .001, petasq = .67")

  # Build ANOVA with ez
  anova_ez <- anova_apa(
    ez::ezANOVA(data, dv = rating, wid = id, between = gender,
                within = c(looks, personality), type = 3, detailed = TRUE),
    print = FALSE
  )

  expect_identical(`[.data.frame`(anova_ez, anova_ez$effect == "gender",
                                  "text"),
                   "F(1, 18) = 0.00, p = .946, petasq < .01")

  expect_identical(`[.data.frame`(anova_ez, anova_ez$effect == "looks", "text"),
                   "F(2, 36) = 423.73, p < .001, petasq = .96")

  expect_identical(`[.data.frame`(anova_ez,
                                  anova_ez$effect == "personality", "text"),
                   "F(2, 36) = 328.25, p < .001, petasq = .95")

  expect_identical(`[.data.frame`(anova_ez,
                                  anova_ez$effect == "gender:looks", "text"),
                   "F(2, 36) = 80.43, p < .001, petasq = .82")

  expect_identical(`[.data.frame`(anova_ez,
                                  anova_ez$effect == "gender:personality",
                                  "text"),
                   "F(2, 36) = 62.45, p < .001, petasq = .78")

  expect_identical(`[.data.frame`(anova_ez,
                                  anova_ez$effect == "looks:personality",
                                  "text"),
                   "F(4, 72) = 36.63, p < .001, petasq = .67")
})

# Output formats ---------------------------------------------------------------

# Example data from Field, A., Miles, J. & Field, Z. (2012). Discovering
# statistics using R. London: Sage Publications. Page 434.
data <- data.frame(id = factor(1:15),
                   dose = rep(c("placebo", "low dose", "high dose"),
                              each = 5),
                   libido = c(3, 2, 1, 1, 4, 5, 2, 4, 2, 3, 7, 4, 5, 3, 6))

anova_afex <- suppressMessages(
  afex::aov_ez(id = "id", dv = "libido", data = data, between = "dose")
)
anova_ez <- ez::ezANOVA(data, dv = libido, wid = id, between = dose,
                        detailed = TRUE)

test_that("anova_apa: markdown", {

  expect_identical(anova_apa(anova_afex, effect = "dose", print = FALSE,
                             format = "markdown"),
                   "*F*(2, 12) = 5.12, *p* = .025, *petasq* = .46")

  expect_identical(anova_apa(anova_afex, effect = "dose", print = FALSE,
                             format = "markdown"),
                   "*F*(2, 12) = 5.12, *p* = .025, *petasq* = .46")

})

test_that("anova_apa: rmarkdown", {

  expect_identical(anova_apa(anova_afex, effect = "dose", print = FALSE,
                             format = "rmarkdown"),
                   "*F*(2, 12) = 5.12, *p* = .025, $\\eta^2_p$ = .46")

  expect_identical(anova_apa(anova_ez, effect = "dose", print = FALSE,
                             format = "rmarkdown"),
                   "*F*(2, 12) = 5.12, *p* = .025, $\\eta^2_p$ = .46")

})

test_that("anova_apa: html", {

  expect_identical(anova_apa(anova_afex, effect = "dose", print = FALSE,
                             format = "html"),
                   paste0("<i>F</i>(2, 12) = 5.12, <i>p</i> = .025, ",
                          "<i>&eta;<sup>2</sup><sub>p</sub></i> = .46"))

  expect_identical(anova_apa(anova_afex, effect = "dose", print = FALSE,
                             format = "html"),
                   paste0("<i>F</i>(2, 12) = 5.12, <i>p</i> = .025, ",
                          "<i>&eta;<sup>2</sup><sub>p</sub></i> = .46"))

})

test_that("anova_apa: latex", {

  expect_identical(anova_apa(anova_afex, effect = "dose", print = FALSE,
                             format = "latex"),
                   paste0("\\textit{F}(2,~12)~=~5.12, \\textit{p}~=~.025, ",
                          "$\\eta^2_p$~=~.46"))

  expect_identical(anova_apa(anova_ez, effect = "dose", print = FALSE,
                             format = "latex"),
                   paste0("\\textit{F}(2,~12)~=~5.12, \\textit{p}~=~.025, ",
                          "$\\eta^2_p$~=~.46"))

})

test_that("anova_apa: plotmath", {

  expect_identical(as.character(anova_apa(anova_afex, effect = "dose",
                                          print = FALSE, format = "plotmath")),
                   paste0("paste(italic(\"F\"), \"(2, 12) = 5.12, \", ",
                          "italic(\"p\"), \" = .025, \", ",
                          "eta[p]^2, \" = .46\")"))

  expect_identical(as.character(anova_apa(anova_ez, effect = "dose",
                                          print = FALSE, format = "plotmath")),
                   paste0("paste(italic(\"F\"), \"(2, 12) = 5.12, \", ",
                          "italic(\"p\"), \" = .025, \", ",
                          "eta[p]^2, \" = .46\")"))

})
