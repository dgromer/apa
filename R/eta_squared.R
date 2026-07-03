#' Partial Eta Squared
#'
#' @param x A call to \code{aov}, \code{ez::ezANOVA} or \code{afex::aov_ez} or
#'   \code{afex::aov_car} or \code{afex::aov_4}
#' @param effect Character string indicating the name of the effect for which
#'   the partial eta squared should be returned.
#' @export
petasq <- function(x, effect)
{
  # Use a pseudo-S3 method dispatch here, because `ezANOVA` returns a list
  # without a particular class

  # aov
  if (inherits(x, "aov"))
  {
    petasq_aov(x, effect)
  }
  # aovlist
  else if (inherits(x, "aovlist"))
  {
    petasq_aovlist(x, effect)
  }
  # afex
  else if (inherits(x, "afex_aov"))
  {
    petasq_afex(x, effect)
  }
  # ez::ezANOVA
  else if (is.list(x) && names(x)[1] == "ANOVA")
  {
    petasq_ezanova(x, effect)
  }
  else
  {
    stop("Unknown object passed to argument 'x'")
  }
}

#' @importFrom stringr str_trim
petasq_aov <- function(x, effect)
{
  x <- summary(x, intercept = TRUE)[[1]]

  row.names(x) <- str_trim(row.names(x))

  if (!effect %in% row.names(x))
  {
    stop("Specified effect not found")
  }

  petasq_(x[effect, "Sum Sq"], x["Residuals", "Sum Sq"])
}

#' @importFrom purrr flatten
#' @importFrom stringr str_trim
petasq_aovlist <- function(x, effect)
{
  if (!effect %in% attr(x$`(Intercept)`$terms, "term.labels"))
  {
    stop("Specified effect not found")
  }

  # summary.aovlist is a list of lists containing data frames
  x <- flatten(summary(x))

  # Look through data frames for specified effect
  for (i in seq_along(x))
  {
    df <- x[[i]]

    row <- which(str_trim(row.names(df)) == effect)

    if (length(row) > 0)
    {
      petasq <- petasq_(df[row, "Sum Sq"], df["Residuals", "Sum Sq"])
    }
  }

  petasq
}

petasq_afex <- function(x, effect)
{
  anova <- anova(x, es = "pes", intercept = TRUE)

  if (!effect %in% row.names(anova))
  {
    stop("Specified effect not found")
  }

  anova[effect, "pes"]
}

petasq_ezanova <- function(x, effect)
{
  anova <- x$ANOVA

  if (!all(c("SSn", "SSd") %in% names(anova)))
  {
    stop("Parameter 'detailed' needs to be set to TRUE in call to `ezANOVA`")
  }

  if (!effect %in% anova$Effect)
  {
    stop("Specified effect not found")
  }
  else
  {
    row <- which(anova$Effect == effect)
  }

  petasq_(anova[row, "SSn"], anova[row, "SSd"])
}

#' Partial Eta Squared
#'
#' Calculate the partial eta squared effect size from sum of
#' squares.
#' \deqn{\eta_p^2 = \frac{SS_effect}{SS_effect + SS_error}}{partial eta squared
#' = SS_effect / (SS_effect + SS_error)}
#'
#' @param ss_effect numeric, sum of squares of the effect
#' @param ss_error numeric, sum of squares of the corresponding error
#' @export
petasq_ <- function(ss_effect, ss_error)
{
  ss_effect / (ss_effect + ss_error)
}

petasq_ci <- function(x, effect, sph_corr = "none", force_sph_corr = FALSE)
{
  # Use a pseudo-S3 method dispatch here, because `ezANOVA` returns a list
  # without a particular class

  # aov
  if (inherits(x, "aov"))
  {
    petasq_ci_aov(x, effect)
  }
  # aovlist
  else if (inherits(x, "aovlist"))
  {
    petasq_ci_aovlist(x, effect)
  }
  # afex
  else if (inherits(x, "afex_aov"))
  {
    petasq_ci_afex(x, effect, sph_corr, force_sph_corr)
  }
  # ez::ezANOVA
  else if (is.list(x) && names(x)[1] == "ANOVA")
  {
    petasq_ci_ezanova(x, effect, sph_corr, force_sph_corr)
  }
  else
  {
    stop("Unknown object passed to argument 'x'")
  }
}

#' @importFrom stringr str_trim
petasq_ci_aov <- function(x, effect)
{
  # Calculate ANOVA table
  anova <- summary(x, intercept = TRUE)[[1]]

  row.names(x) <- str_trim(row.names(x))

  if (!effect %in% row.names(x))
  {
    stop("Specified effect not found")
  }

  # Extract F and degrees of freedom for the requested effect from anova object
  f <- anova[row.names(anova) == effect, "F"]
  df_n <- anova[row.names(anova) == effect, "num Df"]
  df_d <- anova[row.names(anova) == effect, "den Df"]

  petasq_ci_(f, df_n, df_d)
}

petasq_ci_aovlist <- function(x, effect)
{
  # Calculate ANOVA tables for each stratum
  anova <- flatten(summary(x))

  # Clean row names
  anova <- map(anova, \(x) { row.names(x) <- str_trim(row.names(x)); x })

  # Find the requested effect in the ANOVA tables
  effect_position <-
    anova |>
    map(\(x) row.names(x) == effect) |>
    map(\(x) any(x)) |>
    unlist() |>
    which()

  anova <- anova[[effect_position]]

  f <- anova[row.names(anova) == effect, "F value"]
  df_n <- anova[row.names(anova) == effect, "Df"]
  df_d <- anova[row.names(anova) == "Residuals", "Df"]

  petasq_ci_(f, df_n, df_d)
}

petasq_ci_afex <- function(x, effect, sph_corr, force_sph_corr)
{
  # Set 'correction' to FALSE because afex does greenhouse-geisser correction on
  # all within-effects by default
  anova <- anova(x, intercept = TRUE, correction = "none")

  # Extract F and degrees of freedom for the requested effect from anova object
  f <- anova[rownames(anova) == effect, "F"]
  df_n <- anova[rownames(anova) == effect, "num Df"]
  df_d <- anova[rownames(anova) == effect, "den Df"]

  # Apply sphericity correction if requested and necessary
  if (effect %in% names(attr(x, "within")) && sph_corr != "none")
  {
    # To access sphericity tests in afex, we need to call `summary`
    s <- summary(x)

    corr_method <- switch(sph_corr, `greenhouse-geisser` =, gg = "GG",
                          `huynh-feldt` =, hf = "HF")

    # Extract Mauchly's test of sphericity
    sph_tests <- s$sphericity.tests

    if (s$sphericity.tests[effect, "p-value"] < .05 || force_sph_corr)
    {
        df_n <- df_n * s$pval.adjustments[effect, paste(corr_method, "eps")]
        df_d <- df_d * s$pval.adjustments[effect, paste(corr_method, "eps")]
    }
  }

  petasq_ci_(f, df_n, df_d)
}

petasq_ci_ezanova <- function(x, effect, sph_corr, force_sph_corr)
{
  anova <- x$ANOVA

  # Extract F and degrees of freedom for the requested effect from anova object
  f <- anova[rownames(anova) == effect, "F"]
  df_n <- anova[rownames(anova) == effect, "DFn"]
  df_d <- anova[rownames(anova) == effect, "DFd"]

  # Apply sphericity correction if requested and necessary
  if (effect %in% names(attr(x, "within")) && sph_corr != "none")
  {
    # ezANOVA stores sphericity tests and correction values in two data frames,
    # which are combined here.
    mauchlys <- left_join(x$`Mauchly's Test for Sphericity`,
                          x$`Sphericity Corrections`, by = "Effect")

    corr_method <- switch(sph_corr, `greenhouse-geisser` =, gg = "GG",
                          `huynh-feldt` =, hf = "HF")


    if (mauchlys[mauchlys$Effect == effect, "p"] < .05 || force_sph_corr)
    {
        df_n <- df_n * mauchlys[mauchlys$Effect == effect, paste(corr_method,
                                                                 "e")]
        df_d <- df_d * mauchlys[mauchlys$Effect == effect, paste(corr_method,
                                                                 "e")]
    }
  }

  petasq_ci_(f, df_n, df_d)
}

#' @importFrom MBESS conf.limits.ncf
petasq_ci_ <- function(f, df_n, df_d)
{
  # Calculate upper and lower limit of Lambda for 90%-CI
  limits <- MBESS::conf.limits.ncf(f, .9, df_n, df_d)
  
  # Convert Lambda to partial eta-squared
  ci <- c(limits$Lower.Limit, limits$Upper.Limit) /
    (c(limits$Lower.Limit, limits$Upper.Limit) + df_n + df_d + 1)

  paste0("[", fmt_es(ci[1], equal_sign = FALSE, leading_zero = FALSE), "; ",
         fmt_es(ci[2], equal_sign = FALSE, leading_zero = FALSE), "]")
}

getasq <- function(x, effect)
{
  # Use a pseudo-S3 method dispatch here, because `ezANOVA` returns a list
  # without a particular class

  # afex
  if (inherits(x, "afex_aov"))
  {
    getasq_afex(x, effect)
  }
  # ez::ezANOVA
  else if (is.list(x) && names(x)[1] == "ANOVA")
  {
    getasq_ezanova(x, effect)
  }
}

getasq_afex <- function(x, effect)
{
  # afex drops the 'observed' argument when calling `anova` on the afex_aov
  # object, so we need to get the getasq values from $anova_table. The only
  # thing we can't retrieve is the getasq for the intercept ...
  if (effect == "(Intercept)")
  {
    return(NA)
  }

  anova <- x$anova_table

  if (!"ges" %in% names(anova))
  {
    stop("Argument 'es' needs to be set to \"ges\" in call to `aov_*`")
  }

  if (!effect %in% row.names(anova))
  {
    stop("Specified effect not found")
  }

  anova[effect, "ges"]
}

getasq_ezanova <- function(x, effect)
{
  anova <- x$ANOVA

  if (!all(c("SSn", "SSd") %in% names(anova)))
  {
    stop("Parameter 'detailed' needs to be set to TRUE in call to `ezANOVA`")
  }

  if (!effect %in% anova$Effect)
  {
    stop("Specified effect not found")
  }

  anova[which(anova$Effect == effect), "ges"]
}
