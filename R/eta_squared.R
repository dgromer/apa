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

#' @importFrom stats anova
petasq_aov <- function(x, effect)
{
  x <- anova(x)

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
