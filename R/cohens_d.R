#' Cohen's d
#'
#' Calculate Cohen's d from raw data or a call to \code{t_test}/\code{t.test}.
#'
#' To calculate Cohen's d from summary statistics (M, SD, ..) use
#' \link{cohens_d_}.
#'
#' @importFrom stats sd
#' @param x A (non-empty) numeric vector of data values.
#' @param y An optional (non-empty) numeric vector of data values.
#' @param paired A logical indicating whether Cohen's d should be calculated for
#'   a paired sample or two independent samples \emph{(default)}. Ignored when
#'   calculating Cohen's for one sample.
#' @param corr Character specifying the correction applied to calculation of the
#'   effect size: \code{"none"} \emph{(default)} returns Cohen's d,
#'   \code{"hedges_g"} applies Hedges correction and \code{"glass_delta"}
#'   calculates Glass' \eqn{\Delta} (uses the standard deviation of the second
#'   group).
#' @param na.rm Logical. Should missing values be removed?
#' @param data A data frame containing either the variables in the formula
#'   \code{formula} or the variables specified by \code{dv} and \code{iv}.
#' @param dv Character indicating the name of the column in \code{data} for the
#'   dependent variable
#' @param iv Character indicating the name of the column in \code{data} for the
#'   independent variable
#' @param formula A formula of the form \code{lhs ~ rhs} where \code{lhs} is a
#'   numeric variable giving the data values (dependent variable) and \code{rhs}
#'   a factor with two levels giving the corresponding groups (independent
#'   variable).
#' @param ttest An object of class \code{htest} (a call to either \code{t_test}
#'   (preferred) or \code{t.test}).
#' @param ... Further arguments passed to methods.
#' @references Lakens, D. (2013). Calculating and reporting effect sizes to
#' facilitate cumulative science: a practical primer for t-tests and ANOVAs.
#' \emph{Frontiers in Psychology}, 4, 863. doi:10.3389/fpsyg.2013.00863
#' @examples
#' # Calculate from raw data
#' cohens_d(c(10, 15, 11, 14, 17), c(22, 18, 23, 25, 20))
#'
#' # Methods when working with data frames
#' cohens_d(sleep, dv = extra, iv = group, paired = TRUE)
#' # or
#' cohens_d(sleep, dv = "extra", iv = "group", paired = TRUE)
#' # formula interface
#' cohens_d(extra ~ group, sleep, paired = TRUE)
#'
#' # Or pass a call to t_test or t.test
#' cohens_d(t_test(extra ~ group, sleep, paired = TRUE))
#' @export
cohens_d <- function(...) UseMethod("cohens_d")

#' @rdname cohens_d
#' @export
cohens_d.default <- function(x, y = NULL, paired = FALSE,
                             corr = c("none", "hedges_g", "glass_delta"),
                             na.rm = FALSE, ...)
{
  corr <- match.arg(corr)

  # Two independent samples
  if (!paired && !is.null(y))
  {
    m1 <- mean(x, na.rm = na.rm)
    m2 <- mean(y, na.rm = na.rm)

    sd1 <- sd(x, na.rm)
    sd2 <- sd(y, na.rm)

    n1 <- if (!na.rm) length(x) else length(na.omit(x))
    n2 <- if (!na.rm) length(y) else length(na.omit(y))

    d <- cohens_d_(m1, m2, sd1, sd2, n1, n2, corr = corr)
  }
  else
  {
    # One sample
    if (is.null(y))
    {
      y <- 0
    }
    else
    {
      if (length(x) != length(y)) stop("'x' and 'y' must have the same length")
    }

    # Two dependent samples / one sample
    d <- mean(x - y, na.rm = na.rm) / sd(x - y, na.rm)
  }

  d
}

#' @rdname cohens_d
#' @export
cohens_d.data.frame <- function(data, dv, iv, paired = FALSE,
                                corr = c("none", "hedges_g", "glass_delta"),
                                na.rm = FALSE, ...)
{
  corr <- match.arg(corr)

  # Convert iv and dv to character if they are a name
  if (!is.character(substitute(iv))) iv <- as.character(substitute(iv))
  if (!is.character(substitute(dv))) dv <- as.character(substitute(dv))

  sp <- split(data[[dv]], data[[iv]])

  cohens_d(sp[[1]], sp[[2]], paired, corr, na.rm)
}

#' @rdname cohens_d
#' @importFrom stats model.frame setNames
#' @export
cohens_d.formula <- function(formula, data, paired = FALSE,
                             corr = c("none", "hedges_g", "glass_delta"),
                             na.rm = FALSE, ...)
{
  corr <- match.arg(corr)

  mf <- model.frame(formula, data)
  .data <- setNames(split(mf[[1]], mf[[2]]), c("x", "y"))

  do.call("cohens_d", c(.data, paired = paired, corr = corr, na.rm = na.rm))
}

#' @rdname cohens_d
#' @export
cohens_d.htest <- function(ttest, corr = c("none", "hedges_g", "glass_delta"),
                           ...)
{
  corr <- match.arg(corr)

  if (!grepl("t-test", ttest$method))
  {
    stop('ttest must be a call to either `t_test` or `t.test`')
  }

  # A call to `t_test` was passed to argument 'ttest'
  if (!is.null(ttest[["data"]]))
  {
    # t-test for two dependent samples
    if (grepl("Paired", ttest$method))
    {
      cohens_d(ttest$data$x, ttest$data$y, paired = TRUE)
    }
    # t-test for one sample
    else if (grepl("One Sample", ttest$method))
    {
      cohens_d(ttest$data$x)
    }
    # t-test for two independent samples
    else
    {
      cohens_d(ttest$data$x, ttest$data$y, corr = corr)
    }
  }
  # A call to `t.test` was passed to argument 'ttest'
  else
  {
    # t-test for two dependent samples
    if (grepl("Paired", ttest$method))
    {
      cohens_d_(t = unname(ttest$statistic), n = unname(ttest$parameter + 2),
                paired = TRUE)
    }
    # t-test for one sample
    else if (grepl("One Sample", ttest$method))
    {
      cohens_d_(t = unname(ttest$statistic), n = unname(ttest$parameter + 1),
                paired = TRUE)
    }
    # t-test for two independent samples with Welch's correction
    else if (grepl("Welch", ttest$method))
    {
      stop(paste(
        "A Welch test from a call to `t.test` is not supported.",
        "Use either `t_test` or set argument 'var.equal' in `t.test` to TRUE"))
    }
    # t-test for two independent samples
    else
    {
      if (corr == "glass_delta")
      {
        stop(paste(
          "Glass Delta is not supported when passing a test from `t.test`.",
          "Use `t_test` instead."))
      }

      cohens_d_(t = unname(ttest$statistic), n = unname(ttest$parameter + 2),
                corr = corr)
    }
  }
}

#' Cohen's d
#'
#' Calculate Cohens'd from different statistics (see Details).
#'
#' @param m1 Numeric, mean of the first group
#' @param m2 Numeric, mean of the second group
#' @param sd1 Numeric, standard deviation of the first group
#' @param sd2 Numeric, standard deviation of the second group
#' @param n1 Numeric, size of the first group
#' @param n2 Numeric, size of the second group
#' @param t Numeric, t-test statistic
#' @param n Numeric, total sample size
#' @param paired Logical indicating whether to calculate Cohen's for independent
#'   samples or one sample (\code{FALSE}, \emph{default}) or for dependent
#'   samples (\code{TRUE}).
#' @param corr Character specifying the correction applied to calculation of the
#'   effect size: \code{"none"} \emph{(default)} returns Cohen's d,
#'   \code{"hedges_g"} applies Hedges correction and \code{"glass_delta"}
#'   calculates Glass' \eqn{\Delta} (uses the standard deviation of the second
#'   group).
#' @details
#'   The following combinations of statistics are possible:
#'   \itemize{
#'     \item \code{m1}, \code{m2}, \code{sd1}, \code{sd2}, \code{n1} and
#'     \code{n2}
#'     \item \code{t}, \code{n1} and \code{n2}
#'     \item \code{t} and \code{n}
#'   }
#' @references
#'   Lakens, D. (2013). Calculating and reporting effect sizes to facilitate
#'   cumulative science: a practical primer for t-tests and ANOVAs.
#'   \emph{Frontiers in Psychology}, 4, 863. doi:10.3389/fpsyg.2013.00863
#' @export
cohens_d_ <- function(m1 = NULL, m2 = NULL, sd1 = NULL, sd2 = NULL, n1 = NULL,
                      n2 = NULL, t = NULL, n = NULL, paired = FALSE,
                      corr = c("none", "hedges_g", "glass_delta"))
{
  corr <- match.arg(corr)

  # Two independent samples with ms, sds and ns (no or hedges correction)
  if (!any(sapply(list(m1, m2, sd1, sd2, n1, n2), is.null)) &&
      corr != "glass_delta" && !paired)
  {
    d <- (m1 - m2) /
      sqrt(((n1 - 1) * sd1 ^ 2 + (n2 - 1) * sd2 ^ 2) / ((n1 + n2) - 2))

    if (corr == "hedges_g")
    {
      j <- function(a) gamma(a / 2) / (sqrt(a / 2) * gamma((a - 1) / 2))

      d <- d * j(n1 + n2 - 2)
    }
  }
  # Two independent samples with glass' correction
  else if (corr == "glass_delta" && !paired)
  {
    if (!any(sapply(list(m1, m2, sd2), is.null)))
    {
      d <- (m1 - m2) / sd2
    }
    else
    {
      stop("Arguments 'm1', 'm2' and 'sd2' are required for Glass Delta")
    }
  }
  # Two independent samples with t, n1 and n2
  else if (!any(sapply(list(n1, n2, t), is.null)))
  {
    d <- t * sqrt(1 / n1 + 1 / n2)
  }
  # Two independent samples with t and n
  else if (!any(sapply(list(t, n), is.null)) && !paired)
  {
    d <- 2 * t / sqrt(n)
  }
  # Two dependent samples with t and n
  else if (!any(sapply(list(t, n), is.null)) && paired)
  {
    d <- t / sqrt(n)
  }

  d
}
