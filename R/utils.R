#' @importFrom stats terms
# Extract data from a data frame using a formula
extract_data_formula <- function(formula, data, ...)
{
  x <- list()

  # Extract data using the code from stats/R/t.test.R
  oneSampleOrPaired <- FALSE
  if (length(attr(terms(formula[-2L]), "term.labels")) != 1L)
    if (formula[[3L]] == 1L)
      oneSampleOrPaired <- TRUE
    else
      stop("'formula' missing or incorrect")
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  ## need stats:: for non-standard evaluation
  m[[1L]] <- quote(stats::model.frame)
  m$... <- NULL
  mf <- eval(m, parent.frame())
  names(mf) <- NULL
  response <- attr(attr(mf, "terms"), "response")
  if (! oneSampleOrPaired) {
    g <- factor(mf[[-response]])
    if (nlevels(g) != 2L)
      stop("grouping factor must have exactly 2 levels")
    DATA <- split(mf[[response]], g)
    # apa: set data for two sample t-test
    x$x <- DATA[[1L]][complete.cases(DATA[[1L]], DATA[[2L]])]
    x$y <- DATA[[2L]][complete.cases(DATA[[1L]], DATA[[2L]])]
  }
  else { # 1-sample and paired tests
    respVar <- mf[[response]]
    if (inherits(respVar, "Pair")) {
      # apa: set data for paired t-test
      x$x <- respVar[, 1L][complete.cases(respVar[, 1L],
                                                    respVar[, 2L])]
      x$y <- respVar[, 2L][complete.cases(respVar[, 1L],
                                                    respVar[, 2L])]
    }
    else {
      # apa: set data for one sample t-test
      x$x <- na.omit(respVar)
    }
  }

  x
}
