#' Student's t-Test
#'
#' A wrapper for \code{t.test} which includes the original data in the returned
#' object.
#'
#' @inheritParams stats::t.test
#' @seealso \link{t.test}
#'
#' @export
t_test <- function(x, ...) UseMethod("t_test")

#' @rdname t_test
#' @importFrom stats complete.cases na.omit setNames t.test
#' @export
t_test.default <- function(x, y = NULL,
                           alternative = c("two.sided", "less", "greater"),
                           mu = 0, paired = FALSE, var.equal = FALSE,
                           conf.level = 0.95, ...)
{
  t <- t.test(x = x, y = y, alternative = alternative, mu = mu, paired = paired,
              var.equal = var.equal, conf.level = conf.level, ...)

  # Ensure that the 'data.name' element in the returned list matches that of a
  # call to t.test (is "x and y" otherwise)
  if (is.null(y))
  {
    dname <- deparse(substitute(x))
  }
  else
  {
    dname <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))
  }

  t$data.name <- dname

  # Add data to return list, remove NA
  if (is.null(y))
  {
    t[["data"]]$x <- na.omit(x)
  }
  else if (!paired)
  {
    t[["data"]]$x <- na.omit(x)
    t[["data"]]$y <- na.omit(y)
  }
  else
  {
    t[["data"]]$x <- x[complete.cases(x, y)]
    t[["data"]]$y <- y[complete.cases(x, y)]
  }

  t
}

#' @rdname t_test
#' @importFrom stats model.frame na.omit setNames t.test
#' @export
t_test.formula <- function(formula, data, subset, na.action, ...)
{
  t <- t.test(formula = formula, data = data, ...)

  mf <- na.omit(model.frame(formula, data))
  t$data <- setNames(split(mf[[1]], mf[[2]]), c("x", "y"))

  t
}
