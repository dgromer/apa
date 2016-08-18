#' Report t-Test in APA style
#'
#' @param x A call to \code{t_test} or \code{t.test}
#' @param es Character specifying the effect size to report. One of
#'   \code{"cohens_d"} (default), \code{"hedges_g"} or \code{"glass_delta"} if
#'   \code{x} is an independent samples t-test. Ignored if \code{x} is a paired
#'   samples or one sample t-test (cohen's d is reported for these test).
#' @param format Character string specifying the output format. One of
#'   \code{"text"}, \code{"markdown"}, \code{"rmarkdown"}, \code{html},
#'   \code{"latex"}, \code{"docx"} or \code{"plotmath"}.
#' @param info Logical indicating whether to print a message on the used test
#'   (default is \code{FALSE})
#' @param print Logical indicating wheter to print the formatted output via
#'   \code{cat} (\code{TRUE}, default) or return as character string.
#' @examples
#' # Two independent samples t-test
#' t_apa(t_test(1:10, y = c(7:20)))
#'
#' # Two dependent samples t-test
#' t_apa(t_test(extra ~ group, sleep, paired = TRUE))
#'
#' @export
t_apa <- function(x, es = "cohens_d", format = c("text", "markdown",
                                                 "rmarkdown", "html", "latex",
                                                 "docx", "plotmath"),
                  info = FALSE, print = TRUE)
{
  format <- match.arg(format)

  # Check if 'x' was a call to `t_test` or `t.test`
  if (!inherits(x, "htest") && !grepl("t-test", x$method))
  {
    stop("'x' must be a call to `t_test` or `t.test`")
  }

  if (format == "docx")
  {
    return(apa_to_docx("t_apa", x, es = es))
  }

  # Extract and format test statistics
  statistic <- fmt_stat(x$statistic)
  df <- x$parameter
  p <- fmt_pval(x$p.value)
  d <- fmt_es(cohens_d(x, corr = if (es == "cohens_d") "none" else es))

  # Format degrees of freedom if Welch correction was applied
  if (grepl("Welch", x$method))
  {
    df <- fmt_stat(df, equal_sign = FALSE)
  }

  # Check if Hedge's g* or Glass' Delta were requested for one sample or paired
  # t-test.
  if (es != "cohens_d" && (grepl("One Sample|Paired", x$method)))
  {
    warning(paste0("'", es, "' not available for ", x$method, ",",
                   " 'cohens_d' will be reported instead."))
    es <- "cohens_d"
  }

  if (info) message(x$method)

  # Put the formatted string together
  text <- paste0(fmt_symb("t", format), "(", df, ") ", statistic, ", ",
                 fmt_symb("p", format), " ", p, ", ", fmt_symb(es, format), " ",
                 d)

  if (format == "latex")
  {
    text <- fmt_latex(text)
  }
  else if (format == "plotmath")
  {
    # Convert text to an expression
    text <- fmt_plotmath(
      text, "(\\([0-9]+\\.*[0-9]*\\) [<=] -?[0-9]+\\.[0-9]{2}, )",
      "( [<=>] \\.[0-9]{3}, )", "( [<=] -?[0-9]+\\.[0-9]{2}$)"
    )

    # Text is an expression, so we can't use `cat` to print it to the console
    print <- FALSE
  }

  if (print) cat(text) else text
}
