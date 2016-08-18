#' Report Chi-squared test in APA style
#'
#' @param x A call to \code{chisq.test}
#' @param print_n Logical indicating whether to show sample size in text
#' @param format Character string specifying the output format. One of
#'   \code{"text"}, \code{"markdown"}, \code{"rmarkdown"}, \code{html},
#'   \code{"latex"}, \code{"docx"} or \code{"plotmath"}.
#' @param info Logical indicating whether to print a message on the used test
#'   (default is \code{FALSE})
#' @param print Logical indicating wheter to print the formatted output via
#'   \code{cat} (\code{TRUE}, default) or return as character string.
#' @examples
#' # Example data from ?chisq.test
#' m <- rbind(c(762, 327, 468), c(484, 239, 477))
#'
#' chisq_apa(chisq.test(m))
#'
#' @export
chisq_apa <- function(x, print_n = FALSE, format = c("text", "markdown",
                                                     "rmarkdown", "html",
                                                     "latex", "docx",
                                                     "plotmath"),
                      info = FALSE, print = TRUE)
{
  format <- match.arg(format)

  # Check if 'x' was a call to `chisq.test`
  if (!inherits(x, "htest") && !grepl("Chi-squared test", x$method))
  {
    stop("'x' must be a call to `chisq.test`")
  }

  if (format == "docx")
  {
    return(apa_to_docx("chisq_apa", x))
  }

  # Extract and format test statistics
  statistic <- fmt_stat(x$statistic)
  df <- x$parameter
  n <- if (print_n) paste(", n =", sum(x$observed)) else ""
  p <- fmt_pval(x$p.value)

  if (info) message(x$method)

  # Put the formatted string together
  text <- paste0(fmt_symb("chisq", format), "(", df, n, ") ", statistic, ", ",
                 fmt_symb("p", format), " ", p)

  if (format == "latex")
  {
    text <- fmt_latex(text)
  }
  else if (format == "plotmath")
  {
    # Convert text to an expression
    text <- fmt_plotmath(text, "(\\([0-9]+.*\\) [<=] [0-9]+\\.[0-9]{2}, )",
                         "( [<=>] \\.[0-9]{3})")

    # Text is an expression, so we can't use `cat` to print it to the console
    print <- FALSE
  }

  if (print) cat(text) else text
}
