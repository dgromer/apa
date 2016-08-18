#' Report Correlation in APA style
#'
#' @param x A call to \code{cor.test}
#' @param format Character string specifying the output format. One of
#'   \code{"text"}, \code{"markdown"}, \code{"rmarkdown"}, \code{html},
#'   \code{"latex"}, \code{"docx"} or \code{"plotmath"}.
#' @param info Logical indicating whether to print a message on the used test
#'   (default is \code{FALSE})
#' @param print Logical indicating wheter to print the formatted output via
#'   \code{cat} (\code{TRUE}, default) or return as character string.
#' @examples
#' # Example data from ?cor.test
#' x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
#' y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)
#'
#' cor_apa(cor.test(x, y))
#'
#' # Spearman's rho
#' cor_apa(cor.test(x, y, method = "spearman"))
#'
#' # Kendall's tau
#' cor_apa(cor.test(x, y, method = "kendall"))
#'
#' @export
cor_apa <- function(x, format = c("text", "markdown", "rmarkdown", "html",
                                  "latex", "docx", "plotmath"),
                    info = FALSE, print = TRUE)
{
  format <- match.arg(format)

  # Check if 'x' was a call to `cor.test`
  if (!inherits(x, "htest") && !grepl("correlation", x$method))
  {
    stop("'x' must be a call to `cor.test`")
  }

  if (format == "docx")
  {
    return(apa_to_docx("cor_apa", x))
  }

  # Extract and format test statistics
  coef <- tolower(strsplit(x$method, " ")[[1]][1])
  estimate <- fmt_stat(x$estimate, leading_zero = FALSE,
                       negative_values = TRUE)
  df <- x$parameter
  p <- fmt_pval(x$p.value)

  if (info) message(x$method)

  # Put the formatted string together
  text <- paste0(fmt_symb(coef, format),
                 if (coef == "pearson's") paste0("(", df, ") ") else " ",
                 estimate, ", ", fmt_symb("p", format), " ", p)

  if (format == "latex")
  {
    text <- fmt_latex(text)
  }
  else if (format == "plotmath")
  {
    # Convert text to an expression
    text <- fmt_plotmath(text, "(\\([0-9]+\\))", "( [<=] -?\\.[0-9]{2}, )",
                         "( [<=>] \\.[0-9]{3})")

    # Text is an expression, so we can't use `cat` to print it to the console
    print <- FALSE
  }

  if (print) cat(text) else text
}
