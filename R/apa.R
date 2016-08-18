#' APA Formatting for RMarkdown Reports
#'
#' A wrapper around the \code{*_apa} functions, providing a convenient way to
#' use the formatters in inline code in RMarkdown documents.
#'
#' @param x An \R object. Must be a call to one of \code{afex::aov_4},
#'   \code{afex::aov_car}, \code{afex::aov_ez}, \code{chisq.test},
#'   \code{cor.test}, \code{ez::ezANOVA} or \code{t_test}.
#' @param effect (only applicable if \code{x} is an ANOVA) Character string
#'   indicating the name of the effect to display. If is \code{NULL}, all
#'   effects are reported (default).
#' @param format Character string specifying the output format. One of
#'   \code{"text"}, \code{"markdown"}, \code{"rmarkdown"}, \code{html},
#'   \code{"latex"} or \code{"docx"}.
#' @param print Logical indicating whether to return the result as an \R object
#'   (\code{FALSE}) or print using \code{cat} (\code{TRUE}).
#' @param ... Further arguments passed to other methods
#' @seealso \link{anova_apa}, \link{chisq_apa},
#'   \link{cor_apa}, \link{t_apa}
#'
#' @export
apa <- function(x, effect = NULL, format = "rmarkdown", print = FALSE, ...)
{
  if (inherits(x, "htest"))
  {
    if (grepl("Chi-squared test", x$method))
    {
      chisq_apa(x, format = format, print = print, ...)
    }
    else if (grepl("correlation", x$method))
    {
      cor_apa(x, format = format, print = print, ...)
    }
    else if (grepl("t-test", x$method))
    {
      t_apa(x, format = format, print = print, ...)
    }
    else
    {
      stop("Unkown type passed to 'x'")
    }
  }
  else if (inherits(x, "afex_aov") || (is.list(x) && names(x)[1] == "ANOVA"))
  {
    anova_apa(x, effect, format = format, print = print, ...)
  }
  else
  {
    stop("Unkown type passed to 'x'")
  }
}
