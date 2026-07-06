#' APA Formatting for Quarto and RMarkdown Reports
#'
#' A wrapper around the \code{*_apa} functions, providing a convenient way to
#' use the formatters in inline code in Quarto and RMarkdown documents.
#'
#' @param x An \R object. Must be a call to one of \code{afex::aov_4},
#'   \code{afex::aov_car}, \code{afex::aov_ez}, \code{chisq.test},
#'   \code{cor.test}, \code{ez::ezANOVA} or \code{t_test}.
#' @param effect (only applicable if \code{x} is an ANOVA) Character string
#'   indicating the name of the effect to display.
#' @param format Character string specifying the output format. One of
#'   \code{"text"}, \code{"markdown"}, \code{"quarto"}, \code{"rmarkdown"},
#'   \code{html}, \code{"latex"} or \code{"docx"}.
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
      out <- chisq_apa(x, format = format, print = print, ...)
    }
    else if (grepl("correlation", x$method))
    {
      out <- cor_apa(x, format = format, print = print, ...)
    }
    else if (grepl("t-test", x$method))
    {
      out <- t_apa(x, format = format, print = print, ...)
    }
    else
    {
      stop("Unkown type passed to 'x'")
    }
  }
  else if (inherits(x, c("aov", "lm")) || inherits(x, c("aovlist", "listof")) ||
           inherits(x, "afex_aov") || (is.list(x) && names(x)[1] == "ANOVA"))
  {
    if (is.null(effect))
    {
      stop("For calls to ANOVA, an `effect` must be specified.")
    }

    out <- anova_apa(x, effect, format = format, print = print, ...)
  }
  else
  {
    stop("Unkown type passed to 'x'")
  }

  # Prevent escaping of markdown and math syntax in Quarto rendering
  I(out)
}
