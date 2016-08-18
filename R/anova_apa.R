#' Report ANOVA in APA style
#'
#' @param x A call to \code{ez::ezANOVA} or \code{afex::afex_ez},
#'   \code{afex::afex_car} or \code{afex::afex_4}
#' @param effect Character string indicating the name of the effect to display.
#'   If is \code{NULL}, all effects are reported (default).
#' @param sph_corr Character string indicating the method used for correction if
#'   the assumption of sphericity is violated (only applies to repeated-measures
#'   and mixed design ANOVA). Can be one of \code{"greenhouse-geisser"}
#'   (default), \code{"huynh-feldt"} or \code{"none"} (you may also use the
#'   abbreviations \code{"gg"} or \code{"hf"}).
#' @param es Character string indicating the effect size to display in the
#'   output, one of \code{"petasq"} (partial eta squared) or \code{"getasq"}
#'   (generalized eta squared) (you may also use the abbreviations \code{"pes"}
#'   or \code{"ges"}).
#' @param format Character string specifying the output format. One of
#'   \code{"text"}, \code{"markdown"}, \code{"rmarkdown"}, \code{html},
#'   \code{"latex"}, \code{"docx"} or \code{"plotmath"}.
#' @param info Logical indicating whether to print a message on the used test
#'   (default is \code{FALSE})
#' @param print Logical indicating wheter to print the formatted output via
#'   \code{cat} (\code{TRUE}, default) or return as a data frame.
#' @examples
#' # Using the ez package
#' library(ez)
#' data(ANT)
#'
#' x <- ezANOVA(ANT[ANT$error==0,], dv = rt, wid = subnum,
#'              within = c(cue, flank), between = group, detailed = TRUE)
#' anova_apa(x)
#'
#' # Using the afex package
#' library(afex)
#' data(md_12.1)
#'
#' y <- aov_ez(id = "id", dv = "rt", data = md_12.1,
#'             within = c("angle", "noise"))
#' anova_apa(y)
#'
#' @export
anova_apa <- function(x, effect = NULL,
                      sph_corr = c("greenhouse-geisser", "gg", "huynh-feldt",
                                   "hf", "none"),
                      es = c("petasq", "pes", "getasq", "ges"),
                      format = c("text", "markdown", "rmarkdown", "html",
                                 "latex", "docx", "plotmath"),
                      info = FALSE, print = TRUE)
{
  sph_corr <- match.arg(sph_corr)
  es <- match.arg(es)
  format <- match.arg(format)

  # Use a pseudo-S3 method dispatch, because `ezANOVA` returns a list without a
  # particular class

  if (inherits(x, "afex_aov"))
  {
    anova_apa_afex(x, effect, sph_corr, es, format, info, print)
  }
  else if (is.list(x) && names(x)[1] == "ANOVA")
  {
    anova_apa_ezanova(x, effect, sph_corr, es, format, info, print)
  }
  else
  {
    stop("'x' must be a call to `ez::ezANOVA` or `afex::aov_*`")
  }
}

#' @importFrom dplyr data_frame
#' @importFrom magrittr %>% %<>%
#' @importFrom purrr map map_chr
anova_apa_afex <- function(x, effect, sph_corr, es, format, info, print)
{
  info_msg <- ""

  # Set 'correction' to FALSE because afex does greenhouse-geisser correction on
  # all within-effects by default
  anova <- anova(x, intercept = TRUE, correction = "none")

  # Extract information from anova object
  tbl <- data_frame(
    effects = row.names(anova),
    statistic = map_chr(anova$F, fmt_stat),
    df_n = anova$`num Df`, df_d = anova$`den Df`,
    p = map_chr(anova$`Pr(>F)`, fmt_pval),
    symb = map_chr(anova$`Pr(>F)`, p_to_symbol),
    es = map_chr(effects, ~ fmt_es(do.call(es, list(x, .x)),
                                   leading_zero = FALSE))
  )

  # Check if within-effects are present
  if (length(attr(x, "within")) != 0)
  {
    # To access sphericity tests in afex, we need to call `summary`
    s <- summary(x)

    corr_method <- switch(sph_corr, `greenhouse-geisser` =, gg = "GG",
                          `huynh-feldt` =, hf = "HF")

    # Extract Mauchly's test of sphericity
    sph_tests <- s$sphericity.tests

    # Check which effects do not meet the assumption of sphericity
    mauchlys <- dimnames(sph_tests)[[1]][which(sph_tests[, "p-value"] < .05)]

    if (length(mauchlys) > 0)
    {
      # Apply correction to degrees of freedom
      tbl[tbl$effects %in% mauchlys, c("df_n", "df_d")] %<>%
        # Multiply df with correction factor (epsilon)
        `*`(s$pval.adjustments[mauchlys, paste(corr_method, "eps")]) %>%
        # Format to two decimal points
        map(fmt_stat, equal_sign = FALSE)

      # Replace p-values in tbl with corrected ones
      tbl[tbl$effects %in% mauchlys, "p"] <-
        s$pval.adjustments[mauchlys, paste0("Pr(>F[", corr_method, "])")] %>%
        map_chr(fmt_pval)

      # Add performed corrections to info message
      info_msg %<>% paste0(
        "Sphericity corrections:\n",
        "  The following effects were adjusted using the ",
        if (corr_method == "GG") "Greenhouse-Geisser" else "Huynh-Feldt",
        " correction:\n",
        paste0("  ", mauchlys, " (Mauchly's W ",
               map_chr(sph_tests[mauchlys, "Test statistic"], fmt_stat),
               ", p ", map_chr(sph_tests[mauchlys, "p-value"], fmt_pval), ")",
               collapse = "\n")
      )
    }
    else
    {
      info_msg %<>% paste0(
        "Sphericity corrections:\n",
        "  No corrections applied, all p-values for Mauchly's test p > .05"
      )
    }
  }

  if (info && info_msg != "") message(info_msg)

  anova_apa_print(tbl, effect, es, format, print)
}

#' @importFrom dplyr data_frame left_join
#' @importFrom magrittr %>% %<>%
anova_apa_ezanova <- function(x, effect, sph_corr, es, format, info, print)
{
  info_msg <- ""

  anova <- x$ANOVA

  if (!all(c("SSn", "SSd") %in% names(anova)))
  {
    stop("Parameter 'detailed' needs to be set to TRUE in call to `ezANOVA`")
  }

  # Extract information from anova object
  tbl <- data_frame(
    effects = anova$Effect,
    statistic = map_chr(anova$F, fmt_stat),
    df_n = anova$DFn, df_d = anova$DFd, p = map_chr(anova$p, fmt_pval),
    symb = map_chr(anova$p, p_to_symbol),
    es = map_chr(effects, ~ fmt_es(do.call(es, list(x, .x)),
                                   leading_zero = FALSE))
  )

  # Apply correction for violation of sphericity if required
  if ("Mauchly's Test for Sphericity" %in% names(x) && sph_corr != "none")
  {
    corr_method <- switch(sph_corr, `greenhouse-geisser` =, gg = "GG",
                          `huynh-feldt` =, hf = "HF")

    # ezANOVA stores sphericity tests and correction values in two data frames,
    # which are combined here.
    # Next, check which effects do not meet the assumption of sphericity
    mauchlys <- left_join(x$`Mauchly's Test for Sphericity`,
                          x$`Sphericity Corrections`, by = "Effect") %>%
      `[`(.$p < .05, )

    if (nrow(mauchlys) > 0)
    {
      # Apply correction to degrees of freedom
      tbl[match(mauchlys$Effect, tbl$effects), c("df_n", "df_d")] %<>%
        # Multiply df with correction factor (epsilon)
        `*`(mauchlys[[paste0(corr_method, "e")]]) %>%
        # Format to two decimal points
        map(fmt_stat, equal_sign = FALSE)

      # Replace p-values in tbl with corrected ones
      tbl[match(mauchlys$Effect, tbl$effects), "p"] <-
        mauchlys[[paste0("p[", corr_method, "]")]] %>%
        map_chr(fmt_pval)

      # Add performed corrections to info message
      info_msg %<>% paste0(
        "Sphericity corrections:\n",
        "  The following effects were adjusted using the ",
        if (corr_method == "GG") "Greenhouse-Geisser" else "Huynh-Feldt",
        " correction:\n",
        paste0("  ", mauchlys$Effect, " (Mauchly's W ",
               map_chr(mauchlys$W, fmt_stat), ", p ",
               map_chr(mauchlys$p, fmt_pval), ")", collapse = "\n")
      )
    }
    else
    {
      info_msg %<>% paste0(
        "Sphericity corrections:\n",
        "  No corrections applied, all p-values for Mauchly's test p > .05"
      )
    }
  }

  if (info && info_msg != "") message(info_msg)

  anova_apa_print(tbl, effect, es, format, print)
}

#' @importFrom dplyr data_frame
#' @importFrom magrittr %>% %<>%
#' @importFrom purrr map_chr
#' @importFrom rmarkdown render
anova_apa_print <- function(tbl, effect, es_name, format, print)
{
  # Output for default parameters
  if (format == "text" && print)
  {
    anova_apa_print_default(tbl, effect, es_name)
  }
  else if (format == "docx")
  {
    anova_apa_print_docx(tbl, effect, es_name)
  }
  else
  {
    # Put the formatted string together
    text <- paste0(fmt_symb("F", format), "(", tbl$df_n, ", ", tbl$df_d, ") ",
                   tbl$statistic, ", ", fmt_symb("p", format), " ", tbl$p, ", ",
                   fmt_symb(es_name, format), " ", tbl$es)

    if (format == "plotmath")
    {
      return(anova_apa_print_plotmath(tbl, text, effect))
    }
    else if (format == "latex")
    {
      text <- map_chr(text, fmt_latex)
    }

    # cat to console
    if (print)
    {
      if (is.null(effect))
      {
        # Align names of effects
        tbl$effects <- format(paste0(tbl$effects, ": "),
                              width = max(map_chr(tbl$effects, nchar)))

        # Add line breaks
        text <- paste0(tbl$effects, text, "\n")

        for (i in seq_along(text))
        {
          cat(text[i])
        }
      }
      else
      {
        cat(text[which(tbl$effect == effect)])
      }
    }
    # Return as string(s)
    else
    {
      if (is.null(effect))
      {
        data_frame(effect = tbl$effects, text = text)
      }
      else
      {
        text[which(tbl$effects == effect)]
      }
    }
  }
}

anova_apa_print_default <- function(tbl, effect, es_name)
{
  # Split test statistic and its sign, because the tabular output will be
  # aligned along the test statistic
  sign <- substr(tbl$statistic, 1, 1)
  statistic <- substr(tbl$statistic, 2, nchar(tbl$statistic))

  tbl <- data_frame(
    Effect = tbl$effects,
    ` ` = paste0("F(", tbl$df_n, ", ", tbl$df_d, ") ", sign,
                 format(statistic, width = max(nchar(statistic)),
                        justify = "right"),
                 ", p ", tbl$p, ", ", fmt_symb(es_name, "text"), " ", tbl$es,
                 " ", format(tbl$symb, width = 3))
  )

  if (is.null(effect))
  {
    # Use print method from base R data.frame instead of tibble
    print.data.frame(tbl)
  }
  else
  {
    # Extract text for specified effect from tbl.
    `[.data.frame`(tbl, tbl$Effect == effect, " ") %>%
      # Remove alignment whitespaces
      gsub("[[:blank:]]+", " ", .) %>%
      cat()
  }
}

anova_apa_print_docx <- function(tbl, effect, es_name)
{
  # Create temporary markdown file
  tmp <- tempfile("anova_apa", fileext = ".md")
  sink(tmp)
  # Put the formatted string together
  out <- paste0(tbl$effects, " *F*(", tbl$df_n, ", ", tbl$df_d, ") ",
                tbl$statistic, ", *p* ", tbl$p, ", ",
                fmt_symb(es_name, "rmarkdown"), " ", tbl$es, "\n\n")

  if (is.null(effect))
  {
    # Write output line by line to the markdown file
    for (i in seq_along(out)) cat(out[i])
  }
  else
  {
    # Select only the output string for 'effect'
    out[which(tbl$effects == effect)] %>%
      # Remove the name of the effect from the beginning of the string
      sub("^.*\\s\\*F\\*", "\\*F\\*", .) %>%
      # Write to markdown file
      cat()
  }

  sink()
  # Convert markdown to docx
  outfile <- render(tmp, output_format = "word_document", quiet = TRUE)

  sys_open(outfile)
}

anova_apa_print_plotmath <- function(tbl, text, effect)
{
  # Check if 'effect' is specified for plotmath format, because we can't print
  # a data frame with expressions.
  if (is.null(effect))
  {
    stop("Argument 'effect' must be specified if 'format' is \"plotmath\"")
  }

  fmt_plotmath(
    text[which(tbl$effects == effect)],
    "(\\([0-9]+\\.?[0-9]*, [0-9]+\\.?[0-9]*\\) [<=] [0-9]+\\.[0-9]{2}, )",
    "( [<=>] \\.[0-9]{3}, )", "( [<=] -?[0-9]*\\.[0-9]{2}$)"
  )
}
