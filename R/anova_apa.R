#' Report ANOVA in APA style
#'
#' @param x A call to \code{aov}, \code{ez::ezANOVA}, or \code{afex::afex_ez},
#'   \code{afex::afex_car} or \code{afex::afex_4}
#' @param effect Character string indicating the name of the effect to display.
#'   If is \code{NULL}, all effects are reported (default).
#' @param sph_corr Character string indicating the method used for correction if
#'   the assumption of sphericity is violated (only applies to repeated-measures
#'   and mixed design ANOVA). Can be one of \code{"greenhouse-geisser"}
#'   (default), \code{"huynh-feldt"} or \code{"none"} (you may also use the
#'   abbreviations \code{"gg"} or \code{"hf"}).
#' @param force_sph_corr Logical indicating if sphericity correction should be
#'   applied to all within factors regardless of what the result of Mauchly's
#'   test of sphericity is (default is \code{FALSE}).
#' @param es Character string indicating the effect size to display in the
#'   output, one of \code{"petasq"} (partial eta squared) or \code{"getasq"}
#'   (generalized eta squared) (you may also use the abbreviations \code{"pes"}
#'   or \code{"ges"}).
#' @param format Character string specifying the output format. One of
#'   \code{"text"}, \code{"markdown"}, \code{"rmarkdown"}, \code{html},
#'   \code{"latex"}, \code{"latex_math"}, \code{"docx"} or \code{"plotmath"}.
#' @param info Logical indicating whether to print a message on the used test
#'   (default is \code{FALSE})
#' @param print Logical indicating whether to print the formatted output via
#'   \code{cat} (\code{TRUE}, default) or return as a data frame.
#' @examples
#' # Using the ez package
#' if (requireNamespace("ez", quietly = TRUE)) {
#'   data(ANT, package = "ez")
#'
#'   x <- ez::ezANOVA(ANT[ANT$error==0,], dv = rt, wid = subnum,
#'                    within = c(cue, flank), between = group, detailed = TRUE)
#'   anova_apa(x)
#' }
#'
#' # Using the afex package
#' if (requireNamespace("afex", quietly = TRUE)) {
#'   data(md_12.1, package = "afex")
#'
#'   y <- afex::aov_ez(id = "id", dv = "rt", data = md_12.1,
#'                     within = c("angle", "noise"))
#'   anova_apa(y)
#' }
#'
#' @export
anova_apa <- function(x, effect = NULL,
                      sph_corr = c("greenhouse-geisser", "gg", "huynh-feldt",
                                   "hf", "none"),
                      force_sph_corr = FALSE,
                      es = c("petasq", "pes", "getasq", "ges"),
                      format = c("text", "markdown", "rmarkdown", "html",
                                 "latex", "latex_math", "docx", "plotmath"),
                      info = FALSE, print = TRUE)
{
  sph_corr <- match.arg(sph_corr)
  es <- match.arg(es)
  format <- match.arg(format)

  es <- switch(es, pes =, petasq = "petasq", ges =, getasq = "getasq")

  # Use a pseudo-S3 method dispatch, because `ezANOVA` returns a list without a
  # particular class

  if (inherits(x, c("aov", "lm")))
  {
    anova_apa_aov(x, effect, es, format, info, print)
  }
  else if (inherits(x, c("aovlist", "listof")))
  {
    anova_apa_aovlist(x, effect, sph_corr, es, format, info, print)
  }
  else if (inherits(x, "afex_aov"))
  {
    anova_apa_afex(x, effect, sph_corr, force_sph_corr, es, format, info, print)
  }
  else if (is.list(x) && names(x)[1] == "ANOVA")
  {
    anova_apa_ezanova(x, effect, sph_corr, force_sph_corr, es, format, info,
                      print)
  }
  else
  {
    stop("'x' must be a call to `aov`, `ez::ezANOVA`, or `afex::aov_*`")
  }
}

#' @importFrom tibble tibble
#' @importFrom purrr map_chr
#' @importFrom stringr str_trim
anova_apa_aov <- function(x, effect, es, format, info, print)
{
  # Check for unsupported effect size for calls to `aov`
  if (es == "getasq")
  {
    warning(paste("A call to `aov` does not support generalized eta-squared,",
                  "using partial eta-squared instead."), call. = FALSE)

    es <- "petasq"
  }

  info_msg <- ""

  # Calculate ANOVA table
  anova <- summary(x, intercept = TRUE)[[1]]

  # The row number where residuals are stored
  row_resid <- nrow(anova)

  # Extract information from anova object
  tbl <- tibble(
    effects = str_trim(row.names(anova)[-row_resid]),
    statistic = map_chr(anova$`F value`[-row_resid], fmt_stat),
    df_n = anova$Df[-row_resid], df_d = anova$Df[row_resid],
    p = map_chr(anova$`Pr(>F)`[-row_resid], fmt_pval),
    symb = map_chr(anova$`Pr(>F)`[-row_resid], p_to_symbol),
    es = map_chr(effects, \(.x) fmt_es(do.call(es, list(x, .x)),
                                       leading_zero = FALSE))
  )

  if (info && info_msg != "") message(info_msg)

  anova_apa_print(tbl, effect, es, format, print)
}

#' @importFrom dplyr bind_rows
#' @importFrom purrr flatten map
anova_apa_aovlist <- function(x, effect, sph_corr, es, format, info, print)
{
  # Inform that calls to `aov` do not support sphericity correction
  if (sph_corr != "none")
  {
    warning(paste("A call to `aov` does not support sphericity correction,",
                  "continuing without correction of possible violated",
                  "sphericity"), call. = FALSE)
  }

  # Check for unsupported effect size for calls to `aov`
  if (es == "getasq")
  {
    warning(paste("A call to `aov` does not support generalized eta-squared,",
                  "using partial eta-squared instead."), call. = FALSE)

    es <- "petasq"
  }

  info_msg <- ""

  # Calculate ANOVA tables for each stratum
  anova <- flatten(summary(x))

  # Extract information from list of ANOVA tables and store in single data frame
  tbl <- bind_rows(map(anova, extract_stats_aovlist))

  # Calculate effect sizes as extra step, because `extract_stats_aovlist` can't
  # call effect size function on aovlist object ('x') as this is not forwarded.
  tbl$es <- map_chr(tbl$effects, \(.x) fmt_es(do.call(es, list(x, .x)),
                                              leading_zero = FALSE))

  # Reorder rows in tbl
  tbl <- reorder_anova_tbl(tbl)

  if (info && info_msg != "") message(info_msg)

  anova_apa_print(tbl, effect, es, format, print)
}

#' @importFrom tibble tibble
#' @importFrom stringr str_trim
extract_stats_aovlist <- function(x)
{
  # Return NULL if stratum contains residuals only
  if (nrow(x) == 1)
  {
    return(NULL)
  }

  # The row number where residuals are stored
  row_resid <- nrow(x)

  tibble(
    effects = str_trim(row.names(x)[-row_resid]),
    statistic = map_chr(x$`F value`[-row_resid], fmt_stat),
    df_n = x$Df[-row_resid], df_d = x$Df[row_resid],
    p = map_chr(x$`Pr(>F)`[-row_resid], fmt_pval),
    symb = map_chr(x$`Pr(>F)`[-row_resid], p_to_symbol)
  )
}

#' @importFrom dplyr rowwise mutate_at
#' @importFrom tibble tibble
#' @importFrom magrittr %>% %<>%
#' @importFrom purrr map map_chr
#' @importFrom stringr str_extract
anova_apa_afex <- function(x, effect, sph_corr, force_sph_corr, es, format,
                           info, print)
{
  info_msg <- ""

  # Set 'correction' to FALSE because afex does greenhouse-geisser correction on
  # all within-effects by default
  anova <- anova(x, intercept = TRUE, correction = "none")

  # Extract information from anova object
  tbl <- tibble(
    effects = row.names(anova),
    statistic = map_chr(anova$F, fmt_stat),
    df_n = anova$`num Df`, df_d = anova$`den Df`,
    p = map_chr(anova$`Pr(>F)`, fmt_pval),
    symb = map_chr(anova$`Pr(>F)`, p_to_symbol),
    es = map_chr(effects, \(.x) fmt_es(do.call(es, list(x, .x)),
                                       leading_zero = FALSE))
  )

  # Check if within-effects are present and user wants sphericity correction
  if (length(attr(x, "within")) != 0 && sph_corr != "none")
  {
    # To access sphericity tests in afex, we need to call `summary`
    s <- summary(x)

    corr_method <- switch(sph_corr, `greenhouse-geisser` =, gg = "GG",
                          `huynh-feldt` =, hf = "HF")

    # Extract Mauchly's test of sphericity
    sph_tests <- s$sphericity.tests

    # Check if user wants sphericity correction for all within factors
    if (force_sph_corr)
    {
      # Select all within factors
      mauchlys <- dimnames(sph_tests)[[1]]
    }
    else
    {
      # Check which effects do not meet the assumption of sphericity
      mauchlys <- dimnames(sph_tests)[[1]][which(sph_tests[, "p-value"] < .05)]
    }

    if (length(mauchlys) > 0)
    {
      # Apply correction to degrees of freedom
      tbl[tbl$effects %in% mauchlys, c("df_n", "df_d")] %<>%
        # Multiply df with correction factor (epsilon)
        `*`(s$pval.adjustments[mauchlys, paste(corr_method, "eps")])

      # Since corrected dfs have decimal places, we need to format these to two
      tbl <-
        tbl %>%
        rowwise() %>%
        # . %% 1 == 0 checks if number has decimal places
        # As of tibble 3.0.0 we need to manually convert all column entries to
        # character, as types are not converted automatically
        mutate_at(c("df_n", "df_d"),
                  \(x) ifelse(x %% 1 == 0, as.character(x),
                              fmt_stat(x, equal_sign = FALSE)))

      # Replace p-values in tbl with corrected ones
      tbl[tbl$effects %in% mauchlys, "p"] <-
        s$pval.adjustments[mauchlys, paste0("Pr(>F[", corr_method, "])")] %>%
        map_chr(fmt_pval)

      # Update significance asterisks
      tbl$symb <-
        tbl$p %>%
        # P-values have already been formatted, so need to workaround that
        map_chr(\(x) {
          if (x == "< .001")
          {
            "***"
          }
          else
          {
            x %>% str_extract("[0-9.]+") %>% as.numeric() %>% p_to_symbol()
          }
        })

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

  # Reorder rows in tbl
  tbl <- reorder_anova_tbl(tbl)

  if (info && info_msg != "") message(info_msg)

  anova_apa_print(tbl, effect, es, format, print)
}

#' @importFrom dplyr left_join rowwise mutate_at
#' @importFrom magrittr %>% %<>%
#' @importFrom stringr str_extract
#' @importFrom tibble tibble
anova_apa_ezanova <- function(x, effect, sph_corr, force_sph_corr, es, format,
                              info, print)
{
  info_msg <- ""

  anova <- x$ANOVA

  if (!all(c("SSn", "SSd") %in% names(anova)))
  {
    stop("Parameter 'detailed' needs to be set to TRUE in call to `ezANOVA`")
  }

  # Extract information from anova object
  tbl <- tibble(
    effects = anova$Effect,
    statistic = map_chr(anova$F, fmt_stat),
    df_n = anova$DFn, df_d = anova$DFd, p = map_chr(anova$p, fmt_pval),
    symb = map_chr(anova$p, p_to_symbol),
    es = map_chr(effects, \(.x) fmt_es(do.call(es, list(x, .x)),
                                       leading_zero = FALSE))
  )

  # Apply correction for violation of sphericity if required
  if ("Mauchly's Test for Sphericity" %in% names(x) && sph_corr != "none")
  {
    corr_method <- switch(sph_corr, `greenhouse-geisser` =, gg = "GG",
                          `huynh-feldt` =, hf = "HF")

    # ezANOVA stores sphericity tests and correction values in two data frames,
    # which are combined here.
    mauchlys <- left_join(x$`Mauchly's Test for Sphericity`,
                          x$`Sphericity Corrections`, by = "Effect")

    # Checking of significance of Mauchly's test only if user does not want to
    # force sphericity correction for all within factors
    if (!force_sph_corr)
    {
      # Check which effects do not meet the assumption of sphericity
      mauchlys %<>% `[`(.$p < .05, )
    }

    if (nrow(mauchlys) > 0)
    {
      # Apply correction to degrees of freedom
      tbl[match(mauchlys$Effect, tbl$effects), c("df_n", "df_d")] %<>%
        # Multiply df with correction factor (epsilon)
        `*`(mauchlys[[paste0(corr_method, "e")]])

      # Since corrected dfs have decimal places, we need to format these to two
      tbl <-
        tbl %>%
        rowwise() %>%
        # . %% 1 == 0 checks if number has decimal places
        # As of tibble 3.0.0 we need to manually convert all column entries to
        # character, as types are not converted automatically
        mutate_at(c("df_n", "df_d"),
                  \(x) ifelse(x %% 1 == 0, as.character(x),
                              fmt_stat(x, equal_sign = FALSE)))

      # Replace p-values in tbl with corrected ones
      tbl[match(mauchlys$Effect, tbl$effects), "p"] <-
        mauchlys[[paste0("p[", corr_method, "]")]] %>%
        map_chr(fmt_pval)

      # Update significance asterisks
      tbl$symb <-
        tbl$p %>%
        # P-values have already been formatted, so need to workaround that
        map_chr(\(x) {
          if (x == "< .001")
          {
            "***"
          }
          else
          {
            x %>% str_extract("[0-9.]+") %>% as.numeric() %>% p_to_symbol()
          }
        })

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

#' @importFrom magrittr %>% %<>%
#' @importFrom purrr map_chr
#' @importFrom rmarkdown render
#' @importFrom tibble tibble
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

    if (format == "latex")
    {
      text <- map_chr(text, fmt_latex)
    }
    else if (format == "latex_math")
    {
      text <- map_chr(text, fmt_latex_math)
    }
    else if (format == "plotmath")
    {
      return(anova_apa_print_plotmath(tbl, text, effect))
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
        cat(text[which(tbl$effects == effect)])
      }
    }
    # Return as string(s)
    else
    {
      if (is.null(effect))
      {
        tibble(effect = tbl$effects, text = text)
      }
      else
      {
        text[which(tbl$effects == effect)]
      }
    }
  }
}

#' @importFrom tibble tibble
anova_apa_print_default <- function(tbl, effect, es_name)
{
  # Split test statistic and its sign, because the tabular output will be
  # aligned along the test statistic
  sign <- substr(tbl$statistic, 1, 1)
  statistic <- substr(tbl$statistic, 2, nchar(tbl$statistic))

  tbl <- tibble(
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

#' @importFrom magrittr %>%
#' @importFrom purrr map map_dbl
#' @importFrom utils combn
reorder_anova_tbl <- function(x)
{
  # Get names of all main effects
  factors <- grep("[(:]", x$effects, value = TRUE, invert = TRUE)

  # Function for creating names of interaction effects
  concat_fctrs <- function(...) paste(..., collapse = ":")

  new_order <-
    seq_along(factors) %>%
    # Create the new effects order (main effects, two-way interactions, ...)
    map(\(.x) combn(factors, .x, FUN = concat_fctrs, simplify = FALSE)) %>%
    unlist() %>%
    # Add regex for intercept line (if intercept is present in 'x')
    {
      if (any(grepl("(Intercept)", x$effects)))
        c("\\(Intercept\\)", .)
      else
        .
    } %>%
    # Get row index for each effect in old ANOVA table
    map_dbl(\(.x) grep(paste0("^", .x, "$"), x$effects))


  # Apply new order to 'x'
  x[new_order, ]
}
