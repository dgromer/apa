# Format a statistic, e.g. t, F, mean, standard deviation
fmt_stat <- function(statistic, leading_zero = TRUE, equal_sign = TRUE,
                     negative_values = TRUE)
{
  if (!negative_values && statistic < .01)
  {
    statistic <- "< 0.01"
  }
  else
  {
    statistic <- sprintf("%.2f", statistic)

    if (equal_sign)
    {
      statistic <- paste("=", statistic)
    }
  }

  if (!leading_zero)
  {
    statistic <- sub("0\\.", "\\.", statistic)
  }

  statistic
}

# Format a p-value
fmt_pval <- function(p, equal_sign = TRUE)
{
  if (p < .001)
  {
    "< .001"
  }
  else if (p == 1)
  {
    "> .999"
  }
  else if (equal_sign)
  {
    paste("=", substr(sprintf("%.3f", p), 2, 5))
  }
  else
  {
    substr(sprintf("%.3f", p), 2, 5)
  }
}

# Format an effect size
fmt_es <- function(es, leading_zero = TRUE, equal_sign = TRUE)
{
  if (is.na(es))
  {
    return(ifelse(leading_zero, "=   NA", "=  NA"))
  }

  if (abs(es) < .01)
  {
    es <- "< 0.01"
  }
  else if (equal_sign)
  {
    es <- paste("=", sprintf("%.2f", es))
  }
  else
  {
    es <- sprintf("%.2f", es)
  }

  if (!leading_zero)
  {
    if (es == "= 1.00")
    {
      es <- "> .99"
    }
    else
    {
      es <- sub("0.", ".", es)
    }
  }

  es
}

# Format symbols (e.g. chi-squared, d, F, partial eta-squared)
fmt_symb <- function(x, format)
{
  if (format == "text")
  {
    switch(x,
           "chisq"       = "chi^2",
           "cohens_d"    = "d",
           "F"           = "F",
           "getasq"      = "getasq",
           "glass_delta" = "Delta",
           "hedges_g"    = "g",
           "kendall's"   = "r_tau",
           "p"           = "p",
           "pearson's"   = "r",
           "petasq"      = "petasq",
           "r"           = "r",
           "spearman's"  = "r_s",
           "t"           = "t")
  }
  else if (format == "latex")
  {
    switch(x,
           "chisq"       = "$chi^2$",
           "cohens_d"    = "\\textit{d}",
           "F"           = "\\textit{F}",
           "getasq"      = "$\\eta^2_g$",
           "glass_delta" = "$\\Delta$",
           "hedges_g"    = "\\textit{g}",
           "kendall's"   = "$r_\\tau$",
           "p"           = "\\textit{p}",
           "pearson's"   = "\\textit{r}",
           "petasq"      = "$\\eta^2_p$",
           "r"           = "\\textit{r}",
           "spearman's"  = "$r_s$",
           "t"           = "\\textit{t}")
  }
  else if (format == "markdown")
  {
    switch(x,
           "chisq"       = "*chi^2*",
           "cohens_d"    = "*d*",
           "F"           = "*F*",
           "getasq"      = "*getasq*",
           "glass_delta" = "*Delta*",
           "hedges_g"    = "*g*",
           "kendall's"   = "*r_tau*",
           "p"           = "*p*",
           "pearson's"   = "*r*",
           "petasq"      = "*petasq*",
           "r"           = "*r*",
           "spearman's"  = "*r_s*",
           "t"           = "*t*")
  }
  else if (format == "rmarkdown")
  {
    switch(x,
           "chisq"       = "$\\chi^2$",
           "cohens_d"    = "*d*",
           "F"           = "*F*",
           "getasq"      = "$\\eta^2_g$",
           "glass_delta" = "$\\Delta$",
           "hedges_g"    = "*g*",
           "kendall's"   = "$r_\\tau$",
           "p"           = "*p*",
           "pearson's"   = "*r*",
           "petasq"      = "$\\eta^2_p$",
           "r"           = "*r*",
           "spearman's"  = "$r_s$",
           "t"           = "*t*")
  }
  else if (format == "html")
  {
    switch(x,
           "chisq"       = "<i>&chi;</i><sup>2</sup>",
           "cohens_d"    = "<i>d</i>",
           "F"           = "<i>F</i>",
           "getasq"      = "<i>&eta;<sup>2</sup><sub>g</sub></i>",
           "glass_delta" = "<i>&Delta;</i>",
           "hedges_g"    = "<i>g</i>",
           "kendall's"   = "<i>r<sub>&tau;</sub></i>",
           "p"           = "<i>p</i>",
           "pearson's"   = "<i>r</i>",
           "petasq"      = "<i>&eta;<sup>2</sup><sub>p</sub></i>",
           "r"           = "<i>r</i>",
           "spearman's"  = "<i>r<sub>s</sub></i>",
           "t"           = "<i>t</i>")
  }
  else if (format == "plotmath")
  {
    switch(x,
           "chisq"       = "chi^2",
           "cohens_d"    = "italic('d')",
           "F"           = "italic('F')",
           "getasq"      = "eta[g]^2",
           "glass_delta" = "Delta",
           "hedges_g"    = "italic('g')",
           "kendall's"   = "italic(r)[tau]",
           "p"           = "italic('p')",
           "pearson's"   = "italic('r')",
           "petasq"      = "eta[p]^2",
           "r"           = "italic('r')",
           "spearman's"  = "italic(r)[s]",
           "t"           = "italic('t')")
  }
}

# Format a p-value as symbol (e.g. p = .008 as **)
p_to_symbol <- function(p)
{
  if (is.na(p))
  {
    ""
  }
  else if (p >= .1)
  {
    ""
  }
  else if (p < .1 && p >= .05)
  {
    "."
  }
  else if (p < .05 && p >= .01)
  {
    "*"
  }
  else if (p < .01 && p >= .001)
  {
    "**"
  }
  else if (p < .001)
  {
    "***"
  }
}

# Format character strings for better LaTeX printing (i.e. insert non-breaking
# spaces at appropriate positions)
#' @importFrom magrittr %>%
fmt_latex <- function(x)
{
  x %>%
    # Non-breaking spaces around equal sign, smaller than and greater than
    gsub(" ([<=>]) ", "~\\1~", .) %>%
    # Non-breaking space between degrees of freedom in F-value
    gsub("(\\([0-9]+.*,) ([0-9]+.*\\))", "\\1~\\2", .) %>%
    # Non-breaking spaces if n is displayed in chi^2 parantheses
    gsub("(, n)", ",~n", .)
}

# Convert APA text to an expression in R's plotmath syntax
#' @importFrom stringr str_trim
fmt_plotmath <- function(text, ...)
{
  # Remove significance asterisks if there are any
  text <- str_trim(gsub("\\**", "", text))

  dots <- list(...)

  # Enclose plain text in single quotes and add comma between plotmath syntax
  # and plain text because we are going to put everything in a call to `paste`.
  for (i in seq_along(dots))
  {
    # If it is not the last element to be replaced, add comma before and after
    if (i < length(dots))
    {
      text <- sub(dots[[i]], ", '\\1', ", text)
    }
    else
    {
      text <- sub(dots[[i]], ", '\\1'", text)
    }
  }

  text <- paste0("paste(", text, ")")

  # Create the expression
  parse(text = text)
}
