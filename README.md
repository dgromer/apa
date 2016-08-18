# apa

apa's functions format output of statistical tests according to guidelines of the APA (American Psychological Association), ready to copy-and-paste into manuscripts.

The idea of such formatters was introduced in the [schoRsch package](https://cran.r-project.org/package=schoRsch/). apa generalizes this idea by providing formatters for different output formats (text, Markdown, RMarkdown, HTML, LaTeX, docx and R's plotmath syntax).

Currently available formatters are:

-   `anova_apa()`<sup>2</sup>
-   `chisq_apa()`
-   `cor_apa()`
-   `t_apa()`

Further miscellaneous functions:

-   `apa()`: A wrapper around the `*_apa()`-functions for use in inline code in RMarkdown documents.
-   `cohens_d()` / `cohens_d_()`: Calculate Cohen's d effect size (from raw data, t-test or statistical parameters). Also supports Hedge's g* and Glass's &Delta;.
-   `t_test`: A wrapper around `t.test()` that includes the original data in its return list (in order to calculate the effect size in `cohens_d()` and `t_apa()` directly from the data).

<sup>1</sup> [pandoc](http://pandoc.org/) is required for docx output and needs to be installed manually when not using RStudio (which ships pandoc).

<sup>2</sup> Supports input from `ezANOVA()` from the [ez package](https://cran.r-project.org/package=ez) and `aov_ez()` / `aov_car()` / `aov_4()` from the [afex package](https://cran.r-project.org/package=afex).

## Installation

The development version can be installed using:

```r
# install.packages("devtools")
devtools::install_github("dgromer/apa")
```

## Related approaches

-   [schoRsch](https://cran.r-project.org/package=schoRsch/)
-   [papaja](https://github.com/crsh/papaja)
