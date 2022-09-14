---
title: 'apa: Format output of statistical tests in R according to APA guidelines'
tags:
  - r
  - american psychological association
  - anova
  - t-test
  - correlation
  - effect size
authors:
 - name: Daniel Gromer
   orcid: 0000-0002-8619-7478
   affiliation: 1
affiliations:
 - name: University of Würzburg
   index: 1
date: 14 September 2022
bibliography: paper.bib
---

# Summary

The `apa` R package facilitates the reporting of statistical results in scientific manuscripts by automating the process of formatting test results according to the guidelines of the American Psychological Association (APA). By this, both the amount of work and error-proneness of manually formatting test results according to the guidelines are significantly reduced.

# Statement of need

To facilitate the communication of research findings, it is necessary that results of studies are presented in standardized formats. In psychological research, the APA Publication Manual [@APA_2020] offers guidelines for such standardized reporting. For example, the manual demands results of a t-test to be reported in the format *t*(18) = -1.86, *p* = .079. Statistical software like R and SPSS, however, typically display results of statistical tests in tables or list format. This means that researches have to manually transfer the information into their manuscripts, which is both prone to errors and laborious. The `apa` package for the R programming language [@R_2022] automates this process by formatting results of various statistical tests (ANOVA, t-test, chi-squared test, and correlation test) according to the APA guidelines. Several other R packages, such as `schoRsch` [@Pfister_2016] and `papaja` [@Aust_2022] offer a functionality similar to `apa`. The strengths of the `apa` package are its support for various output formats (e.g., text, Markdown, LaTeX, and Microsoft Word, among others) to automate font formatting (e.g., italics, formulas) and various input options for ANOVAs (i.e., `aov()` from base R, the `aov_*()`-functions from `afex`, @Singmann_2022, and `ezANOVA()` from `ez`, @Lawrence_2016).

# Examples

## t-Test

The code block below visualizes the standard ouput of a t-test in R.

```
> t.test(1:10, y = c(7:20))

	Welch Two Sample t-test

data:  1:10 and c(7:20)
t = -5.4349, df = 21.982, p-value = 1.855e-05
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -11.052802  -4.947198
sample estimates:
mean of x mean of y 
      5.5      13.5 
```

Using `t_apa()`, the output is automatically formatted according to the APA guidelines. Please note that instead of the standard `t.test()` function, `apa`'s `t_test()` is used (which is a simple wrapper around `t.test()` which includes the original data to facilitate the calculation of Cohen's *d* effect size in `t_apa()`). 

```
> t_apa(t_test(1:10, y = c(7:20)))
t(21.98) = -5.43, p < .001, d = -2.13
```

To specify a specific output format (e.g., Markdown), the `format` argument can be used to automate font formatting.

```
> t_apa(t_test(1:10, y = c(7:20)), format = "markdown")
*t*(21.98) = -5.43, *p* < .001, *d* = -2.13
```

Pasting the formatted output into a Markdown file then renders automatically displays *t*(21.98) = -5.43, *p* < .001, *d* = -2.13.

## ANOVA

```
> library(afex)
> data(md_12.1)
> x <- aov_ez(id = "id", dv = "rt", data = md_12.1,
+             within = c("angle", "noise"))
> anova_apa(x)
       Effect                                              
1 (Intercept)  F(1, 9) = 598.45, p < .001, petasq = .99 ***
2       angle F(2, 18) =  40.72, p < .001, petasq = .82 ***
3       noise  F(1, 9) =  33.77, p < .001, petasq = .79 ***
4 angle:noise F(2, 18) =  45.31, p < .001, petasq = .83 ***
Warning message:
In summary.Anova.mlm(object$Anova, multivariate = FALSE) :
  HF eps > 1 treated as 1
```

# References
