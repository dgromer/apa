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

The ``apa`` R package facilitates the reporting of statistical results in scientific manuscripts by automating the process of formatting test results according to the guidelines of the American Psychological Association (APA). By this, both the amount of work and error-proneness of manually formatting test results according to the guidelines are significantly reduced.

# Statement of need

To facilitate the communication of research findings, it is necessary that results of studies are presented in standardized formats. In psychological research, the APA Publication Manual [@APA_2020] offers guidelines for such standardized reporting. For example, the manual demands results of a t-test to be reported in the format *t*(18) = -1.86, *p* = .079. Statistical software like R and SPSS, however, typically display results of statistical tests in tables or list format. This means that researches have to manually transfer the information into their manuscripts, which is prone to errors and laborious. The ``apa`` package for the R programming language [@R_2022] automates this process by formatting results of various statistical tests (ANOVA, t-test, chi-squared test, correlation test) according to the APA guidelines. Several other R packages, such as ``schoRsch`` [@Pfister_2016] and ``papaja`` [@Aust_2022] offer a functionality similar to ``apa``. The strength of the ``apa`` package is its support for various output formats (e.g., text, Markdown, LaTeX, and Microsoft Word, among others) to automate font formatting (e.g., italics, formulas).

# Examples

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

Using `t_apa()`, the output is automatically formatted according to the APA guidelines. Please note that instead of the standard `t.test()` function, `apa`'s `t_test()` is used (which is a simple wrapper around `t.test()` which includes the original data to facilitate the calculation of the Cohen's *d* effect size in `t_apa()`). 

```
> library(apa)
> t_apa(t_test(1:10, y = c(7:20)))
t(21.98) = -5.43, p < .001, d = -2.13
```

# References
