# apa 0.3.4.9000

## Bug fixes

* Fix error in `cohen_d` with Hedge's g correction not applying to one-sample
  t-tests. (@spressi, #15)
* Add `one_sample` argument to `cohens_d_` to specify if Cohen's d is requested
  for if providing t and n.
* Fix a missing escape for percent sign in the documentation of `t_apa`.

# apa 0.3.4

## Bug fixes

* Fix spacing error in `t_apa` output for `format = "latex_math"` with 
  confidence interval for Cohen's d. (@yannikstegmann)
* Fix problem with formula interface for `t_test.formula` and `cohens_d.formula`
  with r-devel.

# apa 0.3.3

## New features

* Add option to force sphericity correction on all within factors in ANOVA or
  turn of sphericity correction completely.
* Add option to display confidence interval for pearson correlation.
* Add option to display condidence interval for Cohen's d (experimental).

## Bug fixes

* Add missing backslash for chi-square in LaTeX format.
* Fix error in one sample `cohens_d` if input is from `t_test`.
* Fix error that was introduced by tibble 3.0.0 (old code assumed automatic type
  conversion)

# apa 0.3.2

## Bug fixes

* Fix a test that returned a wrong result in r-devel (t-test now returns a list
  with more elements).

# apa 0.3.1

## Bug fixes

* Fix a bug in `t_test` when the independent variable has unused factor levels.
* Fix a test that assumed no empty groups present (needed for dplyr 0.8    
  compatibility)

# apa 0.3.0

## New features

* Add LaTeX math output format (#3)

## Bug fixes and minor improvements

* Fix error in `anova_apa` when specifying the `effect` argument
* Fix printing of p-values if p = 1.
* Add missing `else` in `anova_apa`. (@stegmannks, #6)
* Fix error in calculation of sample size from degrees of freedom in Cohen's d
  for dependent samples (@lcreteig, #7)

# apa 0.2.0

## New features

* Add support for `aov` in `anova_apa`.

## Bug fixes and minor improvements

* Fix bug when using abbreviations "pes" or "ges" in `anova_apa`.
* Provide same order of effects in `anova_apa` independent of input object
* In `anova_apa` significance asterisks might have been incorrect when p-values 
  were corrected for violation of sphericity.
