# apa 0.3.2.9000

## Bug fixes

* Add missing backslash for chi-square in LaTeX format.
* Fix error in one sample `cohens_d` if input is from `t_test`.

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
