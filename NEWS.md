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
* In `anova_apa` significance asterisks might have been incorrect when p-values were corrected for violation of sphericity.
