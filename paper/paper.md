---
title: 'apa: Format output of statistical tests in R according to APA guidelines'
tags:
  - anova
  - t-test
  - effect size
authors:
 - name: Daniel Gromer
   orcid: 0000-0003-0872-7098
   affiliation: 1
affiliations:
 - name: University of Würzburg
   index: 1
date: 08 January 2019
bibliography: paper.bib
---

# Summary

Um wissenschaftliche Kommunikation zu erleichtern, ist es notwendig, dass Forschungsergebnisse in einem einheitlichen Format dargeboten und ausgetauscht werden. In der psychologischen Forschung ist das APA Publication Manual eine Guideline welche solch ein einheitliches Format vorschreibt (z. B. für die Darstellung von Tabellen, die Präsentation von Ergebnissen, die Zitierung von Literatur). So fordert das APA Publication Manual beispielsweise, dass ein t-Test immer in der Form *t*(18) = -1.86, *p* = .079 berichtet wird. Statistik Programme wie SPSS oder R geben die Ergebnisse typischerweise in Form einer Tabelle oder eines Textes zurück, sodass sie von Hand in das APA-Format übertragen werden müssen. Das Paket ``apa`` für die R programming language übernimmt diese Aufgabe automatisiert und wandelt Ergebnisse unterschiedlicher Tests (ANOVA, t-Test, chisq-Test, correlation test) automatisch in das vom APA Publication Manual geforderte Format um. Zusätzlich unterstützt das ``apa``-Paket unterschiedliche Ausgabeformate (Aufzählen).

Zusätzlich gibt ``apa`` automatisiert Effektstärken aus, wenn sie für den Test vorhanden sind. Dies wird gefordert, ist bisher aber noch nicht überall in die Praxis umgesetzt worden.

# Examples

#

This is a proof of concept integration between a GitHub [@GitHub] repo and figshare [@figshare] in an effort to get a DOI for a GitHub repository. When a repository is tagged for release on GitHub, Fidgit [@Fidgit] will import the release into figshare thus giving the code bundle a DOI. In a somewhat meta fashion, Fidgit is publishing itself to figshare with DOI 'https://doi.org/10.6084/m9.figshare.828487' [@figshare_archive].

-![Fidgit deposited in figshare.](figshare_article.png)

# References
