---
title: 'BCEA: An R Package for Cost-Effectiveness Analysis'
tags:
  - R
  - HTA
  - health economics
  - cost-effectiveness
authors:
  - name: Nathan Green^[corresponding author] # note this makes a footnote saying 'co-first author'
    orcid: 0000-0003-2745-1736
    affiliation: 1 # (Multiple affiliations must be quoted)
  - name: Anna Heath
    orcid: 0000-0002-7263-4251
    affiliation: "1,2,3"
  - name: Gianluca Baio
    orcid: 0000-0003-4314-2570
    affiliation: 1
affiliations:
 - name: Department of Statistical Science, UCL, Torrington Place, UK.
   index: 1
 - name: Child Health Evaluative Sciences, The Hospital for Sick Children, Toronto, ON, Canada.
   index: 2
 - name: Division of Biostatistics, Dalla Lana School of Public Health, University of Toronto, Toronto, ON, Canada.
   index: 3
date: 11 February 2022
bibliography: paper.bib
---

# Summary

Health economic cost-effectiveness analyses (CEA) consist of analytic approaches for combining costs and health consequences of intervention(s). These help to understand how much an intervention may cost (per unit of health gained) compared to an alternative intervention, such as a control or status quo. For resource allocation, a decision maker may wish to know if an intervention is cost saving, and if not then how much more would it cost to implement it compared to a less effective intervention.

Current guidance for cost-effectiveness analyses advocates the quantification of uncertainties which can be represented by random samples obtained from a probability sensitivity analysis or, more efficiently, a Bayesian model.
The R (@R) package \texttt{BCEA} can be used to post-process the sampled costs and health impacts to perform advanced analyses producing standardised and highly customisable outputs.
\texttt{BCEA} is valuable for statisticians and practitioners working in the field of health economic modelling wanting to simplify and standardise their workflow, for example in the preparation of dossiers in support of marketing authorisation, or academic and scientific publications.

# Statement of need

\texttt{BCEA} is a tool for interpreting and presenting the random sample of results from a CEA in a simple, powerful and standardised way, with useful, technically advanced measures and graphical summaries.
\texttt{BCEA} was primarily written to use posterior distribution samples from a Bayesian model (e.g. run in WinBUGS or Stan) but can take any PSA random samples as inputs. \texttt{BCEA} also aims to be used in a health economic modelling workflow, meaning that it can be plugged-in as one of the steps in a CEA analysis.
\texttt{BCEA} does not provide modelling functionality, like some other CEA packages such as \texttt{hesim} (@Incerti2021) or \texttt{heemod} (@Filipovic-Pierucci2017), but the package philosophy (borrowed from UNIX) is to do one thing well by focusing on the analysis following a model run.
The package \texttt{dampack} is also a decision analytic modelling package which provides a suite of functions for analyzing and visualizing the health economic outputs but takes a different design approach to \texttt{BCEA}.
Although it has a focused scope, within this \texttt{BCEA} is designed to be extensible and flexible. Currently, \texttt{BCEA} has base R, \texttt{ggplot2} and \texttt{plotly} versions of the plotting functions.
The code is written so that computation of new statistics and new plotting functionality can be easily added. In \texttt{BCEA} the workflow centres around the \texttt{bcea()} function rather than separate functions for each type of statistic, with the aim to reduce the learning curve and easily expose the package functionality.
Finally, \texttt{BCEA} has an expansive suite of functions from basic cost-effectiveness analyses, e.g. increment benefit (IB) and ICER calculation and plotting, to more sophisticated methods, e.g. Expected Value of Perfect Partial Information (EVPPI).

The breadth of models used for CEA is wide and growing in complexity and applications (@Krijkamp2018, @Krijkamp2019), but their implementation and, in particular, post-processing of their output can (and should be) standardised (@Alarid-Escudero2019a). This has the benefit of greater reliability, facilitating assessment and reuse. Decoupling the modelling from the post-processing allows for flexibility in the CEA model but, as long as its output is in a standard format, then \texttt{BCEA} can be used. Thus, methodologies in CEA modelling can advance independent of the post-processing and presentation.

For further, in-depth details about \texttt{BCEA} we encourage the package user to consult @Baio:2013 and @Baio2017.


# Acknowledgements

We acknowledge contributions from Andrea Berardi in the development of this project.

# References
