# Package index

## Main

- [`BCEA`](https://n8thangreen.github.io/BCEA/reference/BCEA-package.md)
  [`BCEA-package`](https://n8thangreen.github.io/BCEA/reference/BCEA-package.md)
  : BCEA: Bayesian Cost Effectiveness Analysis
- [`bcea()`](https://n8thangreen.github.io/BCEA/reference/bcea.md) :
  Create Bayesian Cost-Effectiveness Analysis Object
- [`new_bcea()`](https://n8thangreen.github.io/BCEA/reference/new_bcea.md)
  : Constructor for bcea

## Setters

Functions for modifying or extending a
[`bcea()`](https://n8thangreen.github.io/BCEA/reference/bcea.md)
analysis.

- [`setComparisons()`](https://n8thangreen.github.io/BCEA/reference/setComparisons.md)
  : Set Comparisons Group
- [`` `setComparisons<-`() ``](https://n8thangreen.github.io/BCEA/reference/setComparisons_assign.md)
  : Set Comparison Group
- [`` `setKmax<-`() ``](https://n8thangreen.github.io/BCEA/reference/setKmax_assign.md)
  : Set Maximum Willingness to Pay
- [`` `setReferenceGroup<-`() ``](https://n8thangreen.github.io/BCEA/reference/setReferenceGroup_assign.md)
  : Set Reference Group
- [`` `mixedAn<-`() ``](https://n8thangreen.github.io/BCEA/reference/mixedAn-set.md)
  : Cost-Effectiveness Analysis When Multiple (Possibly
  Non-Cost-Effective) Interventions are Present on the Market
- [`` `CEriskav<-`() ``](https://n8thangreen.github.io/BCEA/reference/CEriskav_assign.md)
  : Cost-effectiveness Analysis Including a Parameter of Risk Aversion
- [`multi.ce(`*`<bcea>`*`)`](https://n8thangreen.github.io/BCEA/reference/multi.ce.md)
  : Cost-effectiveness Analysis With Multiple Comparison
- [`struct.psa()`](https://n8thangreen.github.io/BCEA/reference/struct.psa.md)
  : Structural Probability Sensitivity Analysis

## Plotting

Functions for plotting a wide variety of cost-effectiveness plots in
base R, ggplot2 and plotly.

- [`ceac.plot()`](https://n8thangreen.github.io/BCEA/reference/ceac.plot.md)
  : Cost-Effectiveness Acceptability Curve (CEAC) Plot
- [`ceaf.plot()`](https://n8thangreen.github.io/BCEA/reference/ceaf.plot.md)
  : Cost-Effectiveness Acceptability Frontier (CEAF) plot
- [`ceef.plot()`](https://n8thangreen.github.io/BCEA/reference/ceef.plot.md)
  : Cost-Effectiveness Efficiency Frontier (CEEF) Plot
- [`ceef_plot_ggplot()`](https://n8thangreen.github.io/BCEA/reference/ceef_plot_graph.md)
  [`ceef_plot_plotly()`](https://n8thangreen.github.io/BCEA/reference/ceef_plot_graph.md)
  [`ceef_plot_base()`](https://n8thangreen.github.io/BCEA/reference/ceef_plot_graph.md)
  : Cost-effectiveness Efficiency Frontier Plot By Graph Device
- [`ceplane.plot()`](https://n8thangreen.github.io/BCEA/reference/ceplane.plot.md)
  : Cost-effectiveness Plane Plot
- [`ceplane_plot_base()`](https://n8thangreen.github.io/BCEA/reference/ceplane_plot_graph.md)
  [`ceplane_plot_ggplot()`](https://n8thangreen.github.io/BCEA/reference/ceplane_plot_graph.md)
  [`ceplane_plot_plotly()`](https://n8thangreen.github.io/BCEA/reference/ceplane_plot_graph.md)
  : Cost-Effectiveness Plane Plot By Graph Device
- [`contour()`](https://n8thangreen.github.io/BCEA/reference/contour.md)
  : Contour Plots for the Cost-Effectiveness Plane
- [`contour2()`](https://n8thangreen.github.io/BCEA/reference/contour2.md)
  : Specialised CE-plane Contour Plot
- [`contour_base()`](https://n8thangreen.github.io/BCEA/reference/contour_graph.md)
  [`contour_ggplot()`](https://n8thangreen.github.io/BCEA/reference/contour_graph.md)
  [`contour_plotly()`](https://n8thangreen.github.io/BCEA/reference/contour_graph.md)
  : Contour Cost-Effectiveness Plane
- [`eib.plot()`](https://n8thangreen.github.io/BCEA/reference/eib.plot.md)
  : Expected Incremental Benefit (EIB) Plot
- [`eib_plot_base()`](https://n8thangreen.github.io/BCEA/reference/eib_plot_graph.md)
  [`eib_plot_ggplot()`](https://n8thangreen.github.io/BCEA/reference/eib_plot_graph.md)
  [`eib_plot_plotly()`](https://n8thangreen.github.io/BCEA/reference/eib_plot_graph.md)
  : Expected Incremental Benefit Plot By Graph Device
- [`evi.plot()`](https://n8thangreen.github.io/BCEA/reference/evi.plot.md)
  : Expected Value of Information (EVI) Plot
- [`evi.plot(`*`<mixedAn>`*`)`](https://n8thangreen.github.io/BCEA/reference/evi.plot.mixedAn.md)
  : EVI Plot of the Health Economic Analysis For Mixed Analysis
- [`evi_plot_base()`](https://n8thangreen.github.io/BCEA/reference/evi_plot_graph.md)
  [`evi_plot_ggplot()`](https://n8thangreen.github.io/BCEA/reference/evi_plot_graph.md)
  [`evi_plot_plotly()`](https://n8thangreen.github.io/BCEA/reference/evi_plot_graph.md)
  : Expected Value of Information Plot By Graph Device
- [`ib.plot()`](https://n8thangreen.github.io/BCEA/reference/ib.plot.md)
  : Incremental Benefit (IB) Distribution Plot
- [`ib_plot_base()`](https://n8thangreen.github.io/BCEA/reference/ib_plot_graph.md)
  [`ib_plot_ggplot()`](https://n8thangreen.github.io/BCEA/reference/ib_plot_graph.md)
  [`ib_plot_plotly()`](https://n8thangreen.github.io/BCEA/reference/ib_plot_graph.md)
  : IB plot base R version
- [`info.rank()`](https://n8thangreen.github.io/BCEA/reference/info.rank.md)
  : Information-Rank Plot for bcea Class
- [`info_rank_base()`](https://n8thangreen.github.io/BCEA/reference/info_rank_graph.md)
  [`info_rank_ggplot()`](https://n8thangreen.github.io/BCEA/reference/info_rank_graph.md)
  [`info_rank_plotly()`](https://n8thangreen.github.io/BCEA/reference/info_rank_graph.md)
  : Info Rank Plot By Graph Device
- [`plot(`*`<bcea>`*`)`](https://n8thangreen.github.io/BCEA/reference/plot.bcea.md)
  : Summary Plot of the Health Economic Analysis
- [`plot(`*`<CEriskav>`*`)`](https://n8thangreen.github.io/BCEA/reference/plot.CEriskav.md)
  : Plots EIB and EVPI for the Risk Aversion Case
- [`CEriskav_plot_base()`](https://n8thangreen.github.io/BCEA/reference/CEriskav_plot_graph.md)
  [`CEriskav_plot_ggplot()`](https://n8thangreen.github.io/BCEA/reference/CEriskav_plot_graph.md)
  [`CEriskav_plot_plotly()`](https://n8thangreen.github.io/BCEA/reference/CEriskav_plot_graph.md)
  : Cost-effectiveness Plot Including a Parameter of Risk Aversion
- [`plot(`*`<evppi>`*`)`](https://n8thangreen.github.io/BCEA/reference/plot.evppi.md)
  : Plot Expected Value of Partial Information With Respect to a Set of
  Parameters
- [`evppi_plot_base()`](https://n8thangreen.github.io/BCEA/reference/evppi_plot_graph.md)
  [`evppi_plot_ggplot()`](https://n8thangreen.github.io/BCEA/reference/evppi_plot_graph.md)
  : Plot Expected Value of Partial Information With Respect to a Set of
  Parameters

## Statistics

Lower level functions for computing a range of cost-effectiveness
statistics.

- [`best_interv_given_k()`](https://n8thangreen.github.io/BCEA/reference/best_interv_given_k.md)
  : Optimal intervention
- [`compute_CEAC()`](https://n8thangreen.github.io/BCEA/reference/compute_CEAC.md)
  : Compute Cost-Effectiveness Acceptability Curve
- [`compute_EIB()`](https://n8thangreen.github.io/BCEA/reference/compute_EIB.md)
  : Compute Expected Incremental Benefit
- [`compute_EVI()`](https://n8thangreen.github.io/BCEA/reference/compute_EVI.md)
  : Compute Expected Value of Information
- [`compute_IB()`](https://n8thangreen.github.io/BCEA/reference/compute_IB.md)
  : Compute Incremental Benefit
- [`compute_ICER()`](https://n8thangreen.github.io/BCEA/reference/compute_ICER.md)
  : Compute Incremental Cost-Effectiveness Ratio
- [`compute_U()`](https://n8thangreen.github.io/BCEA/reference/compute_U.md)
  : Compute U Statistic
- [`compute_Ubar()`](https://n8thangreen.github.io/BCEA/reference/compute_Ubar.md)
  : Compute NB for mixture of interventions
- [`compute_Ustar()`](https://n8thangreen.github.io/BCEA/reference/compute_Ustar.md)
  : Compute Ustar Statistic
- [`compute_ceaf()`](https://n8thangreen.github.io/BCEA/reference/compute_ceaf.md)
  : Compute Cost-Effectiveness Acceptability Frontier
- [`compute_eib_cri()`](https://n8thangreen.github.io/BCEA/reference/compute_eib_cri.md)
  : Calculate Credible Intervals
- [`compute_kstar()`](https://n8thangreen.github.io/BCEA/reference/compute_kstar.md)
  : Compute k^\*
- [`compute_ol()`](https://n8thangreen.github.io/BCEA/reference/compute_ol.md)
  : Compute Opportunity Loss
- [`compute_p_best_interv()`](https://n8thangreen.github.io/BCEA/reference/compute_p_best_interv.md)
  : Compute Probability Best Intervention
- [`compute_p_optimal_best()`](https://n8thangreen.github.io/BCEA/reference/compute_p_optimal_best.md)
  : Compute Probability Optimal Intervention Best
- [`compute_vi()`](https://n8thangreen.github.io/BCEA/reference/compute_vi.md)
  : Compute Value of Information

## Outputs

Functions for reporting the results of the cost-effectiveness analysis.

- [`sim_table()`](https://n8thangreen.github.io/BCEA/reference/sim_table.md)
  : Table of Simulation Statistics for the Health Economic Model

- [`tabulate_means()`](https://n8thangreen.github.io/BCEA/reference/tabulate_means.md)
  : Calculate Dataset For ICERs From bcea Object

- [`summary(`*`<bcea>`*`)`](https://n8thangreen.github.io/BCEA/reference/summary.bcea.md)
  :

  Summary Method for Objects of Class `bcea`

- [`summary(`*`<mixedAn>`*`)`](https://n8thangreen.github.io/BCEA/reference/summary.mixedAn.md)
  :

  Summary Methods For Objects in the Class `mixedAn` (Mixed Analysis)

- [`summary(`*`<pairwise>`*`)`](https://n8thangreen.github.io/BCEA/reference/summary.pairwise.md)
  :

  Summary Method for Objects of Class `pairwise`

## Miscellaneous

Helper functions and other intermediate computations.

- [`createInputs()`](https://n8thangreen.github.io/BCEA/reference/createInputs.md)
  : Create Inputs for EVPI Calculation
- [`evppi()`](https://n8thangreen.github.io/BCEA/reference/evppi.md) :
  Expected Value of Perfect Partial Information (EVPPI) for Selected
  Parameters
- [`is.bcea()`](https://n8thangreen.github.io/BCEA/reference/is.bcea.md)
  : Check bcea Class
- [`print(`*`<bcea>`*`)`](https://n8thangreen.github.io/BCEA/reference/print.bcea.md)
  : bcea Print Method
- [`select_plot_type()`](https://n8thangreen.github.io/BCEA/reference/select_plot_type.md)
  : Choose Graphical Engine

## Data

- [`Smoking`](https://n8thangreen.github.io/BCEA/reference/Smoking.md)
  [`cost`](https://n8thangreen.github.io/BCEA/reference/Smoking.md)
  [`data`](https://n8thangreen.github.io/BCEA/reference/Smoking.md)
  [`eff`](https://n8thangreen.github.io/BCEA/reference/Smoking.md)
  [`life.years`](https://n8thangreen.github.io/BCEA/reference/Smoking.md)
  [`pi_post`](https://n8thangreen.github.io/BCEA/reference/Smoking.md)
  [`smoking`](https://n8thangreen.github.io/BCEA/reference/Smoking.md)
  [`smoking_output`](https://n8thangreen.github.io/BCEA/reference/Smoking.md)
  [`treats`](https://n8thangreen.github.io/BCEA/reference/Smoking.md) :
  Smoking Cessation Cost-Effectiveness Data
- [`Vaccine`](https://n8thangreen.github.io/BCEA/reference/Vaccine.md)
  [`c.pts`](https://n8thangreen.github.io/BCEA/reference/Vaccine.md)
  [`cost.GP`](https://n8thangreen.github.io/BCEA/reference/Vaccine.md)
  [`cost.hosp`](https://n8thangreen.github.io/BCEA/reference/Vaccine.md)
  [`cost.otc`](https://n8thangreen.github.io/BCEA/reference/Vaccine.md)
  [`cost.time.off`](https://n8thangreen.github.io/BCEA/reference/Vaccine.md)
  [`cost.time.vac`](https://n8thangreen.github.io/BCEA/reference/Vaccine.md)
  [`cost.travel`](https://n8thangreen.github.io/BCEA/reference/Vaccine.md)
  [`cost.trt1`](https://n8thangreen.github.io/BCEA/reference/Vaccine.md)
  [`cost.trt2`](https://n8thangreen.github.io/BCEA/reference/Vaccine.md)
  [`cost.vac`](https://n8thangreen.github.io/BCEA/reference/Vaccine.md)
  [`e.pts`](https://n8thangreen.github.io/BCEA/reference/Vaccine.md)
  [`N`](https://n8thangreen.github.io/BCEA/reference/Vaccine.md)
  [`N.outcomes`](https://n8thangreen.github.io/BCEA/reference/Vaccine.md)
  [`N.resources`](https://n8thangreen.github.io/BCEA/reference/Vaccine.md)
  [`QALYs.adv`](https://n8thangreen.github.io/BCEA/reference/Vaccine.md)
  [`QALYs.death`](https://n8thangreen.github.io/BCEA/reference/Vaccine.md)
  [`QALYs.hosp`](https://n8thangreen.github.io/BCEA/reference/Vaccine.md)
  [`QALYs.inf`](https://n8thangreen.github.io/BCEA/reference/Vaccine.md)
  [`QALYs.pne`](https://n8thangreen.github.io/BCEA/reference/Vaccine.md)
  [`vaccine_mat`](https://n8thangreen.github.io/BCEA/reference/Vaccine.md)
  : Influenza Vaccination Cost-Effectiveness Data
- [`statins_base`](https://n8thangreen.github.io/BCEA/reference/statins_base.md)
  : Statins Evidence-Synthesis Data (Base Model)
- [`statins_HC`](https://n8thangreen.github.io/BCEA/reference/statins_HC.md)
  : Statins Evidence-Synthesis Data (Robust Model)

## Deprecated

- [`make.report()`](https://n8thangreen.github.io/BCEA/reference/BCEA-deprecated.md)
  [`mce.plot()`](https://n8thangreen.github.io/BCEA/reference/BCEA-deprecated.md)
  [`plot.mixedAn()`](https://n8thangreen.github.io/BCEA/reference/BCEA-deprecated.md)
  :

  Deprecated functions in package BCEA.
