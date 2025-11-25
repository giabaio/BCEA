# Cost-Effectiveness Acceptability Curve (CEAC) Plot By Graph Device

Choice of base R, ggplot2 or plotly.

## Usage

``` r
ceac_plot_base(he, pos_legend, graph_params, ...)

# S3 method for class 'pairwise'
ceac_plot_base(he, pos_legend, graph_params, ...)

# S3 method for class 'bcea'
ceac_plot_base(he, pos_legend, graph_params, ...)

ceac_plot_ggplot(he, pos_legend, graph_params, ...)

# S3 method for class 'pairwise'
ceac_plot_ggplot(he, pos_legend, graph_params, ...)

# S3 method for class 'bcea'
ceac_plot_ggplot(he, pos_legend, graph_params, ...)

ceac_ggplot(he, pos_legend, graph_params, ceac, ...)

ceac_plot_plotly(he, pos_legend, graph_params, ...)

# S3 method for class 'pairwise'
ceac_plot_plotly(he, pos_legend, graph_params, ...)

# S3 method for class 'bcea'
ceac_plot_plotly(he, pos_legend, graph_params, ...)

ceac_plotly(he, pos_legend = "bottomright", graph_params, ceac, ...)
```

## Arguments

- he:

  A `bcea` object containing the results of the Bayesian modelling and
  the economic evaluation.

- pos_legend:

  Legend position

- graph_params:

  Aesthetic ggplot parameters

- ...:

  Additional arguments

- ceac:

  ceac index in `he`
