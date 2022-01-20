
If you use `grafify`, please cite

Shenoy, A. R. (2021) grafify: an R package for easy graphs, ANOVAs and
post-hoc comparisons. Zenodo. <http://doi.org/10.5281/zenodo.5136508>

Latest DOI for all versions:
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5136507.svg)](https://doi.org/10.5281/zenodo.5136507)

### Installation

`grafify` is now on CRAN, can be installed by typing
`install.packages("grafify")`.

Any updates not yet on can will be made available on here first for
which you’ll need the `remotes` package. Type
`remotes::install_github("ashenoy-cmbi/grafify@*release")` to get the
version on GitHub instead.

`grafify` requires the following packages to be installed:
`broom.mixed`, `car`, `emmeans`, `ggplot2`, `Hmisc`, `lme4`, `lmerTest`,
`magrittr`, `pbkrtest`, `purrr`, `stats`, `tidyr`.

### Motivation

I made this package mainly to plot great-looking graphs quickly while
exploring data, and for linear regressions for ANOVA. I also use it to
introduce linear models in my teaching. If you’re interested in basic
theory and code for statistics written for biologists, also visit
Statistics for [Micro/Immuno
Biologists](https://microimmunostats.netlify.app).

### **Latest version: v2.0.0 on CRAN**

### Features

#### Graphs

`grafify` has five main features (graphs, colour blind-friendly colour
schemes, ANOVAs & post-hoc comparisons, practice datasets and data
simulations):

1.  There are 19 `plot_` functions of 6 broad types in `grafify`. The
    `plot_scatter..` versions are preferred when there are many data
    points, `plot_dot..` versions have a “cleaner” layout for smaller
    datasets.

    1.  Two categorical variables: these graphs either use scatter (or
        also called jitter) or dot plot geometries:
        `plot_scatterbar_sd`, `plot_scatterbox`, `plot_scatterviolin`
        and `plot_dotbar_sd`, `plot_dotbox`, `plot_dotviolin` **New
        since v1.5.0**: new `plot_` functions like the above but ending
        in `_sc` for *same colour* or *single colour*. See vignettes for
        details.
    2.  Three or four categorical variables (one-way or two-way ANOVA
        designs): `plot_3d_scatterbar`, `plot_3d_scatterbox`,
        `plot_4d_scatterbar`, `plot_4d_scatterbox`
    3.  Quantitative X & Y, plus a third variable: `plot_xy_NumGroup`,
        `plot_xy_CatGroup`
    4.  Matched before-after graphs: `plot_befafter_colours`,
        `plot_befafter_shapes`
    5.  Data distributions: `plot_qqline`, `plot_density`
        `plot_histogram`, and residuals of linear models with
        `plot_qqmodel` **New since v1.5.0**: `plot_qqmodel` which
        generates a Q-Q plot of model residuals
    6.  Summary graphs with SD error bars: `plot_bar_sd`,
        `plot_point_sd` **New since v1.5.0**: *single colour* versions
        of these available as names ending with `_sc`

<img src="man/figures/all_graphsv1.4.1.jpg" width="90%" />

#### Colourblind-friendly colour schemes

The following discreet (qualitative) and continuous (quantitative)
palettes are implemented in `grafify`:

<img src="man/figures/grafify_palettesv0.2.0.jpg" width="90%" />

(The continuous colour scheme based on Paul Tol’s [YlOrBl
variant](https://personal.sron.nl/~pault/#sec:sequential) is new in
v0.2.0.)

Apply these discrete palettes to any `ggplot2` object with
`scale_fill_grafify`, `scale_colour_grafify`, `scale_fill_grafify2` or
`scale_colour_grafify2`. Palettes available are: `okabe_ito` (see Mike
Mol’s
[blog](https://mikemol.github.io/technique/colorblind/2018/02/11/color-safe-palette.html)),
`bright`, `pale`, `muted`, `dark`, `light`, `vibrant`, and `contrast`
colours (see Paul Tol’s
[blog](https://personal.sron.nl/~pault/#sec:qualitative)).

Apply the `yellow_conti` continuous colour scheme using
`scale_fill_grafify_c` and `scale_colour_grafify_c`.

#### Linear models

Fitting linear models and linear mixed models and obtaining ANOVA tables

1.  linear models for ordinary ANOVAs: `simple_anova`, `simple_model`,
2.  linear mixed effects for repeated-measures and randomised-block
    design ANOVAs: `mixed_anova`, `mixed_model`, `mixed_anova_slopes` &
    `mixed_model_slopes`.

#### Post-hoc comparisons

Perform post-hoc comparisons based on fitted models

1.  `posthoc_Pariwise`
2.  `posthoc_Levelwise`
3.  `posthoc_vsRef`
4.  `posthoc_Trends`

#### Data simulation

Generating random one-way and two-way data based on mean and SD and
residual error.

1.  one-way designs: `make_1way_data`, `make_1way_rb_data`
2.  two-way designs: `make_2way_data`, `make_2way_rb_data`

### Vignettes

The best place to see `grafify` in action is the
[vignettes](https://grafify-vignettes.netlify.app) website, which has
detailed description of all functions.

### Function references

Go to this [website](https://ashenoy-cmbi.github.io/grafify/index.html)
for function documentations.

### Hexsticker

<img src="man/figures/grafify.png" width="150px" />

### Status

<!-- badges: start -->

[![R-CMD-check](https://github.com/ashenoy-cmbi/grafify/workflows/R-CMD-check/badge.svg)](https://github.com/ashenoy-cmbi/grafify/actions)
