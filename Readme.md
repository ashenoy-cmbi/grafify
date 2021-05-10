
### Installation

To install this package use the following steps. You’ll need the package
`remotes` installed first, skip to the second step if you already have
it.

``` r
install.packages("remotes") #install remotes
remotes::install_github("ashenoy-cmbi/grafify@*release", dependencies = T) #install latest release with dependencies
```

`grafify` depends on `ggplot2`, `lmerTest`, `emmeans`, `car`,
`pbkrtest`, `tidyr`, `purrr` and `Hmisc` packages.

### Motivation

<img src="grafify.png" width="150px" />

The main goals of this package, besides making it easier for me to share
data and functions for the statistics workshop, are the following:

1.  easily make common types of graphs based on `ggplot2` but with fewer
    lines of code
2.  carry out ANOVA analysis using linear models (`lm`) and mixed
    effects (`lmer`)
3.  perform post-hoc comparisons (through `emmeans`)
4.  simulate one-way and two-way ANOVA design data for power analysis

If you’re interested in basic theory and code for stats in R, also visit
Statistics for [Micro/Immuno
Biologists](https://microimmunostats.netlify.app).

### **Latest version: v1.4.1**

### Features

`grafify` has four main kinds of functions as follows.

1.  There are 19 `plot_` functions of 6 broad types in `grafify`. The
    `plot_scatter..` versions are preferred when there are many data
    points, `plot_dot..` versions have a “cleaner” layout for smaller
    datasets.

    1.  Two categorical variables: these graphs either use scatter (or
        also called jitter) or dot plot geometries:
        `plot_scatterbar_sd`, `plot_scatterbox`, `plot_scatterviolin`
        and `plot_dotbar_sd`, `plot_dotbox`, `plot_dotviolin`
    2.  Three or four categorical variables: `plot_3d_scatterbar`,
        `plot_3d_scatterbox`, `plot_4d_scatterbar`, `plot_4d_scatterbox`
    3.  Quantitative X-Y & a third variable: `plot_xy_NumGroup`,
        `plot_xy_CatGroup`
    4.  Matched before-after graphs: `plot_befafter_colours`,
        `plot_befafter_shapes`
    5.  Data distributions: `plot_qqline`, `plot_density`
        `plot_histogram`
    6.  Summary graphs with SD error bars: `plot_bar_sd`,
        `plot_point_sd`

<img src="all_graphsv1.4.1.jpg" width="90%" />

The following qualitative and continuous palettes are implemented in
`grafify`:

<img src="grafify_palettesv0.2.0.jpg" width="90%" />

(The continuous colour scheme based on Paul Tol’s [YlOrBl
variant](https://personal.sron.nl/~pault/#sec:sequential) is new in
v0.2.0.)

1.  Fitting linear models and linear mixed models and obtaining ANOVA
    tables

    1.  linear models for ordinary ANOVAs: `simple_anova`,
        `simple_model`,
    2.  linear mixed effects ANOVAs: `mixed_anova`, `mixed_model`

2.  Perform post-hoc comparisons based on fitted models

    1.  `posthoc_Pariwise`
    2.  `posthoc_Levelwise`
    3.  `posthoc_vsRef`

3.  Generating random one-way and two-way data based on mean and SD.

    1.  one-way designs: `make_1way_data`, `make_1way_rb_data`
    2.  two-way designs: `make_2way_data`, `make_2way_rb_data`

4.  Colour-blind compatible schemes:

    1.  `okabe_ito` (see Mike Mol’s
        [blog](https://mikemol.github.io/technique/colorblind/2018/02/11/color-safe-palette.html)).
    2.  `bright`, `pale`, `muted`, `dark`, `light`, `vibrant`, and
        `contrast` colours (see Paul Tol’s
        [blog](https://personal.sron.nl/~pault/#sec:qualitative)).

### Vignettes

Visit the `grafify` [vignettes](https://grafify-vignettes.netlify.app)
website for detailed examples of usage.

### Release notes

Full reference to all functions available at [`grafify`
pages](https://ashenoy-cmbi.github.io/grafify/index.html).

#### New in v1.4.1

This version “breaks” a few arguments from v0.3.1, therefore is v1.4.1.
Specifically, opacity for both symbols and bars/boxes/violins can be set
using `s_alpha` and `b_alpha` or `v_alpha`, respectively; previously,
only bars/boxes/violin opacity could be set with a single `alpha`
parameter. Old code with just `alpha` will no longer work, sorry! There
are also new graph types and arguments for ANOVAs as below.

1.  New graph types

    1.  `plot_density` and `plot_histogram` for smooth density or
        histogram plots through `geom_density` and `geom_histogram`
        respectively.
    2.  two new plot types `plot_scatterbox` and `plot_scatterviolin`
        that complement the `plot_dot...` versions and instead use
        `geom_point` with `position_jitter`. These versions are useful
        when a large number of data points are needed to be plotted.

2.  Updates

    1.  `simple_anova` where the table also has Mean SS.
    2.  `mixed_anova` now has two new arguments, one to change method
        for Df calculation and second to get type I or III SS (default
        is type II).
    3.  `jitter` argument added to `plot_3d..` and `plot_4d..` functions
        for consistency with other scatter plots.
    4.  `bwid` argument (for adjusting width of bars) added to
        `plot_scatterbar_sd` for consistency.

#### New in v0.3.1

Bug fixes in `mixed_model` and `simple_model` which now correctly lists
the data used in the call field.

#### New in v0.3.0

1.  A new `plot_4d_scatterbar` function which is like
    `plot_4d_scatterbox` but plots bar and SD. So there are now two
    `plot_3d_` and `plot_4d_` functions.
2.  Text on X-axis on all graphs can be rotated from 0-90 using
    `TextXAngle` argument to prevent overlap.
3.  `plot_dot_` functions now have `dotthick` option to set stroke
    thickness. This is similar to `symthick` for scatter/jitter plots.
4.  Using `facet_wrap` or `facet_grid` will not draw a box around panel
    text (unlike the default in `theme_classic()`).
5.  `plot_3d_` and `plot_4d_` functions draw symbols in black colour.

#### Fixed in v0.2.1:

1.  Bug fixes in `plot_3d_scatterbar` and `plot_3d_scatterbox`, which
    now correctly use the “shapes” variable to fill colour of bars/boxes
    and shape of the symbols; symbols are depicted in black.
2.  `simple_anova` generates type II ANOVA table through `car::Anova()`,
    so the `car` package is now a dependency. v0.1.0 and v0.2.0
    generated type I ANOVA table through `stats::anova()`.

#### New in v0.2.0:

1.  the main difference from v0.1.0 is that all `plot_` functions apply
    the `all_grafify` colour scheme by default (see `plot_` vignettes on
    how to change colours)
2.  two new types of graphs are possible with two quantitative X-Y plots
    with a third variable that is either numeric (`plot_xy_NumGroup`) or
    categorical (`plot_xy_CatGroup`).
3.  there are two new continuous colour schemes (`scale_fill_grafify_c`
    and `scale_colour_grafify_c`), based on [Paul Tol’s
    variant](https://personal.sron.nl/~pault/#sec:sequential) of YlOrBl
    scheme.
