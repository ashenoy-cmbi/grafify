
### Installation

To install this package use the following steps. You’ll need the package
`remotes` installed first, skip to the second step if you already have
it.

``` r
install.packages("remotes") #install remotes
remotes::install_github("ashenoy-cmbi/grafify@*release", dependencies = T) #install latest release with dependencies
```

This package requires `dplyr`, `purrr`, `ggplot2`, `lmerTest`, `emmeans`
and `Hmisc`. Additionally, `cowplot` and `colorblindr` are suggested
packages for making pretty graphs.

### Motivation

<img src="grafify_pptx_small.png" width="150px" />

The main goals of this package, besides making it easier for me to share
data and functions for the statistics workshop, are the following: 1.
easily make common types of grafs based on `ggplot2` but with fewer
lines of code 2. carry out ANOVA analysis using linear models and mixed
effects 3. perform different kinds of post-hoc comparisons 4. simulate
simple one-way and two-way ANOVA design data

### **Latest version: v0.2.0**

New in v0.2.0:

1.  the main difference from v0.1.0 is that all `plot_` functions apply
    the `all_grafify` colour scheme by default (see `plot_` vignettes on
    how to change colours)
2.  two new types of grafs are possible with two quantitative X-Y plots
    with a third variable that is either numeric (`plot_xy_NumGroup`) or
    categorical (`plot_xy_CatGroup`).
3.  there are two new continuous colour schemes (`scale_fill_grafify_c`
    and `scale_colour_grafify_c`), based on [Paul Tol’s
    variant](https://personal.sron.nl/~pault/#sec:sequential) of YlOrBl
    scheme.

### Features

`grafify` has four main kinds of functions as follows.

1.  Making graphs easily using 14 `plot_` functions of 5 broad types
    
    1.  using two variables: `plot_scatterbar_sd`, `plot_dotbar_sd`,
        `plot_dotbox`, `plot_dotviolin`
    2.  using three or four variables: `plot_3d_scatterbar`,
        `plot_3d_scatterbox`, `plot_4d_scatterbox`
    3.  using 3 variables where 2 are quantiative: `plot_xy_NumGroup`,
        `plot_xy_CatGroup` (new in v0.2.0)
    4.  before-after graphs of matched data: `plot_befafter_colours`,
        `plot_befafter_shapes`
    5.  QQ plot to check distribution: `plot_qqline`
    6.  summary graphs with SD error bars: `plot_bar_sd`,
        `plot_point_sd`

<img src="all_graphsv0.2.0.jpg" width="90%" />

The following qualitative and continuous palettes are implemented in
`grafify`:

<img src="grafify_palettesv0.2.0.jpg" width="90%" />

(The continuous colour scheme based on Paul Tol’s [YlOrBl
variant](https://personal.sron.nl/~pault/#sec:sequential) is new in
v0.2.0.)

2.  Fitting linear models and linear mixed models and obtaining ANOVA
    tables
    
    1.  linear models for ordinary ANOVAs: `simple_anova`,
        `simple_model`,
    2.  linear mixed effects ANOVAs: `mixed_anova`, `mixed_model`

3.  Perform post-hoc comparisons based on fitted models
    
    1.  `posthoc_Pariwise`
    2.  `posthoc_Levelwise`
    3.  `posthoc_vsRef`

4.  Generating random one-way and two-way data based on mean and SD.
    
    1.  one-way designs: `make_1way_data`, `make_1way_rb_data`
    2.  two-way designs: `make_2way_data`, `make_2way_rb_data`

5.  Colour-blind compatible schemes:
    
    1.  `okabe_ito` (see Mike Mol’s
        [blog](https://mikemol.github.io/technique/colorblind/2018/02/11/color-safe-palette.html)).
    2.  `bright`, `pale`, `muted`, `dark`, `light`, `vibrant`, and
        `contrast` colours (see Paul Tol’s
        [blog](https://personal.sron.nl/~pault/#sec:qualitative)).

### Vignettes

Visit the `grafify` [vignettes](https://grafify-vignettes.netlify.app)
website for detailed examples of usage.

### Companion website

If you’re interested in basic stats in R, also visit Statistics for
[Micro/Immuno Biologists](https://microimmunostats.netlify.app).
