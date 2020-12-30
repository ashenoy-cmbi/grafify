
### Installation

To install this package use the following steps. You’ll need the package
`remotes` installed first, skip to the second step if you already have
it.

``` r
install.packages("remotes") #install remotes
remotes::install_github("ashenoy-cmbi/grafify", dependencies = T) #install with dependencies
```

This package requires `dplyr`, `purrr`, `ggplot2`, `lmerTest`, `emmeans`
and `Hmisc`. Additionally, `cowplot` and `colorblindr` are suggested
packages for making pretty graphs.

### Introduction

<img src="grafify_pptx_small.png" width="150px" />

The main goals of this package are to make it easier to share data and
functions for the statistics workshop, and the following: 1. enable easy
grafs based on `ggplot2` 2. carry out ANOVA analysis using linear models
and mixed effects 3. perform post-hoc comparisons using `emmeans` 4.
make simple one-way and two-way ANOVA design data

This package has four main kinds of functions as follows.

1.  Making graphs easily using 12 `plot_` functions of 5 broad types
    
    1.  using two variables: `plot_scatterbar_sd`, `plot_dotbar_sd`,
        `plot_dotbox`, `plot_dotviolin`
    2.  using three or four variables: `plot_3d_scatterbar`,
        `plot_3d_scatterbox`, `plot_4d_scatterbox`
    3.  before-after graphs of matched data: `plot_befafter_colours`,
        `plot_befafter_shapes`
    4.  QQ plot to check distribution: `plot_qqline`
    5.  summary graphs with SD error bars: `plot_bar_sd`,
        `plot_point_sd`

2.  Fitting linear models and linear mixed models and obtaining ANOVA
    tables
    
    1.  linear models for ordinary ANOVAs: `simple_anova`,
        `simple_model`, `simple_mod_summary`,
    2.  linear mixed effects ANOVAs: `mixed_anova`, `mixed_model`,
        `mixed_mod_summary`

3.  Perform post-hoc comparisons based on fitted models
    
    1.  `posthoc_Pariwise`
    2.  `posthoc_Levelwise`
    3.  `posthoc_vsRef`

4.  Generating random one-way and two-way data based on mean and SD.
    
    1.  one-way designs: `make_1way_data`, `make_1way_rb_data`
    2.  two-way designs: `make_2way_data`, `make_2way_rb_data`

### Vignettes

Visit the `grafify` [vignettes](https://grafify-vignettes.netlify.app)
website for detailed examples of usage.
