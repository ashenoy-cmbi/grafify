#' @title In vitro experiments measuring percentage cell death in three genotypes of cells.
#'
#' @description These data are from in vitro measurements of death of host cells (measured as percentage of total cells) after infection with three different strains of a pathogenic bacterium, from five independent experiments. The three strains are three levels within the fixed factor Genotype. The five independent experiments are levels within the random variable Experiment. These data can be analysed using linear mixed effects modeling. These data are from [Goddard _et al_, Cell Rep, 2019, doi.org/10.1016/j.celrep.2019.03.100](https://www.sciencedirect.com/science/article/pii/S2211124719304474)
#'
#' @format data.frame: 15 obs. of  3 variables.
#'
#' \describe{
#' \item{Experiment}{Experiment - a random factor with 5 levels "Exp_1","Exp_2"...}
#' \item{Genotype}{Genotypes - a fixed factor with 3 levels: "WT","KO_1","KO_2".}
#' \item{Death}{Numerical dependent variable indicating percentage cell death.}
#'}
"data_1w_death"

#' @title Data from two-way ANOVA with randomised block design of treatments of strains of mice.
#'
#' @description Data from Festing, ILAR Journal (2014) 55, 472--476 <doi: 10.1093/ilar/ilu045>. These data are suitable for two-way linear mixed effects modelling.
#' The activity of GST (numerical dependent variable) was measured in 4 strains of mice (levels with the fixed factor Strain) either treated or controls (levels within the fixed factor Treatment). Once mouse each was used in two randomised blocks, which is the random factor (Block).
#'
#' @format data.frame:	16 obs. of  4 variables:
#'
#' \describe{
#' \item{Block}{A random factor with 2 levels "A" and "B".}
#' \item{Treatment}{A fixed factor with 2 levels: "Control" & "Treated"}
#' \item{Strain}{A fixed factor with 4 levels: "129Ola", "A/J", "NIH" & "BALB/C"}
#' \item{GST}{Numerical dependent variable indicating GST activity measurement}
#'}
"data_2w_Festing"

#' @title Matched data from two groups where difference between them is consistent.
#'
#' @description An example dataset for paired difference Student's _t_ test. These are bodyweight (Mass) in grams of same mice left untreated or treated, which are two groups to be compared. The data are in a longtable format, and the two groups are levels within the factor "Condition". The Subject column lists ID of matched mice that were measured without and with treatment. These data are from [Sanchez-Garrido _et al_, Sci Signal, 2018, DOI: 10.1126/scisignal.aat6903](https://www.science.org/doi/10.1126/scisignal.aat6903).
#'
#' @format data.frame:	20 obs. of  3 variables:
#'
#' \describe{
#' \item{Subject}{Factor with 10 levels, denoted by capital letters, representing individuals or subjects.}
#' \item{Condition}{A fixed factor with 2 levels: "Untreated" & "Treated".}
#' \item{Mass}{Numerical dependent variable indicating body mass of mice}
#'}
"data_t_pdiff"

#' @title Matched data from two groups where ratio between them is consistent.
#'
#' @description An example dataset for paired ratio Student's _t_ test. These are Cytokine measurements by ELISA (in ng/ml) from 33 independent in vitro experiments performed on two Genotypes that we want to compare. The data are in a longtable format, and the two groups are levels within the factor "Genotype". The Experiment column lists ID of matched experiments.
#'
#' @format data.frame:	66 obs. of  3 variables:
#'
#' \describe{
#' \item{Genotype}{Factor with 2 levels, representing genotypes to be compared ("WT" & "KO").}
#' \item{Experiment}{A random factor with 33 levels representing independent experiments, denoted as "Exp_1", "Exp_2"...}
#' \item{Cytokine}{Numerical dependent variable indicating cytokine measured by ELISA.}
#'}
"data_t_pratio"

#' @title In vitro measurement of percentage cell death - two-way ANOVA design with repeated measures, and randomised blocks.
#'
#' @description These are measurements of death of infected host cells (as percentage of total cells) upon infection with two strains of bacteria, measured at two time points, in 6 independent experiments.  These data repeated-measures data suitable for two-way linear mixed effects modeling with experiment and subjects as random factors.
#'
#' @format data.frame:	24 obs. of  6 variables:
#'
#' \describe{
#' \item{Experiment}{A random factor with 6 levels "e1", "e2"...}
#' \item{Time}{A fixed factor with 2 levels: "t100" & "t300".}
#' \item{Time2}{A numeric column that allows plotting data on a quantitative "Time" axis. The "Time" column has "factor" type values that should be used for the ANOVA..}
#' \item{Genotype}{A fixed factor with 2 levels that we want to compare "WT" & "KO".}
#' \item{Subject}{A random factor with 12 levels: "s1", "s2"... These are cell culture wells that were measured at two time points, and indicate "subjects" that underwent repeated-measures within each of 6 experiments. Subject IDs for WT and KO are unique and clearly indicate different wells.}
#' \item{PI}{Numerical dependent variable indicating propidium iodide dye uptake as a measure of cell death. These are percentage of dead cells out of total cells plated.}
#'}
"data_2w_Tdeath"

#' @title Doubling time of E.coli measured by 10 students three independent times.
#'
#' @description An example dataset showing measurements of _E. coli_ doubling times (in min) measured by 10 different students in 3 independent experiments each. Note that Experiments are just called Exp1-Exp3 even though Exp1 of any of the students are not connected in anyway - this will confuse R! Data are from [Micro/Immuno Stats](https://microimmunostats.netlify.app)
#'
#' @format tibble:	30 obs. of  3 variables:
#'
#' \describe{
#' \item{Student}{Factor with 10 levels, representing different students.}
#' \item{Experiment}{A factor with 3 levels representing independent experiments.}
#' \item{Doubling_time}{Numerical dependent variable indicating measured doubling time in min.}
#'}
"data_doubling_time"

#' @title Hierarchical data from 25 subjects either treated or not at 5 hospitals - two-way ANOVA design with repeated measures.
#'
#' @description An example dataset on measurements of blood cholesterol levels measured in 5 subjects measured before and after receiving a Drug. Five patients each were recruited at 5 hospitals (a-e), so that there are 25 different subjects (1-25) measured twice.  Data are from [Micro/Immuno Stats](https://microimmunostats.netlify.app)
#'
#' @format tibble:	30 obs. of  3 variables:
#'
#' \describe{
#' \item{Hospital}{Factor with 5 levels (a-e), representing different hospitals where subjects were recruited.}
#' \item{Subject}{A factor with 25 levels denoting individuals on whom measurements were made twice.}
#' \item{Treatment}{A factor with 2 levels indicating when measurements were made, i.e. before and after drug.}
#' \item{Cholesterol}{Numerical dependent variable indicating measured doubling time in min.}
#'}
"data_cholesterol"

