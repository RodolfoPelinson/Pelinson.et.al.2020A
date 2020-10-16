Spatial Autocorrelation Analysis
================
Rodolfo Pelinson
14/10/2020

The goal of pelinson.et.al.2020 is to walk the user through the
statistical analysis presented in:  
**“Pelinson et al 2020. Top predator introduction changes the effects of
spatial isolation on freshwater community structure”**  
DOI: <https://doi.org/10.1101/857318>

You can install the last version of `Pelinson.et.al.2020A` package from
my [GitHub](https://github.com/RodolfoPelinson/pelinson.et.al.2020)
with:

``` r
install.packages("devtools")
devtools::install_github("RodolfoPelinson/Pelinson.et.al.2020A")
library("Pelinson.et.al.2020A")
```

This will give you access to all the data and functions used to produce
the results shown in “Pelinson et al 2020. Top predator introduction
changes the effects of spatial isolation on freshwater community
structure”.

Other packages used here are:  
`adegraphics` version 1.0-15  
`adespatial` version 0.3-8  
`ade4` version 1.7-15  
`mvabund` version 4.1.3

If you have problems with updated versions of those packages, you can
create a project with its own library. Then you update this library with
the packages used in this project at the versions that they were
originally used. To that, you can run this code:

``` r
install.packages("renv") #Only if do not have "renv"
renv::init()
renv::restore(lockfile = "https://raw.githubusercontent.com/RodolfoPelinson/Pelinson.et.al.2020A/master/renv.lock",
              packages = c("vegan","permute","mvabund","gllvm","lme4","emmeans","adegraphics","adespatial","ade4","mvabund"),clean = FALSE)
```

It should update your library with the package versions used in this
project. This may take a while to create the library.

``` r
library(adegraphics)
library(adespatial)
library(ade4)
library(mvabund)
```

## Spatial Autocorrelation Analisis

Due to inevitable practical constraints, one limitation of our
experimental design is that experimental units were not fully spatially
isolated from each other. That is, one might hypothesize that as local
insect populations build up over time and emerge from ponds, they may
affect the local dynamics of neighboring ponds. We, therefore, tested
whether the spatial configuration of the experimental ponds could
explain the community structures we observed at the end of our
experiment (i.e. last survey). To do so, we computed artificial spatial
variables using distance-based Moran’s Eigenvector Maps (MEMs).

Loading necessary data.

``` r
data(coord)
data(com_SS3)
com_SS3_mvabund <- mvabund(com_SS3)
data(isolation_SS3)
data(fish_SS3)
```

First we have to define a threshold distance to truncate the distance
matrix. We chose 60m as threshold. Thus I measured the distance between
ponds A1 and A2, which was 28 meters, to find how much 60 meters mean in
our xy coordinate matrix.

``` r
DistA1_A2_meters <- 28
DistA1_A2_decimals <- dist(coord)[1]
dist_60m <- (60*DistA1_A2_decimals)/28
dist_60m
```

    ## [1] 0.0005808447

Now we create the distance based Moran Eigenvector Maps (dbMEMs):

``` r
dbMEM_exp1 <- dbmem(dist(coord), silent = F, thresh = dist_60m)
```

    ## User-provided truncation threshold = 0.0005808447 
    ## Time to compute dbMEMs = 0.000000  sec

``` r
rownames(dbMEM_exp1) <- rownames(coord)
```

We can plot our ponds and see wich ones were considered conected with a
60 meter truncation distance:

``` r
adegraphics::s.label(coord, nb = attr(dbMEM_exp1, "listw"), xax = 2, yax = 1, plabels = list(cex = 1, boxes = list(col = "grey90")), 
                     ylim = c(-22.8092,-22.8045), xlim = c(-49.1908, -49.1870))
```

![](Spatial-Autocorrelation-Analysis_files/figure-gfm/plotting%20ponds-1.png)<!-- -->

We endded up with 4 MEMs. We can plot then to see what patterns each one
account for:

MEM1

``` r
sr_value(coord, dbMEM_exp1[,1], ylim = c(-22.81,-22.804), xlim = c(-49.19057, -49.18725), grid=T, csize = 0.8, clegend = 1.5, xax = 2, yax = 1, method = "bubble")
```

![](Spatial-Autocorrelation-Analysis_files/figure-gfm/MEM1-1.png)<!-- -->

MEM2

``` r
sr_value(coord, dbMEM_exp1[,2], ylim = c(-22.81,-22.804), xlim = c(-49.19057, -49.18725), grid=T, csize = 0.8, clegend = 1.5, xax = 2, yax = 1, method = "bubble")
```

![](Spatial-Autocorrelation-Analysis_files/figure-gfm/MEM2-1.png)<!-- -->

MEM3

``` r
sr_value(coord, dbMEM_exp1[,3], ylim = c(-22.81,-22.804), xlim = c(-49.19057, -49.18725), grid=T, csize = 0.8, clegend = 1.5, xax = 2, yax = 1, method = "bubble")
```

![](Spatial-Autocorrelation-Analysis_files/figure-gfm/MEM3-1.png)<!-- -->

MEM4

``` r
sr_value(coord, dbMEM_exp1[,4], ylim = c(-22.81,-22.804), xlim = c(-49.19057, -49.18725), grid=T, csize = 0.8, clegend = 1.5, xax = 2, yax = 1, method = "bubble")
```

![](Spatial-Autocorrelation-Analysis_files/figure-gfm/MEM4-1.png)<!-- -->

Before analyzing the data, we remove the ponds for wich we do not have
community data:

``` r
dbMEM_exp1 <- dbMEM_exp1[which(rownames(coord) != "A4" & rownames(coord) != "B3" & rownames(coord) != "C3" & rownames(coord) != "C4"),]
```

Now, first, lets see wich MEMs can significantly explain community
patterns doing Likelihood Ratio Tests

``` r
fit_SS3_MEMs_only <- manyglm(com_SS3_mvabund ~  dbMEM_exp1$MEM1 +  dbMEM_exp1$MEM2 +  dbMEM_exp1$MEM3 +  dbMEM_exp1$MEM4 , family = "negative.binomial")
set.seed(1);anova_MEMs_only <- anova(fit_SS3_MEMs_only , nBoot = 10000,  p.uni  = "none", test = "LR")
```

    ## Time elapsed: 0 hr 7 min 28 sec

``` r
anova_MEMs_only
```

    ## Analysis of Deviance Table
    ## 
    ## Model: com_SS3_mvabund ~ dbMEM_exp1$MEM1 + dbMEM_exp1$MEM2 + dbMEM_exp1$MEM3 + dbMEM_exp1$MEM4
    ## 
    ## Multivariate test:
    ##                 Res.Df Df.diff   Dev Pr(>Dev)  
    ## (Intercept)         19                         
    ## dbMEM_exp1$MEM1     18       1 40.49    0.042 *
    ## dbMEM_exp1$MEM2     17       1 19.87    0.525  
    ## dbMEM_exp1$MEM3     16       1 25.72    0.345  
    ## dbMEM_exp1$MEM4     15       1 37.38    0.218  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Arguments:
    ##  Test statistics calculated assuming uncorrelated response (for faster computation) 
    ##  P-value calculated using 10000 iterations via PIT-trap resampling.

It seems that MEM1 is important\! However, if we look at the figure
showing the MEM1, it seems to capture spatial patterns that are caused
by our isolation treatments. Lets see if it remains important after
accounting for effect of treatments:

``` r
fit_SS3_no_effect <- manyglm(com_SS3_mvabund ~  1, family = "negative.binomial")
fit_SS3_fish <- manyglm(com_SS3_mvabund ~  fish_SS3, family = "negative.binomial")
fit_SS3_isolation <- manyglm(com_SS3_mvabund ~  fish_SS3 + isolation_SS3, family = "negative.binomial")
fit_SS3_interaction <- manyglm(com_SS3_mvabund ~  fish_SS3 * isolation_SS3, family = "negative.binomial")
fit_SS3_interaction_MEM1 <- manyglm(com_SS3_mvabund ~  (fish_SS3*isolation_SS3) + dbMEM_exp1$MEM1, family = "negative.binomial")
fit_SS3_interaction_MEM2 <- manyglm(com_SS3_mvabund ~  (fish_SS3*isolation_SS3) + dbMEM_exp1$MEM1+ dbMEM_exp1$MEM2, family = "negative.binomial")
fit_SS3_interaction_MEM3 <- manyglm(com_SS3_mvabund ~  (fish_SS3*isolation_SS3) + dbMEM_exp1$MEM1+ dbMEM_exp1$MEM2+ dbMEM_exp1$MEM3, family = "negative.binomial")
fit_SS3_interaction_MEM4 <- manyglm(com_SS3_mvabund ~  (fish_SS3*isolation_SS3) + dbMEM_exp1$MEM1+ dbMEM_exp1$MEM2+ dbMEM_exp1$MEM3+ dbMEM_exp1$MEM4, family = "negative.binomial")


set.seed(1);anova_treatments_MEMs <- anova(fit_SS3_no_effect,
                                           fit_SS3_fish,
                                           fit_SS3_isolation,
                                           fit_SS3_interaction,
                                           fit_SS3_interaction_MEM1,
                                           fit_SS3_interaction_MEM2,
                                           fit_SS3_interaction_MEM3,
                                           fit_SS3_interaction_MEM4,
                                           nBoot = 10000,  p.uni  = "none", test = "LR")
```

    ## Time elapsed: 0 hr 17 min 8 sec

``` r
anova_treatments_MEMs
```

    ## Analysis of Deviance Table
    ## 
    ## fit_SS3_no_effect: com_SS3_mvabund ~ 1
    ## fit_SS3_fish: com_SS3_mvabund ~ fish_SS3
    ## fit_SS3_isolation: com_SS3_mvabund ~ fish_SS3 + isolation_SS3
    ## fit_SS3_interaction: com_SS3_mvabund ~ fish_SS3 * isolation_SS3
    ## fit_SS3_interaction_MEM1: com_SS3_mvabund ~ (fish_SS3 * isolation_SS3) + dbMEM_exp1$MEM1
    ## fit_SS3_interaction_MEM2: com_SS3_mvabund ~ (fish_SS3 * isolation_SS3) + dbMEM_exp1$MEM1 + dbMEM_exp1$MEM2
    ## fit_SS3_interaction_MEM3: com_SS3_mvabund ~ (fish_SS3 * isolation_SS3) + dbMEM_exp1$MEM1 + dbMEM_exp1$MEM2 + dbMEM_exp1$MEM3
    ## fit_SS3_interaction_MEM4: com_SS3_mvabund ~ (fish_SS3 * isolation_SS3) + dbMEM_exp1$MEM1 + dbMEM_exp1$MEM2 + dbMEM_exp1$MEM3 + dbMEM_exp1$MEM4
    ## 
    ## Multivariate test:
    ##                          Res.Df Df.diff   Dev Pr(>Dev)  
    ## fit_SS3_no_effect            19                         
    ## fit_SS3_fish                 18       1 49.09    0.018 *
    ## fit_SS3_isolation            16       2 72.96    0.054 .
    ## fit_SS3_interaction          14       2 91.12    0.015 *
    ## fit_SS3_interaction_MEM1     13       1 39.43    0.143  
    ## fit_SS3_interaction_MEM2     12       1 22.54    0.717  
    ## fit_SS3_interaction_MEM3     11       1 31.73    0.352  
    ## fit_SS3_interaction_MEM4     10       1 62.40    0.100 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Arguments:
    ##  Test statistics calculated assuming uncorrelated response (for faster computation) 
    ##  P-value calculated using 10000 iterations via PIT-trap resampling.

It seems that after accounting for the effect of presence of fish and
isolation, all of the MEMs are not important in explaining community
patterns.
