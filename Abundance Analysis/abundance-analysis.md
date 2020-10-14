The goal of pelinson.et.al.2020 is to walk the user through the
statistical analysis presented in “Pelinson et al 2020. Top predator
introduction changes the effects of spatial isolation on freshwater
community structure”

Installation
------------

You can install the last version of `pelinson.et.al.2020` package from
my [GitHub](https://github.com/RodolfoPelinson/pelinson.et.al.2020)
with:

    #install.packages("devtools")
    #devtools::install_github("RodolfoPelinson/pelinson.et.al.2020")
    library(pelinson.et.al.2020)

Other packages used here are: `lme4` version 1.1-23  
`emmeans` version 1.4.8

    library(lme4)
    library(emmeans)

This will give you access to all the data and functions used to produce
the results shown in “Pelinson et al 2020. Top predator introduction
changes the effects of spatial isolation on freshwater community
structure”.

Testing for differences in abundance
------------------------------------

First, lets load the necessary data.

    data(abundance_predators)
    data(abundance_consumers)
    data(survey)
    data(fish)
    data(isolation)
    data(ID)

This is for testing for differences in the abundance of predatory
insects across all treatments and sampling surveys. We used generalized
linear mixed models with a negative binomial distribution (`glmer.nb`
function from package `lme4`) to fit the models. Then we used `anova`
from package `lme4` to compute likelihood ratio tests.

    `pred_no_effect` <- glmer.nb(abundance_predators~ 1 + (1|ID), data = treatments, control = glmerControl(optimizer = "bobyqa"))
    `pred_survey` <- glmer.nb(abundance_predators~(survey) + (1|ID), data = treatments, control = glmerControl(optimizer = "bobyqa"))
    `pred_fish` <- glmer.nb(abundance_predators~(survey+fish) + (1|ID), data = treatments, control = glmerControl(optimizer = "bobyqa"))
    `pred_isolation` <- glmer.nb(abundance_predators~(survey+fish+isolation) + (1|ID), data = treatments, control = glmerControl(optimizer = "bobyqa"))
    `pred_survey:fish` <- glmer.nb(abundance_predators~(survey + fish + isolation + survey:fish) + (1|ID), data = treatments, control = glmerControl(optimizer = "bobyqa"))
    `pred_survey:isolation` <- glmer.nb(abundance_predators~(survey + fish + isolation + survey:fish + survey:isolation) + (1|ID), data = treatments, control = glmerControl(optimizer = "bobyqa"))
    `pred_fish:isolation` <- glmer.nb(abundance_predators~(survey + fish + isolation + survey:fish + survey:isolation + fish:isolation) + (1|ID), data = treatments, control = glmerControl(optimizer = "bobyqa"))
    `pred_survey:fish:isolation` <- glmer.nb(abundance_predators~(survey + fish + isolation + survey:fish + survey:isolation + fish:isolation + survey:fish:isolation) + (1|ID), data = treatments, control = glmerControl(optimizer = "bobyqa"))

    Anova_predators <- anova(`pred_no_effect`, 
          `pred_survey`,
          `pred_fish`,
          `pred_isolation`,
          `pred_survey:fish`,
          `pred_survey:isolation`,
          `pred_fish:isolation`,
          `pred_survey:fish:isolation`)

    Anova_predators

    ## Data: treatments
    ## Models:
    ## pred_no_effect: abundance_predators ~ 1 + (1 | ID)
    ## pred_survey: abundance_predators ~ (survey) + (1 | ID)
    ## pred_fish: abundance_predators ~ (survey + fish) + (1 | ID)
    ## pred_isolation: abundance_predators ~ (survey + fish + isolation) + (1 | ID)
    ## pred_survey:fish: abundance_predators ~ (survey + fish + isolation + survey:fish) + 
    ## pred_survey:fish:     (1 | ID)
    ## pred_survey:isolation: abundance_predators ~ (survey + fish + isolation + survey:fish + 
    ## pred_survey:isolation:     survey:isolation) + (1 | ID)
    ## pred_fish:isolation: abundance_predators ~ (survey + fish + isolation + survey:fish + 
    ## pred_fish:isolation:     survey:isolation + fish:isolation) + (1 | ID)
    ## pred_survey:fish:isolation: abundance_predators ~ (survey + fish + isolation + survey:fish + 
    ## pred_survey:fish:isolation:     survey:isolation + fish:isolation + survey:fish:isolation) + 
    ## pred_survey:fish:isolation:     (1 | ID)
    ##                            npar    AIC    BIC  logLik deviance   Chisq Df
    ## pred_no_effect                3 607.28 613.94 -300.64   601.28           
    ## pred_survey                   5 549.27 560.37 -269.64   539.27 62.0118  2
    ## pred_fish                     6 538.52 551.84 -263.26   526.52 12.7524  1
    ## pred_isolation                8 523.49 541.25 -253.75   507.49 19.0261  2
    ## pred_survey:fish             10 522.47 544.66 -251.23   502.47  5.0279  2
    ## pred_survey:isolation        14 526.25 557.32 -249.12   498.25  4.2168  4
    ## pred_fish:isolation          16 525.10 560.61 -246.55   493.10  5.1532  2
    ## pred_survey:fish:isolation   20 524.51 568.90 -242.25   484.51  8.5888  4
    ##                            Pr(>Chisq)    
    ## pred_no_effect                           
    ## pred_survey                 3.422e-14 ***
    ## pred_fish                   0.0003556 ***
    ## pred_isolation              7.388e-05 ***
    ## pred_survey:fish            0.0809488 .  
    ## pred_survey:isolation       0.3774649    
    ## pred_fish:isolation         0.0760338 .  
    ## pred_survey:fish:isolation  0.0722409 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Now some *post-hoc* tests using function `emmeans` from package
`emmeans` to identify pairwise differences for the effect of sampling
survey and isolation, separately. *Post-hoc* tests were always applied
to the most complex model to account for the effect of all possible
interactions.

    emmeans(`pred_survey:fish:isolation`, list(pairwise ~ survey), adjust = "sidak") 

    ## NOTE: Results may be misleading due to involvement in interactions

    ## $`emmeans of survey`
    ##  survey emmean    SE  df asymp.LCL asymp.UCL
    ##  1        1.78 0.144 Inf      1.43      2.12
    ##  2        3.24 0.106 Inf      2.98      3.49
    ##  3        3.50 0.116 Inf      3.22      3.77
    ## 
    ## Results are averaged over the levels of: fish, isolation 
    ## Results are given on the log (not the response) scale. 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: sidak method for 3 estimates 
    ## 
    ## $`pairwise differences of survey`
    ##  contrast estimate    SE  df z.ratio p.value
    ##  1 - 2      -1.460 0.147 Inf  -9.949 <.0001 
    ##  1 - 3      -1.721 0.154 Inf -11.145 <.0001 
    ##  2 - 3      -0.261 0.119 Inf  -2.194 0.0824 
    ## 
    ## Results are averaged over the levels of: fish, isolation 
    ## Results are given on the log (not the response) scale. 
    ## P value adjustment: sidak method for 3 tests

    emmeans(`pred_survey:fish:isolation`, list(pairwise ~ isolation), adjust = "sidak") 

    ## NOTE: Results may be misleading due to involvement in interactions

    ## $`emmeans of isolation`
    ##  isolation emmean    SE  df asymp.LCL asymp.UCL
    ##  30          3.54 0.146 Inf      3.20      3.89
    ##  120         2.75 0.156 Inf      2.38      3.13
    ##  480         2.21 0.175 Inf      1.80      2.63
    ## 
    ## Results are averaged over the levels of: survey, fish 
    ## Results are given on the log (not the response) scale. 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: sidak method for 3 estimates 
    ## 
    ## $`pairwise differences of isolation`
    ##  contrast  estimate    SE  df z.ratio p.value
    ##  30 - 120     0.791 0.213 Inf 3.706   0.0006 
    ##  30 - 480     1.330 0.228 Inf 5.831   <.0001 
    ##  120 - 480    0.539 0.234 Inf 2.303   0.0625 
    ## 
    ## Results are averaged over the levels of: survey, fish 
    ## Results are given on the log (not the response) scale. 
    ## P value adjustment: sidak method for 3 tests

Same thing now for herbivores and detritivores.

    `cons_no_effect` <- glmer.nb(abundance_consumers~ 1 + (1|ID), data = treatments, control = glmerControl(optimizer = "bobyqa"))
    `cons_survey` <- glmer.nb(abundance_consumers~(survey) + (1|ID), data = treatments, control = glmerControl(optimizer = "bobyqa"))
    `cons_fish` <- glmer.nb(abundance_consumers~(survey+fish) + (1|ID), data = treatments, control = glmerControl(optimizer = "bobyqa"))
    `cons_isolation` <- glmer.nb(abundance_consumers~(survey+fish+isolation) + (1|ID), data = treatments, control = glmerControl(optimizer = "bobyqa"))
    `cons_survey:fish` <- glmer.nb(abundance_consumers~(survey + fish + isolation + survey:fish) + (1|ID), data = treatments, control = glmerControl(optimizer = "bobyqa"))
    `cons_survey:isolation` <- glmer.nb(abundance_consumers~(survey + fish + isolation + survey:fish + survey:isolation) + (1|ID), data = treatments, control = glmerControl(optimizer = "bobyqa"))
    `cons_fish:isolation` <- glmer.nb(abundance_consumers~(survey + fish + isolation + survey:fish + survey:isolation + fish:isolation) + (1|ID), data = treatments, control = glmerControl(optimizer = "bobyqa"))
    `cons_survey:fish:isolation` <- glmer.nb(abundance_consumers~(survey + fish + isolation + survey:fish + survey:isolation + fish:isolation + survey:fish:isolation) + (1|ID), data = treatments, control = glmerControl(optimizer = "bobyqa"))

    Anova_consumers <- anova(`cons_no_effect`, 
          `cons_survey`,
          `cons_fish`,
          `cons_isolation`,
          `cons_survey:fish`,
          `cons_survey:isolation`,
          `cons_fish:isolation`,
          `cons_survey:fish:isolation`)
    Anova_consumers

    ## Data: treatments
    ## Models:
    ## cons_no_effect: abundance_consumers ~ 1 + (1 | ID)
    ## cons_survey: abundance_consumers ~ (survey) + (1 | ID)
    ## cons_fish: abundance_consumers ~ (survey + fish) + (1 | ID)
    ## cons_isolation: abundance_consumers ~ (survey + fish + isolation) + (1 | ID)
    ## cons_survey:fish: abundance_consumers ~ (survey + fish + isolation + survey:fish) + 
    ## cons_survey:fish:     (1 | ID)
    ## cons_survey:isolation: abundance_consumers ~ (survey + fish + isolation + survey:fish + 
    ## cons_survey:isolation:     survey:isolation) + (1 | ID)
    ## cons_fish:isolation: abundance_consumers ~ (survey + fish + isolation + survey:fish + 
    ## cons_fish:isolation:     survey:isolation + fish:isolation) + (1 | ID)
    ## cons_survey:fish:isolation: abundance_consumers ~ (survey + fish + isolation + survey:fish + 
    ## cons_survey:fish:isolation:     survey:isolation + fish:isolation + survey:fish:isolation) + 
    ## cons_survey:fish:isolation:     (1 | ID)
    ##                            npar    AIC    BIC  logLik deviance   Chisq Df
    ## cons_no_effect                3 843.83 850.49 -418.91   837.83           
    ## cons_survey                   5 777.47 788.56 -383.73   767.47 70.3627  2
    ## cons_fish                     6 779.16 792.47 -383.58   767.16  0.3095  1
    ## cons_isolation                8 780.63 798.39 -382.32   764.63  2.5255  2
    ## cons_survey:fish             10 781.80 804.00 -380.90   761.80  2.8277  2
    ## cons_survey:isolation        14 778.60 809.67 -375.30   750.60 11.2050  4
    ## cons_fish:isolation          16 776.49 812.00 -372.24   744.49  6.1095  2
    ## cons_survey:fish:isolation   20 780.85 825.24 -370.42   740.85  3.6436  4
    ##                            Pr(>Chisq)    
    ## cons_no_effect                           
    ## cons_survey                 5.259e-16 ***
    ## cons_fish                     0.57797    
    ## cons_isolation                0.28287    
    ## cons_survey:fish              0.24321    
    ## cons_survey:isolation         0.02435 *  
    ## cons_fish:isolation           0.04713 *  
    ## cons_survey:fish:isolation    0.45638    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Now *post-hoc* tests to identify pairwise differences for the effect of
sampling survey, and interaction between isolation and survey, and
between isolation and presence of fish:

    emmeans(`cons_survey:fish:isolation`, list(pairwise ~ survey), adjust = "sidak") 

    ## NOTE: Results may be misleading due to involvement in interactions

    ## $`emmeans of survey`
    ##  survey emmean    SE  df asymp.LCL asymp.UCL
    ##  1        3.17 0.149 Inf      2.81      3.52
    ##  2        4.87 0.147 Inf      4.52      5.22
    ##  3        5.90 0.178 Inf      5.48      6.33
    ## 
    ## Results are averaged over the levels of: fish, isolation 
    ## Results are given on the log (not the response) scale. 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: sidak method for 3 estimates 
    ## 
    ## $`pairwise differences of survey`
    ##  contrast estimate    SE  df z.ratio p.value
    ##  1 - 2       -1.70 0.198 Inf  -8.616 <.0001 
    ##  1 - 3       -2.74 0.215 Inf -12.731 <.0001 
    ##  2 - 3       -1.03 0.207 Inf  -4.986 <.0001 
    ## 
    ## Results are averaged over the levels of: fish, isolation 
    ## Results are given on the log (not the response) scale. 
    ## P value adjustment: sidak method for 3 tests

    emmeans(`cons_survey:fish:isolation`, list(pairwise ~ isolation|fish), adjust = "sidak") 

    ## NOTE: Results may be misleading due to involvement in interactions

    ## $`emmeans of isolation | fish`
    ## fish = absent:
    ##  isolation emmean    SE  df asymp.LCL asymp.UCL
    ##  30          3.98 0.213 Inf      3.47      4.49
    ##  120         4.99 0.215 Inf      4.48      5.50
    ##  480         4.71 0.219 Inf      4.19      5.24
    ## 
    ## fish = present:
    ##  isolation emmean    SE  df asymp.LCL asymp.UCL
    ##  30          4.85 0.235 Inf      4.29      5.41
    ##  120         4.57 0.236 Inf      4.01      5.13
    ##  480         4.78 0.216 Inf      4.27      5.30
    ## 
    ## Results are averaged over the levels of: survey 
    ## Results are given on the log (not the response) scale. 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: sidak method for 3 estimates 
    ## 
    ## $`pairwise differences of isolation | fish`
    ## fish = absent:
    ##  contrast  estimate    SE  df z.ratio p.value
    ##  30 - 120   -1.0089 0.293 Inf -3.444  0.0017 
    ##  30 - 480   -0.7347 0.301 Inf -2.443  0.0431 
    ##  120 - 480   0.2743 0.300 Inf  0.916  0.7377 
    ## 
    ## fish = present:
    ##  contrast  estimate    SE  df z.ratio p.value
    ##  30 - 120    0.2794 0.306 Inf  0.912  0.7399 
    ##  30 - 480    0.0674 0.312 Inf  0.216  0.9950 
    ##  120 - 480  -0.2120 0.312 Inf -0.679  0.8727 
    ## 
    ## Results are averaged over the levels of: survey 
    ## Results are given on the log (not the response) scale. 
    ## P value adjustment: sidak method for 3 tests

    emmeans(`cons_survey:fish:isolation`, list(pairwise ~ isolation|survey), adjust = "sidak") 

    ## NOTE: Results may be misleading due to involvement in interactions

    ## $`emmeans of isolation | survey`
    ## survey = 1:
    ##  isolation emmean    SE  df asymp.LCL asymp.UCL
    ##  30          2.86 0.254 Inf      2.26      3.47
    ##  120         3.62 0.250 Inf      3.02      4.22
    ##  480         3.02 0.256 Inf      2.40      3.63
    ## 
    ## survey = 2:
    ##  isolation emmean    SE  df asymp.LCL asymp.UCL
    ##  30          4.94 0.242 Inf      4.36      5.51
    ##  120         4.96 0.258 Inf      4.34      5.57
    ##  480         4.72 0.242 Inf      4.14      5.30
    ## 
    ## survey = 3:
    ##  isolation emmean    SE  df asymp.LCL asymp.UCL
    ##  30          5.44 0.307 Inf      4.71      6.18
    ##  120         5.76 0.276 Inf      5.10      6.42
    ##  480         6.51 0.276 Inf      5.85      7.16
    ## 
    ## Results are averaged over the levels of: fish 
    ## Results are given on the log (not the response) scale. 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: sidak method for 3 estimates 
    ## 
    ## $`pairwise differences of isolation | survey`
    ## survey = 1:
    ##  contrast  estimate    SE  df z.ratio p.value
    ##  30 - 120    -0.759 0.356 Inf -2.133  0.0955 
    ##  30 - 480    -0.153 0.360 Inf -0.426  0.9642 
    ##  120 - 480    0.605 0.352 Inf  1.721  0.2346 
    ## 
    ## survey = 2:
    ##  contrast  estimate    SE  df z.ratio p.value
    ##  30 - 120    -0.021 0.346 Inf -0.061  0.9999 
    ##  30 - 480     0.216 0.340 Inf  0.635  0.8931 
    ##  120 - 480    0.237 0.346 Inf  0.684  0.8704 
    ## 
    ## survey = 3:
    ##  contrast  estimate    SE  df z.ratio p.value
    ##  30 - 120    -0.315 0.373 Inf -0.843  0.7833 
    ##  30 - 480    -1.064 0.409 Inf -2.600  0.0277 
    ##  120 - 480   -0.749 0.388 Inf -1.931  0.1521 
    ## 
    ## Results are averaged over the levels of: fish 
    ## Results are given on the log (not the response) scale. 
    ## P value adjustment: sidak method for 3 tests
