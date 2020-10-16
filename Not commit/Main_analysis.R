#Required packages
library(mvabund)
library(vegan)
library(gllvm)

#Loading data
source("Loading data.R")

#Loading auxiliary functions
source("Dif_dist.R")
source("My_coefplot.R")
source("plot_null.R")
source("plot_ordination.R")


##### Running full models####

com_incomplete_mvabund <- mvabund(com_incomplete)

#Mean-variance plot log scale
meanvar.plot(com_incomplete_mvabund ~ SS_incomplete*(fish_incomplete*isolation_incomplete), table =T, pch = 16) 
abline(a = 0, b = 1, lwd = 2)
box(lwd = 2)

#Mean-variance plot
meanvar.plot(com_incomplete_mvabund ~ SS_incomplete*(fish_incomplete*isolation_incomplete), table =T, log = "", xlim= c(0,50), ylim= c(0,500), pch = 16) 
abline(a = 0, b = 1, lwd = 2)
box(lwd = 2)

#ploting residuals for both Negative binomial and Poisson distributions
fit_incosistent_interaction_NB <- manyglm(com_incomplete_mvabund ~ SS_incomplete * (fish_incomplete * isolation_incomplete), family = "negative.binomial", cor.type = "I")
fit_incosistent_interaction_POIS <- manyglm(com_incomplete_mvabund ~ SS_incomplete * (fish_incomplete * isolation_incomplete), family = "poisson", cor.type = "I")

resid_POIS <- residuals.manyglm(fit_incosistent_interaction_POIS); resid_POIS[which(resid_POIS == Inf)] <- NA
qqnorm(resid_POIS)
qqline(resid_POIS,col="red")

resid_NB <- residuals.manyglm(fit_incosistent_interaction_NB)
qqnorm(resid_NB)
qqline(resid_NB,col="red")

####################################################



#Permutation matrix
set.seed(3);control <- how(within = Within(type = 'free'),
               plots = Plots(strata = ID_incomplete, type = 'free'),
               nperm = 10000)
permutations <- shuffleSet(nrow(com_incomplete), control = control)
permutations


#Runing the models
fit_no_effect <- manyglm(com_incomplete_mvabund ~ 1, family = "negative.binomial", cor.type = "I")
fit_Time <- manyglm(com_incomplete_mvabund ~ SS_incomplete, family = "negative.binomial", cor.type = "I")
fit_Time_fish <- manyglm(com_incomplete_mvabund ~ SS_incomplete + fish_incomplete, family = "negative.binomial", cor.type = "I")
fit_Time_fish_isolation <- manyglm(com_incomplete_mvabund ~ SS_incomplete + fish_incomplete + isolation_incomplete, family = "negative.binomial", cor.type = "I")
fit_Time_interaction <- manyglm(com_incomplete_mvabund ~ SS_incomplete + (fish_incomplete * isolation_incomplete), family = "negative.binomial", cor.type = "I")
fit_incosistent_interaction <- manyglm(com_incomplete_mvabund ~ SS_incomplete * (fish_incomplete * isolation_incomplete), family = "negative.binomial", cor.type = "I")

#Runing the Likelihood ratio tests
anova_incomplete <- anova.manyglm(fit_no_effect,
                                  fit_Time,
                                  fit_Time_fish,
                                  fit_Time_fish_isolation,
                                  fit_Time_interaction,
                                  fit_incosistent_interaction, bootID = permutations ,  test = "LR", resamp = "pit.trap")


anova_incomplete2 <- anova.manyglm(fit_incosistent_interaction, bootID = permutations ,  test = "LR", resamp = "pit.trap")



##########models separeted by Sampling survey#####

#SS1
com_SS1_mvabund <- mvabund(com_SS1)

fit_SS1_no_effect <- manyglm(com_SS1_mvabund ~  1, family = "negative.binomial")
fit_SS1_fish <- manyglm(com_SS1_mvabund ~  fish_SS1, family = "negative.binomial")
fit_SS1_isolation <- manyglm(com_SS1_mvabund ~  isolation_SS1, family = "negative.binomial")
fit_SS1_interaction <- manyglm(com_SS1_mvabund ~  fish_SS1*isolation_SS1, family = "negative.binomial")

set.seed(3);anova_SS1_fish <- anova(fit_SS1_no_effect, fit_SS1_fish, nBoot = 10000,  p.uni  = "none", test = "LR")
anova_SS1_fish
set.seed(3);anova_SS1_isolation <- anova(fit_SS1_no_effect, fit_SS1_isolation, nBoot = 10000,  p.uni  = "none", test = "LR")
anova_SS1_isolation
set.seed(3);anova_SS1_interaction <- anova(fit_SS1_no_effect, fit_SS1_interaction, nBoot = 10000,  p.uni  = "none", test = "LR")
anova_SS1_interaction

set.seed(3);anova_SS1 <- anova(fit_SS1_interaction, nBoot = 10000,  p.uni  = "none", test = "LR")


fit_SS1_interaction
  
fit_SS1_no_effect$deviance - fit_SS1_fish$deviance


#Is the effect of treatments mediated by trophic level?
fit_SS1_no_trait_interaction <- traitglm(L = com_SS1, R = env_SS1, Q = TRAITS_SS1, formula = ~ isolation_SS1 * fish_SS1, method = "manyglm", col.intercepts = T)
fit_SS1_trait_pred_interaction <- traitglm(L = com_SS1, R = env_SS1, Q = TRAITS_SS1, formula = ~ (isolation_SS1:trophic) * (fish_SS1:trophic), method = "manyglm", col.intercepts = T)
fit_SS1_trait_pred_volume_interaction <- traitglm(L = com_SS1, R = env_SS1, Q = TRAITS_SS1, formula = ~ (isolation_SS1:trophic + isolation_SS1:volume_log) * (fish_SS1:trophic + fish_SS1:volume_log), method = "manyglm", col.intercepts = T)

set.seed(3);anova_SS1_trait_interaction <- anova.traitglm(fit_SS1_no_trait_interaction,
                                              fit_SS1_trait_pred_interaction,
                                              #fit_SS1_trait_pred_volume_interaction,
                                              nBoot = 10000, test = "LR")
anova_SS1_trait_interaction



######Testing for diferences in effect of fish across distances####
set.seed(3);Dif_dist_SS1<- Dif_dist(com = com_SS1, x1 = fish_SS1,x2 = isolation_SS1,nperm = 10000, family = "negative.binomial", num.lv = 2,
                                    strata = T, show.perm = T, orig_n.init = 20, perm_n.init = 1, type = "centroid", method = "VA", refit_perm = F)
Dif_dist_SS1




#SS2
com_SS2_mvabund <- mvabund(com_SS2)

fit_SS2_no_effect <- manyglm(com_SS2_mvabund ~  1, family = "negative.binomial")
fit_SS2_fish <- manyglm(com_SS2_mvabund ~  fish_SS2, family = "negative.binomial")
fit_SS2_fish_isolation <- manyglm(com_SS2_mvabund ~  fish_SS2+isolation_SS2, family = "negative.binomial")
fit_SS2_interaction <- manyglm(com_SS2_mvabund ~  fish_SS2*isolation_SS2, family = "negative.binomial")

set.seed(3);anova_SS2_fish <- anova(fit_SS2_no_effect, fit_SS2_fish, nBoot = 10000,  p.uni  = "none", test = "LR")
anova_SS2_fish
set.seed(3);anova_SS2_fish_isolation <- anova(fit_SS2_fish, fit_SS2_fish_isolation, nBoot = 10000,  p.uni  = "none", test = "LR")
anova_SS2_fish_isolation
set.seed(3);anova_SS2_interaction <- anova(fit_SS2_fish_isolation, fit_SS2_interaction, nBoot = 10000,  p.uni  = "none", test = "LR")
anova_SS2_interaction

set.seed(3);anova_SS2 <- anova(fit_SS2_interaction, nBoot = 10000,  p.uni  = "none", test = "LR")



#Is the effect of treatments mediated by trophic level?

data.frame(colnames(com_SS3), TRAITS_SS3$trophic)

fit_SS2_no_trait_interaction <- traitglm(L = com_SS2, R = env_SS2, Q = TRAITS_SS2, formula = ~ isolation_SS2 * fish_SS2, method = "manyglm", col.intercepts = T)
fit_SS2_trait_pred_interaction <- traitglm(L = com_SS2, R = env_SS2, Q = TRAITS_SS2, formula = ~ (isolation_SS2:trophic) * (fish_SS2:trophic), method = "manyglm", col.intercepts = T)
fit_SS2_trait_pred_volume_interaction <- traitglm(L = com_SS2, R = env_SS2, Q = TRAITS_SS2, formula = ~ (isolation_SS2:trophic + isolation_SS2:volume_log) * (fish_SS2:trophic + fish_SS2:volume_log), method = "manyglm", col.intercepts = T)

set.seed(3);anova_SS2_trait_interaction <- anova.traitglm(fit_SS2_no_trait_interaction,
                                              fit_SS2_trait_pred_interaction,
                                              #fit_SS2_trait_pred_volume_interaction,
                                              nBoot = 10000, test = "LR")

fit_SS2_trait_pred_interaction
######Testing for diferences in effect of fish across distances####
set.seed(3);Dif_dist_SS1_2<- Dif_dist2(com = com_SS1, x1 = fish_SS1,x2 = isolation_SS1,nperm = 10000, family = "negative.binomial", num.lv = 2,
                                       strata = T, show.perm = T, orig_n.init = 20, perm_n.init = 1, type = "centroid", method = "VA", refit_perm = F)
Dif_dist_SS1_2




#SS3
com_SS3_mvabund <- mvabund(com_SS3)

fit_SS3_no_effect <- manyglm(com_SS3_mvabund ~  1, family = "negative.binomial")
fit_SS3_fish <- manyglm(com_SS3_mvabund ~  fish_SS3, family = "negative.binomial")
fit_SS3_fish_isolation <- manyglm(com_SS3_mvabund ~  fish_SS3+isolation_SS3, family = "negative.binomial")
fit_SS3_interaction <- manyglm(com_SS3_mvabund ~  fish_SS3*isolation_SS3, family = "negative.binomial")

set.seed(3);anova_SS3_fish <- anova(fit_SS3_no_effect, fit_SS3_fish, nBoot = 10000,  p.uni  = "none", test = "LR")
anova_SS3_fish
set.seed(3);anova_SS3_fish_isolation <- anova(fit_SS3_fish, fit_SS3_fish_isolation, nBoot = 10000,  p.uni  = "none", test = "LR")
anova_SS3_fish_isolation
set.seed(3);anova_SS3_interaction <- anova(fit_SS3_fish, fit_SS3_interaction, nBoot = 10000,  p.uni  = "none", test = "LR")
anova_SS3_interaction

set.seed(3);anova_SS3 <- anova(fit_SS3_interaction, nBoot = 10000,  p.uni  = "none", test = "LR")


data.frame(fish_SS3, isolation_SS3)

fit_SS3_no_trait_interaction <- traitglm(L = com_SS3, R = env_SS3, formula = ~ isolation_SS3 * fish_SS3, method = "manyglm", col.intercepts = T)
fit_SS3_trait_pred_interaction <- traitglm(L = com_SS3, R = env_SS3, Q = TRAITS_SS3, formula = ~ (isolation_SS3:trophic) * (fish_SS3:trophic), method = "manyglm", col.intercepts = T)
fit_SS3_trait_pred_volume_interaction <- traitglm(L = com_SS3, R = env_SS3, Q = TRAITS_SS3, formula = ~ (isolation_SS3:trophic + isolation_SS3:volume_log) * (fish_SS3:trophic + fish_SS3:volume_log), method = "manyglm", col.intercepts = T)

set.seed(3);anova_SS3_trait_interaction <- anova(fit_SS3_no_trait_interaction,
                                              fit_SS3_trait_pred_interaction,
                                              #fit_SS3_trait_pred_volume_interaction,
                                              nBoot = 10000, test = "LR")




######Testing for diferences in effect of fish across distances####
#RNGkind(sample.kind = "Rounding")
set.seed(3)

#Excluding samples for balanced design
first <- sample(row(com_SS3)[which(fish_SS3=="absent" & isolation_SS3 == "30")], size = 1)
second <- sample(row(com_SS3)[which(fish_SS3=="absent" & isolation_SS3 == "120")], size = 1)
com_SS3_incomplete <- com_SS3[-c(first,second),]
fish_SS3_incomplete <- fish_SS3[-c(first,second)]
isolation_SS3_incomplete <- isolation_SS3[-c(first,second)]

set.seed(3);Dif_dist_SS3<- Dif_dist(com = com_SS3_incomplete, x1 = fish_SS3_incomplete,x2 = isolation_SS3_incomplete,nperm = 10000, family = "negative.binomial", num.lv = 2,
                                    strata = T, show.perm = T, orig_n.init = 20, perm_n.init = 1, type = "centroid", method = "VA", refit_perm = F)

Dif_dist_SS3




########GLLVM#####

##All SS together
fit_gllvm_com<- gllvm(com,family = "negative.binomial", method = "VA", row.eff = F,n.init = 50, seed = 3, num.lv = 2)

#SS1
fit_gllvm_no_effect_SS1<- gllvm(com_SS1,family = "negative.binomial", method = "VA", row.eff = F,n.init = 50, seed = 3, num.lv = 2)

#SS2
fit_gllvm_no_effect_SS2<- gllvm(com_SS2,family = "negative.binomial", method = "VA", row.eff = F,n.init = 50, seed = 3, num.lv = 2)

#SS3
fit_gllvm_no_effect_SS3<- gllvm(com_SS3,family = "negative.binomial", method = "VA", row.eff = F,n.init = 50, seed = 3, num.lv = 2)



#This would be what a null cenario whould look like
permutations <- shuffleSet(nrow(com_SS3_incomplete),
                           control = how(within = Within(type = 'free'),plots = Plots(strata = fish_SS3_incomplete, type = 'free'),
                                         nperm = 1))

example_original_SS3_incomplete <- gllvm(com_SS3_incomplete,family = "negative.binomial", method = "VA", row.eff = F,n.init = 10, seed = 3, num.lv = 2)
example_NULL_SS3_incomplete <- gllvm(com_SS3_incomplete[permutations,],family = "negative.binomial", method = "VA", row.eff = F,n.init = 50, seed = 1, num.lv = 2)


par(mfrow = c(4,4))

plot_null(com_SS3_incomplete, nperm = 15 ,x1 = fish_SS3_incomplete, x2 = isolation_SS3_incomplete,
          family = "negative.binomial", strata = T, orig_n.init = 10, xlim = c(-2.5,2.5), ylim = c(-2.5,2.5), 
          refit_perm = F, elipse = T, site_colors = c("sienna1", "steelblue1","sienna3","steelblue3","sienna4", "steelblue4"))

#####








