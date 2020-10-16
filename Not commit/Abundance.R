library(MASS)
library(car)
library(emmeans)
library(lme4)
source("Loading data.R")


###ALL Surveys

abundance_predators <- rowSums(com[,which(TRAITS$trophic == "Pr")])
abundance_consumers <- rowSums(com[,which(TRAITS$trophic == "Non_Pred")])

`pred_no_effect` <- glmer.nb(abundance_predators~ 1 + (1|ID))
`pred_SS` <- glmer.nb(abundance_predators~(SS) + (1|ID))
`pred_fish` <- glmer.nb(abundance_predators~(SS+fish) + (1|ID))
`pred_isolation` <- glmer.nb(abundance_predators~(SS+fish+isolation) + (1|ID))
`pred_SS:fish` <- glmer.nb(abundance_predators~(SS + fish + isolation + SS:fish) + (1|ID))
`pred_SS:isolation` <- glmer.nb(abundance_predators~(SS + fish + isolation + SS:fish + SS:isolation) + (1|ID))
`pred_fish:isolation` <- glmer.nb(abundance_predators~(SS + fish + isolation + SS:fish + SS:isolation + fish:isolation) + (1|ID))
`pred_SS:fish:isolation` <- glmer.nb(abundance_predators~(SS + fish + isolation + SS:fish + SS:isolation + fish:isolation + SS:fish:isolation) + (1|ID))

Anova_predators <- anova(`pred_no_effect`,
      `pred_SS`,
      `pred_fish`,
      `pred_isolation`,
      `pred_SS:fish`,
      `pred_SS:isolation`,
      `pred_fish:isolation`,
      `pred_SS:fish:isolation`)

summary(Anova_predators)
nobs(`pred_SS:fish:isolation`)
ngrps(`pred_SS:fish:isolation`)

emmeans(mod_ab_all_pred, list(pairwise ~ isolation), adjust = "tukey") #decrease in abundance with isolation (30 # 120=480)
emmeans(mod_ab_all_pred, list(pairwise ~ SS), adjust = "tukey") #increase in abundance with time (1 # 2=3)

?anova.glm


?anova.merMod


`cons_no_effect` <- glmer.nb(abundance_consumers~ 1 + (1|ID), data = treatments)
`cons_SS` <- glmer.nb(abundance_consumers~(SS) + (1|ID))
`cons_fish` <- glmer.nb(abundance_consumers~(SS+fish) + (1|ID))
`cons_isolation` <- glmer.nb(abundance_consumers~(SS+fish+isolation) + (1|ID))
`cons_SS:fish` <- glmer.nb(abundance_consumers~(SS + fish + isolation + SS:fish) + (1|ID))
`cons_SS:isolation` <- glmer.nb(abundance_consumers~(SS + fish + isolation + SS:fish + SS:isolation) + (1|ID))
`cons_fish:isolation` <- glmer.nb(abundance_consumers~(SS + fish + isolation + SS:fish + SS:isolation + fish:isolation) + (1|ID))
`cons_SS:fish:isolation` <- glmer.nb(abundance_consumers~(SS + fish + isolation + SS:fish + SS:isolation + fish:isolation + SS:fish:isolation) + (1|ID))

anova(`cons_no_effect`,
      `cons_SS`,
      `cons_fish`,
      `cons_isolation`,
      `cons_SS:fish`,
      `cons_SS:isolation`,
      `cons_fish:isolation`,
      `cons_SS:fish:isolation`)

emmeans(mod_ab_all_cons, list(pairwise ~ SS), adjust = "tukey") #increase in abundance with time
emmeans(mod_ab_all_cons, list(pairwise ~ isolation|fish), adjust = "tukey") #increase in abundance with isolation, but only for fishless ponds
emmeans(mod_ab_all_cons, list(pairwise ~ isolation|SS), adjust = "tukey") #increase in abundance with isolation, but only in the last survey


###############################################################################
abundance_SS3_predators <- rowSums(com_SS3[,which(TRAITS_SS3$trophic == "Pr")])
abundance_SS3_consumers <- rowSums(com_SS3[,which(TRAITS_SS3$trophic == "Non_Pred")])


svg("Abundance_predators.svg", width = 5, height = 5, pointsize = 13, bg = "transparent")
boxplot(abundance_SS3_predators~fish_isolation_SS3, outline = F, ylab = "Abundance", xlab = "", at = c(1,2,3,5,6,7), lwd = 1.5, ylim = c(0,240), col = "transparent")
mylevels <- levels(fish_isolation_SS3)
levelProportions <- summary(fish_isolation_SS3)/length(fish_isolation_SS3)
col <- c(c("sienna1","sienna3","sienna4"), c("steelblue1","steelblue3","steelblue4"))
#bg <- c(rep("sienna3",3), rep("dodgerblue3",3),rep("sienna3",3), rep("dodgerblue3",3))
pch <- c(15,16,17,15,16,17)
for(i in 1:length(mylevels)){

  x<- c(1,2,3,5,6,7)[i]
  thislevel <- mylevels[i]
  thisvalues <- abundance_SS3_predators[fish_isolation_SS3==thislevel]

  # take the x-axis indices and add a jitter, proportional to the N in each level
  myjitter <- jitter(rep(x, length(thisvalues)), amount=levelProportions[i]/0.8)
  points(myjitter, thisvalues, pch=pch[i], col=col[i] , cex = 1.5, lwd = 3)

}
boxplot(abundance_SS3_predators~fish_isolation_SS3, add = T, col = "transparent", outline = F,at = c(1,2,3,5,6,7), lwd = 1.5)
box(lwd = 2.5)
dev.off()

mod_ab_pred <- glm.nb(abundance_SS3_predators~fish_SS3*isolation_SS3)
Anova(mod_ab_pred, test.statistic = "LR", type = "II")

emmeans(mod_ab_pred, list(pairwise ~ isolation_SS3), adjust = "tukey")



svg("Abundance_consumers.svg", width = 5, height = 5, pointsize = 13, bg = "transparent")
boxplot(abundance_SS3_consumers~fish_isolation_SS3, outline = F, ylab = "Abundance", xlab = "", at = c(1,2,3,5,6,7), lwd = 1.5, ylim = c(0,1300), col = "transparent")
mylevels <- levels(fish_isolation_SS3)
levelProportions <- summary(fish_isolation_SS3)/length(fish_isolation_SS3)
col <- c(c("sienna1","sienna3","sienna4"), c("steelblue1","steelblue3","steelblue4"))
pch <- c(15,16,17,15,16,17)
for(i in 1:length(mylevels)){

  x<- c(1,2,3,5,6,7)[i]
  thislevel <- mylevels[i]
  thisvalues <- abundance_SS3_consumers[fish_isolation_SS3==thislevel]

  # take the x-axis indices and add a jitter, proportional to the N in each level
  myjitter <- jitter(rep(x, length(thisvalues)), amount=levelProportions[i]/0.8)
  points(myjitter, thisvalues, pch=pch[i], col=col[i] , cex = 1.5, lwd = 3)

}
boxplot(abundance_SS3_consumers~fish_isolation_SS3, add = T, col = "transparent", outline = F,at = c(1,2,3,5,6,7), lwd = 1.5)
box(lwd = 2.5)
dev.off()


mod_ab_pred <- glm.nb(abundance_SS3_consumers~fish_SS3*isolation_SS3)
Anova(mod_ab_pred, test.statistic = "LR", type = "II")


isolation_fishless_SS3 <- isolation_SS3[which(fish_SS3 == "absent")]
abundance_SS3_consumers_fishless <- abundance_SS3_consumers[which(fish_SS3 == "absent")]
mod_ab_cons_fishless <- glm.nb(abundance_SS3_consumers_fishless~isolation_fishless_SS3)
Anova(mod_ab_cons_fishless, test.statistic = "LR", type = "II")
emmeans(mod_ab_cons_fishless, list(pairwise ~ isolation_fishless_SS3), adjust = "tukey")


mod_ab_cons_fish <- glm.nb(abundance_SS3_consumers[which(fish_SS3 == "present")]~isolation_SS3[which(fish_SS3 == "present")])
Anova(mod_ab_cons_fish, test.statistic = "LR", type = "II")





boxplot((log(abundance_SS3_predators/abundance_SS3_consumers))~fish_isolation_SS3, outline = F, ylab = "Predator/Prey Ratio", xlab = "", at = c(1,2,3,5,6,7), lwd = 1.5, ylim = c(-6,5))
mylevels <- levels(fish_isolation_SS3)
levelProportions <- summary(fish_isolation_SS3)/length(fish_isolation_SS3)
col <- c(c("sienna1","sienna3","sienna4"), c("steelblue1","steelblue3","steelblue4"))
#bg <- c(rep("sienna3",3), rep("dodgerblue3",3),rep("sienna3",3), rep("dodgerblue3",3))
pch <- c(15,16,17,15,16,17)
for(i in 1:length(mylevels)){

  x<- c(1,2,3,5,6,7)[i]
  thislevel <- mylevels[i]
  thisvalues <- (abundance_SS3_predators/abundance_SS3_consumers)[fish_isolation_SS3==thislevel]

  # take the x-axis indices and add a jitter, proportional to the N in each level
  myjitter <- jitter(rep(x, length(thisvalues)), amount=levelProportions[i]/0.8)
  points(myjitter, thisvalues, pch=pch[i], col=col[i] , cex = 1.5, lwd = 3)

}
boxplot((abundance_SS3_predators/abundance_SS3_consumers)~fish_isolation_SS3, add = T, col = "transparent", outline = F,at = c(1,2,3,5,6,7), lwd = 1.5)
box(lwd = 2.5)

mod_ratio <- lm(log(abundance_SS3_predators/abundance_SS3_consumers)~fish_SS3*isolation_SS3)
Anova(mod_ratio, test.statistic = "F")
