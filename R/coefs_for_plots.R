z<-qnorm(0.975, mean = 0, sd = 1)


#  coefitients for effect of TIME####===========================================================================================================================

#Effect of time 1 to 2

fit_Time_1_2 <- manyglm(com_incomplete_mvabund ~ relevel(survey_incomplete, ref = "1"), family = "negative.binomial")

length(survey_incomplete)
dim(com_incomplete_mvabund)

effect_Time_1_2<-fit_Time_1_2$coefficients[2,]
effect_Time_1_2_se <-fit_Time_1_2$stderr.coefficients[2,]
effect_Time_1_2_se[which(effect_Time_1_2_se>100)] <- fit_Time_1_2$stderr.coefficients[1,][which(effect_Time_1_2_se>100)]
fit_Time_1_2 <- manyglm(com_incomplete_mvabund ~ relevel(survey_incomplete, ref = "2"), family = "negative.binomial")
effect_Time_1_2_se[which(effect_Time_1_2_se>100)] <- fit_Time_1_2$stderr.coefficients[1,][which(effect_Time_1_2_se>100)]

effect_Time_1_2[which(effect_Time_1_2_se>100)] <- 0
effect_Time_1_2_se[which(effect_Time_1_2_se>100)] <- 0

effect_Time_1_2
effect_Time_1_2_se

effect_Time_upper_1_2 <- effect_Time_1_2 + (z*effect_Time_1_2_se)
effect_Time_lower_1_2 <- effect_Time_1_2 - (z*effect_Time_1_2_se)



#Effect of time 2 to 3

fit_Time_2_3 <- manyglm(com_incomplete_mvabund ~ relevel(survey_incomplete, ref = "2"), family = "negative.binomial")

effect_Time_2_3<-fit_Time_2_3$coefficients[3,]
effect_Time_2_3_se <-fit_Time_2_3$stderr.coefficients[3,]
effect_Time_2_3_se[which(effect_Time_2_3_se>100)] <- fit_Time_2_3$stderr.coefficients[1,][which(effect_Time_2_3_se>100)]
fit_Time_2_3 <- manyglm(com_incomplete_mvabund ~ relevel(survey_incomplete, ref = "3"), family = "negative.binomial")
effect_Time_2_3_se[which(effect_Time_2_3_se>100)] <- fit_Time_2_3$stderr.coefficients[1,][which(effect_Time_2_3_se>100)]

effect_Time_2_3[which(effect_Time_2_3_se>100)] <- 0
effect_Time_2_3_se[which(effect_Time_2_3_se>100)] <- 0

effect_Time_2_3
effect_Time_2_3_se

effect_Time_upper_2_3 <- effect_Time_2_3 + (z*effect_Time_2_3_se)
effect_Time_lower_2_3 <- effect_Time_2_3 - (z*effect_Time_2_3_se)


#Effect of time 1 to 3

fit_Time_1_3 <- manyglm(com_incomplete_mvabund ~ relevel(survey_incomplete, ref = "1"), family = "negative.binomial")

effect_Time_1_3<-fit_Time_1_3$coefficients[3,]
effect_Time_1_3_se <-fit_Time_1_3$stderr.coefficients[3,]
effect_Time_1_3_se[which(effect_Time_1_3_se>100)] <- fit_Time_1_3$stderr.coefficients[1,][which(effect_Time_1_3_se>100)]
fit_Time_1_3 <- manyglm(com_incomplete_mvabund ~ relevel(survey_incomplete, ref = "3"), family = "negative.binomial")
effect_Time_1_3_se[which(effect_Time_1_3_se>100)] <- fit_Time_1_3$stderr.coefficients[1,][which(effect_Time_1_3_se>100)]

effect_Time_1_3[which(effect_Time_1_3_se>100)] <- 0
effect_Time_1_3_se[which(effect_Time_1_3_se>100)] <- 0

effect_Time_1_3
effect_Time_1_3_se

effect_Time_upper_1_3 <- effect_Time_1_3 + (z*effect_Time_1_3_se)
effect_Time_lower_1_3 <- effect_Time_1_3 - (z*effect_Time_1_3_se)






##SS1

#Effect of fish in 30 m SS1

fit_SS1_fish_effect_30 <- manyglm(com_SS1_mvabund ~ fish_SS1 * relevel(isolation_SS1, ref = "30"), family = "negative.binomial")

fish_effect_SS1_30<-fit_SS1_fish_effect_30$coefficients[2,]
fish_effect_SS1_30_se <-fit_SS1_fish_effect_30$stderr.coefficients[2,]
fish_effect_SS1_30_se[which(fish_effect_SS1_30_se>100)] <- fit_SS1_fish_effect_30$stderr.coefficients[1,][which(fish_effect_SS1_30_se>100)]
fit_SS1_fish_effect_30 <- manyglm(com_SS1_mvabund ~ relevel(fish_SS1, ref = "present") * relevel(isolation_SS1, ref = "30"), family = "negative.binomial")
fish_effect_SS1_30_se[which(fish_effect_SS1_30_se>100)] <- fit_SS1_fish_effect_30$stderr.coefficients[1,][which(fish_effect_SS1_30_se>100)]

fish_effect_SS1_30[which(fish_effect_SS1_30_se>100)] <- 0
fish_effect_SS1_30_se[which(fish_effect_SS1_30_se>100)] <- 0

fish_effect_SS1_30
fish_effect_SS1_30_se

fish_effect_SS1_upper_30 <- fish_effect_SS1_30 + (z*fish_effect_SS1_30_se)
fish_effect_SS1_lower_30 <- fish_effect_SS1_30 - (z*fish_effect_SS1_30_se)


#Effect of fish in 120 m SS1

fit_SS1_fish_effect_120 <- manyglm(com_SS1_mvabund ~ fish_SS1 * relevel(isolation_SS1, ref = "120"), family = "negative.binomial")

fish_effect_SS1_120<-fit_SS1_fish_effect_120$coefficients[2,]
fish_effect_SS1_120_se <-fit_SS1_fish_effect_120$stderr.coefficients[2,]
fish_effect_SS1_120_se[which(fish_effect_SS1_120_se>100)] <- fit_SS1_fish_effect_120$stderr.coefficients[1,][which(fish_effect_SS1_120_se>100)]
fit_SS1_fish_effect_120 <- manyglm(com_SS1_mvabund ~ relevel(fish_SS1, ref = "present") * relevel(isolation_SS1, ref = "120"), family = "negative.binomial")
fish_effect_SS1_120_se[which(fish_effect_SS1_120_se>100)] <- fit_SS1_fish_effect_120$stderr.coefficients[1,][which(fish_effect_SS1_120_se>100)]

fish_effect_SS1_120[which(fish_effect_SS1_120_se>100)] <- 0
fish_effect_SS1_120_se[which(fish_effect_SS1_120_se>100)] <- 0

fish_effect_SS1_120
fish_effect_SS1_120_se

fish_effect_SS1_upper_120 <- fish_effect_SS1_120 + (z*fish_effect_SS1_120_se)
fish_effect_SS1_lower_120 <- fish_effect_SS1_120 - (z*fish_effect_SS1_120_se)


#Effect of fish in 480 m SS1

fit_SS1_fish_effect_480 <- manyglm(com_SS1_mvabund ~ fish_SS1 * relevel(isolation_SS1, ref = "480"), family = "negative.binomial")

fish_effect_SS1_480<-fit_SS1_fish_effect_480$coefficients[2,]
fish_effect_SS1_480_se <-fit_SS1_fish_effect_480$stderr.coefficients[2,]
fish_effect_SS1_480_se[which(fish_effect_SS1_480_se>100)] <- fit_SS1_fish_effect_480$stderr.coefficients[1,][which(fish_effect_SS1_480_se>100)]
fit_SS1_fish_effect_480 <- manyglm(com_SS1_mvabund ~ relevel(fish_SS1, ref = "present") * relevel(isolation_SS1, ref = "480"), family = "negative.binomial")
fish_effect_SS1_480_se[which(fish_effect_SS1_480_se>100)] <- fit_SS1_fish_effect_480$stderr.coefficients[1,][which(fish_effect_SS1_480_se>100)]

fish_effect_SS1_480[which(fish_effect_SS1_480_se>100)] <- 0
fish_effect_SS1_480_se[which(fish_effect_SS1_480_se>100)] <- 0

fish_effect_SS1_480
fish_effect_SS1_480_se

fish_effect_SS1_upper_480 <- fish_effect_SS1_480 + (z*fish_effect_SS1_480_se)
fish_effect_SS1_lower_480 <- fish_effect_SS1_480 - (z*fish_effect_SS1_480_se)



####__________________________________________________________________________________

##SS2

#Effect of fish in 30 m SS2

fit_SS2_fish_effect_30 <- manyglm(com_SS2_mvabund ~ fish_SS2 * relevel(isolation_SS2, ref = "30"), family = "negative.binomial")

fish_effect_SS2_30<-fit_SS2_fish_effect_30$coefficients[2,]
fish_effect_SS2_30_se <-fit_SS2_fish_effect_30$stderr.coefficients[2,]
fish_effect_SS2_30_se[which(fish_effect_SS2_30_se>100)] <- fit_SS2_fish_effect_30$stderr.coefficients[1,][which(fish_effect_SS2_30_se>100)]
fit_SS2_fish_effect_30 <- manyglm(com_SS2_mvabund ~ relevel(fish_SS2, ref = "present") * relevel(isolation_SS2, ref = "30"), family = "negative.binomial")
fish_effect_SS2_30_se[which(fish_effect_SS2_30_se>100)] <- fit_SS2_fish_effect_30$stderr.coefficients[1,][which(fish_effect_SS2_30_se>100)]

fish_effect_SS2_30[which(fish_effect_SS2_30_se>100)] <- 0
fish_effect_SS2_30_se[which(fish_effect_SS2_30_se>100)] <- 0

fish_effect_SS2_30
fish_effect_SS2_30_se

fish_effect_SS2_upper_30 <- fish_effect_SS2_30 + (z*fish_effect_SS2_30_se)
fish_effect_SS2_lower_30 <- fish_effect_SS2_30 - (z*fish_effect_SS2_30_se)


#Effect of fish in 120 m SS2

fit_SS2_fish_effect_120 <- manyglm(com_SS2_mvabund ~ fish_SS2 * relevel(isolation_SS2, ref = "120"), family = "negative.binomial")

fish_effect_SS2_120<-fit_SS2_fish_effect_120$coefficients[2,]
fish_effect_SS2_120_se <-fit_SS2_fish_effect_120$stderr.coefficients[2,]
fish_effect_SS2_120_se[which(fish_effect_SS2_120_se>100)] <- fit_SS2_fish_effect_120$stderr.coefficients[1,][which(fish_effect_SS2_120_se>100)]
fit_SS2_fish_effect_120 <- manyglm(com_SS2_mvabund ~ relevel(fish_SS2, ref = "present") * relevel(isolation_SS2, ref = "120"), family = "negative.binomial")
fish_effect_SS2_120_se[which(fish_effect_SS2_120_se>100)] <- fit_SS2_fish_effect_120$stderr.coefficients[1,][which(fish_effect_SS2_120_se>100)]

fish_effect_SS2_120[which(fish_effect_SS2_120_se>100)] <- 0
fish_effect_SS2_120_se[which(fish_effect_SS2_120_se>100)] <- 0

fish_effect_SS2_120
fish_effect_SS2_120_se

fish_effect_SS2_upper_120 <- fish_effect_SS2_120 + (z*fish_effect_SS2_120_se)
fish_effect_SS2_lower_120 <- fish_effect_SS2_120 - (z*fish_effect_SS2_120_se)


#Effect of fish in 480 m SS2

fit_SS2_fish_effect_480 <- manyglm(com_SS2_mvabund ~ fish_SS2 * relevel(isolation_SS2, ref = "480"), family = "negative.binomial")

fish_effect_SS2_480<-fit_SS2_fish_effect_480$coefficients[2,]
fish_effect_SS2_480_se <-fit_SS2_fish_effect_480$stderr.coefficients[2,]
fish_effect_SS2_480_se[which(fish_effect_SS2_480_se>100)] <- fit_SS2_fish_effect_480$stderr.coefficients[1,][which(fish_effect_SS2_480_se>100)]
fit_SS2_fish_effect_480 <- manyglm(com_SS2_mvabund ~ relevel(fish_SS2, ref = "present") * relevel(isolation_SS2, ref = "480"), family = "negative.binomial")
fish_effect_SS2_480_se[which(fish_effect_SS2_480_se>100)] <- fit_SS2_fish_effect_480$stderr.coefficients[1,][which(fish_effect_SS2_480_se>100)]

fish_effect_SS2_480[which(fish_effect_SS2_480_se>100)] <- 0
fish_effect_SS2_480_se[which(fish_effect_SS2_480_se>100)] <- 0

fish_effect_SS2_480
fish_effect_SS2_480_se

fish_effect_SS2_upper_480 <- fish_effect_SS2_480 + (z*fish_effect_SS2_480_se)
fish_effect_SS2_lower_480 <- fish_effect_SS2_480 - (z*fish_effect_SS2_480_se)



fit_SS2_trait_pred_interaction <- traitglm(L = com_SS2, R = env_SS2, Q = TRAITS_SS2, formula = ~ (relevel(isolation_SS2, ref = "30"):trophic) * (fish_SS2:trophic), method = "manyglm")
trait_fish_30_SS2 <- fit_SS2_trait_pred_interaction$coefficients[(1+dim(TRAITS_SS2)[1]+6):(1+dim(TRAITS_SS2)[1]+6+1)]; names(trait_fish_30_SS2) <- c("Non_Pred", "Pred")
trait_fish_30_SS2_se <- fit_SS2_trait_pred_interaction$stderr.coefficients[(1+dim(TRAITS_SS2)[1]+6):(1+dim(TRAITS_SS2)[1]+6+1)]; names(trait_fish_30_SS2_se) <- c("Non_Pred", "Pred")
trait_fish_30_SS2_lower <- trait_fish_30_SS2 - z*trait_fish_30_SS2_se
trait_fish_30_SS2_upper <- trait_fish_30_SS2 + z*trait_fish_30_SS2_se

fit_SS2_trait_pred_interaction <- traitglm(L = com_SS2, R = env_SS2, Q = TRAITS_SS2, formula = ~ (relevel(isolation_SS2, ref = "120"):trophic) * (fish_SS2:trophic), method = "manyglm")
trait_fish_120_SS2 <- fit_SS2_trait_pred_interaction$coefficients[(1+dim(TRAITS_SS2)[1]+6):(1+dim(TRAITS_SS2)[1]+6+1)]; names(trait_fish_120_SS2) <- c("Non_Pred", "Pred")
trait_fish_120_SS2_se <- fit_SS2_trait_pred_interaction$stderr.coefficients[(1+dim(TRAITS_SS2)[1]+6):(1+dim(TRAITS_SS2)[1]+6+1)]; names(trait_fish_120_SS2_se) <- c("Non_Pred", "Pred")
trait_fish_120_SS2_lower <- trait_fish_120_SS2 - z*trait_fish_120_SS2_se
trait_fish_120_SS2_upper <- trait_fish_120_SS2 + z*trait_fish_120_SS2_se

fit_SS2_trait_pred_interaction <- traitglm(L = com_SS2, R = env_SS2, Q = TRAITS_SS2, formula = ~ (relevel(isolation_SS2, ref = "480"):trophic) * (fish_SS2:trophic), method = "manyglm")
trait_fish_480_SS2  <- fit_SS2_trait_pred_interaction$coefficients[(1+dim(TRAITS_SS2)[1]+6):(1+dim(TRAITS_SS2)[1]+6+1)]; names(trait_fish_480_SS2) <- c("Non_Pred", "Pred")
trait_fish_480_SS2_se <- fit_SS2_trait_pred_interaction$stderr.coefficients[(1+dim(TRAITS_SS2)[1]+6):(1+dim(TRAITS_SS2)[1]+6+1)]; names(trait_fish_480_SS2_se) <- c("Non_Pred", "Pred")
trait_fish_480_SS2_lower <- trait_fish_480_SS2 - z*trait_fish_480_SS2_se
trait_fish_480_SS2_upper <- trait_fish_480_SS2 + z*trait_fish_480_SS2_se


####__________________________________________________________________________________

##SS3

#Effect of fish in 30 m SS3

fit_SS3_fish_effect_30 <- manyglm(com_SS3_mvabund ~ fish_SS3 * relevel(isolation_SS3, ref = "30"), family = "negative.binomial")

fish_effect_SS3_30<-fit_SS3_fish_effect_30$coefficients[2,]
fish_effect_SS3_30_se <-fit_SS3_fish_effect_30$stderr.coefficients[2,]
fish_effect_SS3_30_se[which(fish_effect_SS3_30_se>100)] <- fit_SS3_fish_effect_30$stderr.coefficients[1,][which(fish_effect_SS3_30_se>100)]
fit_SS3_fish_effect_30 <- manyglm(com_SS3_mvabund ~ relevel(fish_SS3, ref = "present") * relevel(isolation_SS3, ref = "30"), family = "negative.binomial")
fish_effect_SS3_30_se[which(fish_effect_SS3_30_se>100)] <- fit_SS3_fish_effect_30$stderr.coefficients[1,][which(fish_effect_SS3_30_se>100)]


fish_effect_SS3_30[which(fish_effect_SS3_30_se>100)] <- 0
fish_effect_SS3_30_se[which(fish_effect_SS3_30_se>100)] <- 0

fish_effect_SS3_30
fish_effect_SS3_30_se

fish_effect_SS3_upper_30 <- fish_effect_SS3_30 + (z*fish_effect_SS3_30_se)
fish_effect_SS3_lower_30 <- fish_effect_SS3_30 - (z*fish_effect_SS3_30_se)




#Effect of fish in 120 m SS3

fit_SS3_fish_effect_120 <- manyglm(com_SS3_mvabund ~ fish_SS3 * relevel(isolation_SS3, ref = "120"), family = "negative.binomial")

fish_effect_SS3_120<-fit_SS3_fish_effect_120$coefficients[2,]
fish_effect_SS3_120_se <-fit_SS3_fish_effect_120$stderr.coefficients[2,]
fish_effect_SS3_120_se[which(fish_effect_SS3_120_se>100)] <- fit_SS3_fish_effect_120$stderr.coefficients[1,][which(fish_effect_SS3_120_se>100)]
fit_SS3_fish_effect_120 <- manyglm(com_SS3_mvabund ~ relevel(fish_SS3, ref = "present") * relevel(isolation_SS3, ref = "120"), family = "negative.binomial")
fish_effect_SS3_120_se[which(fish_effect_SS3_120_se>100)] <- fit_SS3_fish_effect_120$stderr.coefficients[1,][which(fish_effect_SS3_120_se>100)]

fish_effect_SS3_120[which(fish_effect_SS3_120_se>100)] <- 0
fish_effect_SS3_120_se[which(fish_effect_SS3_120_se>100)] <- 0

fish_effect_SS3_120
fish_effect_SS3_120_se

fish_effect_SS3_upper_120 <- fish_effect_SS3_120 + (z*fish_effect_SS3_120_se)
fish_effect_SS3_lower_120 <- fish_effect_SS3_120 - (z*fish_effect_SS3_120_se)


#Effect of fish in 480 m SS3

fit_SS3_fish_effect_480 <- manyglm(com_SS3_mvabund ~ fish_SS3 * relevel(isolation_SS3, ref = "480"), family = "negative.binomial")

fish_effect_SS3_480<-fit_SS3_fish_effect_480$coefficients[2,]
fish_effect_SS3_480_se <-fit_SS3_fish_effect_480$stderr.coefficients[2,]
fish_effect_SS3_480_se[which(fish_effect_SS3_480_se>100)] <- fit_SS3_fish_effect_480$stderr.coefficients[1,][which(fish_effect_SS3_480_se>100)]
fit_SS3_fish_effect_480 <- manyglm(com_SS3_mvabund ~ relevel(fish_SS3, ref = "present") * relevel(isolation_SS3, ref = "480"), family = "negative.binomial")
fish_effect_SS3_480_se[which(fish_effect_SS3_480_se>100)] <- fit_SS3_fish_effect_480$stderr.coefficients[1,][which(fish_effect_SS3_480_se>100)]

fish_effect_SS3_480[which(fish_effect_SS3_480_se>100)] <- 0
fish_effect_SS3_480_se[which(fish_effect_SS3_480_se>100)] <- 0

fish_effect_SS3_480
fish_effect_SS3_480_se

fish_effect_SS3_upper_480 <- fish_effect_SS3_480 + (z*fish_effect_SS3_480_se)
fish_effect_SS3_lower_480 <- fish_effect_SS3_480 - (z*fish_effect_SS3_480_se)


fit_SS3_trait_pred_interaction <- traitglm(L = com_SS3, R = env_SS3, Q = TRAITS_SS3, formula = ~ (isolation_SS3:trophic) * (fish_SS3:trophic), method = "manyglm", col.intercepts = T)
trait_fish_30_SS3 <- fit_SS3_trait_pred_interaction$coefficients[(1+dim(TRAITS_SS3)[1]+6):(1+dim(TRAITS_SS3)[1]+6+1)]; names(trait_fish_30_SS3) <- c("Non_Pred", "Pred")
trait_fish_30_SS3_se <- fit_SS3_trait_pred_interaction$stderr.coefficients[(1+dim(TRAITS_SS3)[1]+6):(1+dim(TRAITS_SS3)[1]+6+1)]; names(trait_fish_30_SS3_se) <- c("Non_Pred", "Pred")
trait_fish_30_SS3_lower <- trait_fish_30_SS3 - z*trait_fish_30_SS3_se
trait_fish_30_SS3_upper <- trait_fish_30_SS3 + z*trait_fish_30_SS3_se

fit_SS3_trait_pred_interaction <- traitglm(L = com_SS3, R = env_SS3, Q = TRAITS_SS3, formula = ~ (relevel(isolation_SS3, ref = "120"):trophic) * (fish_SS3:trophic), method = "manyglm", col.intercepts = T)
trait_fish_120_SS3 <- fit_SS3_trait_pred_interaction$coefficients[(1+dim(TRAITS_SS3)[1]+6):(1+dim(TRAITS_SS3)[1]+6+1)]; names(trait_fish_120_SS3) <- c("Non_Pred", "Pred")
trait_fish_120_SS3_se <- fit_SS3_trait_pred_interaction$stderr.coefficients[(1+dim(TRAITS_SS3)[1]+6):(1+dim(TRAITS_SS3)[1]+6+1)]; names(trait_fish_120_SS3_se) <- c("Non_Pred", "Pred")
trait_fish_120_SS3_lower <- trait_fish_120_SS3 - z*trait_fish_120_SS3_se
trait_fish_120_SS3_upper <- trait_fish_120_SS3 + z*trait_fish_120_SS3_se

fit_SS3_trait_pred_interaction <- traitglm(L = com_SS3, R = env_SS3, Q = TRAITS_SS3, formula = ~ (relevel(isolation_SS3, ref = "480"):trophic) * (fish_SS3:trophic), method = "manyglm", col.intercepts = T)
trait_fish_480_SS3  <- fit_SS3_trait_pred_interaction$coefficients[(1+dim(TRAITS_SS3)[1]+6):(1+dim(TRAITS_SS3)[1]+6+1)]; names(trait_fish_480_SS3) <- c("Non_Pred", "Pred")
trait_fish_480_SS3_se <- fit_SS3_trait_pred_interaction$stderr.coefficients[(1+dim(TRAITS_SS3)[1]+6):(1+dim(TRAITS_SS3)[1]+6+1)]; names(trait_fish_480_SS3_se) <- c("Non_Pred", "Pred")
trait_fish_480_SS3_lower <- trait_fish_480_SS3 - z*trait_fish_480_SS3_se
trait_fish_480_SS3_upper <- trait_fish_480_SS3 + z*trait_fish_480_SS3_se






#######################################################___________________________ISOLATION IN FISHLESS PONDS


##SS1

#Effect from 30 m to 120m

fit_SS1_isolation_effect_30_120 <- manyglm(com_SS1_mvabund ~ fish_SS1 * relevel(isolation_SS1, ref = "30"), family = "negative.binomial")

isolation_effect_SS1_30_120<-fit_SS1_isolation_effect_30_120$coefficients[3,]
isolation_effect_SS1_30_120_se <-fit_SS1_isolation_effect_30_120$stderr.coefficients[3,]
isolation_effect_SS1_30_120_se[which(isolation_effect_SS1_30_120_se>100)] <- fit_SS1_isolation_effect_30_120$stderr.coefficients[1,][which(isolation_effect_SS1_30_120_se>100)]
isolation_effect_SS1_30_120_se[which(isolation_effect_SS1_30_120_se>100)] <- fit_SS1_isolation_effect_30_120$stderr.coefficients[1,][which(isolation_effect_SS1_30_120_se>100)]

isolation_effect_SS1_30_120[which(isolation_effect_SS1_30_120_se>100)] <- 0
isolation_effect_SS1_30_120_se[which(isolation_effect_SS1_30_120_se>100)] <- 0

isolation_effect_SS1_30_120
isolation_effect_SS1_30_120_se

isolation_effect_SS1_upper_30_120 <- isolation_effect_SS1_30_120 + (z*isolation_effect_SS1_30_120_se)
isolation_effect_SS1_lower_30_120 <- isolation_effect_SS1_30_120 - (z*isolation_effect_SS1_30_120_se)




#Effect from 120 m to 480m

fit_SS1_isolation_effect_120_480 <- manyglm(com_SS1_mvabund ~ fish_SS1 * relevel(isolation_SS1, ref = "120"), family = "negative.binomial")

isolation_effect_SS1_120_480<-fit_SS1_isolation_effect_120_480$coefficients[4,]
isolation_effect_SS1_120_480_se <-fit_SS1_isolation_effect_120_480$stderr.coefficients[4,]
isolation_effect_SS1_120_480_se[which(isolation_effect_SS1_120_480_se>100)] <- fit_SS1_isolation_effect_120_480$stderr.coefficients[1,][which(isolation_effect_SS1_120_480_se>100)]
fit_SS1_isolation_effect_120_480 <- manyglm(com_SS1_mvabund ~ fish_SS1 * relevel(isolation_SS1, ref = "480"), family = "negative.binomial")
isolation_effect_SS1_120_480_se[which(isolation_effect_SS1_120_480_se>100)] <- fit_SS1_isolation_effect_120_480$stderr.coefficients[1,][which(isolation_effect_SS1_120_480_se>100)]

isolation_effect_SS1_120_480[which(isolation_effect_SS1_120_480_se>100)] <- 0
isolation_effect_SS1_120_480_se[which(isolation_effect_SS1_120_480_se>100)] <- 0

isolation_effect_SS1_120_480
isolation_effect_SS1_120_480_se

isolation_effect_SS1_upper_120_480 <- isolation_effect_SS1_120_480 + (z*isolation_effect_SS1_120_480_se)
isolation_effect_SS1_lower_120_480 <- isolation_effect_SS1_120_480 - (z*isolation_effect_SS1_120_480_se)



#Effect from 30 m to 480m

fit_SS1_isolation_effect_30_480 <- manyglm(com_SS1_mvabund ~ fish_SS1 * relevel(isolation_SS1, ref = "30"), family = "negative.binomial")

isolation_effect_SS1_30_480<-fit_SS1_isolation_effect_30_480$coefficients[4,]
isolation_effect_SS1_30_480_se <-fit_SS1_isolation_effect_30_480$stderr.coefficients[4,]
isolation_effect_SS1_30_480_se[which(isolation_effect_SS1_30_480_se>100)] <- fit_SS1_isolation_effect_30_480$stderr.coefficients[1,][which(isolation_effect_SS1_30_480_se>100)]
fit_SS1_isolation_effect_30_480 <- manyglm(com_SS1_mvabund ~ fish_SS1 * relevel(isolation_SS1, ref = "480"), family = "negative.binomial")
isolation_effect_SS1_30_480_se[which(isolation_effect_SS1_30_480_se>100)] <- fit_SS1_isolation_effect_30_480$stderr.coefficients[1,][which(isolation_effect_SS1_30_480_se>100)]

isolation_effect_SS1_30_480[which(isolation_effect_SS1_30_480_se>100)] <- 0
isolation_effect_SS1_30_480_se[which(isolation_effect_SS1_30_480_se>100)] <- 0

isolation_effect_SS1_30_480
isolation_effect_SS1_30_480_se

isolation_effect_SS1_upper_30_480 <- isolation_effect_SS1_30_480 + (z*isolation_effect_SS1_30_480_se)
isolation_effect_SS1_lower_30_480 <- isolation_effect_SS1_30_480 - (z*isolation_effect_SS1_30_480_se)



##SS2

#Effect from 30 m to 120m

fit_SS2_isolation_effect_30_120 <- manyglm(com_SS2_mvabund ~ fish_SS2 * relevel(isolation_SS2, ref = "30"), family = "negative.binomial")

isolation_effect_SS2_30_120<-fit_SS2_isolation_effect_30_120$coefficients[3,]
isolation_effect_SS2_30_120_se <-fit_SS2_isolation_effect_30_120$stderr.coefficients[3,]
isolation_effect_SS2_30_120_se[which(isolation_effect_SS2_30_120_se>100)] <- fit_SS2_isolation_effect_30_120$stderr.coefficients[1,][which(isolation_effect_SS2_30_120_se>100)]
fit_SS2_isolation_effect_30_120 <- manyglm(com_SS2_mvabund ~ fish_SS2 * relevel(isolation_SS2, ref = "120"), family = "negative.binomial")
isolation_effect_SS2_30_120_se[which(isolation_effect_SS2_30_120_se>100)] <- fit_SS2_isolation_effect_30_120$stderr.coefficients[1,][which(isolation_effect_SS2_30_120_se>100)]

isolation_effect_SS2_30_120[which(isolation_effect_SS2_30_120_se>100)] <- 0
isolation_effect_SS2_30_120_se[which(isolation_effect_SS2_30_120_se>100)] <- 0

isolation_effect_SS2_30_120
isolation_effect_SS2_30_120_se

isolation_effect_SS2_upper_30_120 <- isolation_effect_SS2_30_120 + (z*isolation_effect_SS2_30_120_se)
isolation_effect_SS2_lower_30_120 <- isolation_effect_SS2_30_120 - (z*isolation_effect_SS2_30_120_se)




#Effect from 120 m to 480m

fit_SS2_isolation_effect_120_480 <- manyglm(com_SS2_mvabund ~ fish_SS2 * relevel(isolation_SS2, ref = "120"), family = "negative.binomial")

isolation_effect_SS2_120_480<-fit_SS2_isolation_effect_120_480$coefficients[4,]
isolation_effect_SS2_120_480_se <-fit_SS2_isolation_effect_120_480$stderr.coefficients[4,]
isolation_effect_SS2_120_480_se[which(isolation_effect_SS2_120_480_se>100)] <- fit_SS2_isolation_effect_120_480$stderr.coefficients[1,][which(isolation_effect_SS2_120_480_se>100)]
fit_SS2_isolation_effect_120_480 <- manyglm(com_SS2_mvabund ~ fish_SS2 * relevel(isolation_SS2, ref = "480"), family = "negative.binomial")
isolation_effect_SS2_120_480_se[which(isolation_effect_SS2_120_480_se>100)] <- fit_SS2_isolation_effect_120_480$stderr.coefficients[1,][which(isolation_effect_SS2_120_480_se>100)]

isolation_effect_SS2_120_480[which(isolation_effect_SS2_120_480_se>100)] <- 0
isolation_effect_SS2_120_480_se[which(isolation_effect_SS2_120_480_se>100)] <- 0

isolation_effect_SS2_120_480
isolation_effect_SS2_120_480_se

isolation_effect_SS2_upper_120_480 <- isolation_effect_SS2_120_480 + (z*isolation_effect_SS2_120_480_se)
isolation_effect_SS2_lower_120_480 <- isolation_effect_SS2_120_480 - (z*isolation_effect_SS2_120_480_se)



#Effect from 30 m to 480m

fit_SS2_isolation_effect_30_480 <- manyglm(com_SS2_mvabund ~ fish_SS2 * relevel(isolation_SS2, ref = "30"), family = "negative.binomial")

isolation_effect_SS2_30_480<-fit_SS2_isolation_effect_30_480$coefficients[4,]
isolation_effect_SS2_30_480_se <-fit_SS2_isolation_effect_30_480$stderr.coefficients[4,]
isolation_effect_SS2_30_480_se[which(isolation_effect_SS2_30_480_se>100)] <- fit_SS2_isolation_effect_30_480$stderr.coefficients[1,][which(isolation_effect_SS2_30_480_se>100)]
fit_SS2_isolation_effect_30_480 <- manyglm(com_SS2_mvabund ~ fish_SS2 * relevel(isolation_SS2, ref = "480"), family = "negative.binomial")
isolation_effect_SS2_30_480_se[which(isolation_effect_SS2_30_480_se>100)] <- fit_SS2_isolation_effect_30_480$stderr.coefficients[1,][which(isolation_effect_SS2_30_480_se>100)]

isolation_effect_SS2_30_480[which(isolation_effect_SS2_30_480_se>100)] <- 0
isolation_effect_SS2_30_480_se[which(isolation_effect_SS2_30_480_se>100)] <- 0

isolation_effect_SS2_30_480
isolation_effect_SS2_30_480_se

isolation_effect_SS2_upper_30_480 <- isolation_effect_SS2_30_480 + (z*isolation_effect_SS2_30_480_se)
isolation_effect_SS2_lower_30_480 <- isolation_effect_SS2_30_480 - (z*isolation_effect_SS2_30_480_se)


fit_SS2_trait_pred_interaction <- traitglm(L = com_SS2, R = env_SS2, Q = TRAITS_SS2, formula = ~ (fish_SS2:trophic) * (isolation_SS2:trophic), method = "manyglm")
trait_30_120_SS2 <- fit_SS2_trait_pred_interaction$coefficients[(1+dim(TRAITS_SS2)[1]+4):(1+dim(TRAITS_SS2)[1]+4+1)]; names(trait_30_120_SS2) <- c("Non_Pred", "Pred")
trait_30_120_SS2_se <-fit_SS2_trait_pred_interaction$stderr.coefficients[(1+dim(TRAITS_SS2)[1]+4):(1+dim(TRAITS_SS2)[1]+4+1)]; names(trait_30_120_SS2_se) <- c("Non_Pred", "Pred")
trait_30_120_SS2_lower <- trait_30_120_SS2 - z*trait_30_120_SS2_se
trait_30_120_SS2_upper <- trait_30_120_SS2 + z*trait_30_120_SS2_se

fit_SS2_trait_pred_interaction <- traitglm(L = com_SS2, R = env_SS2, Q = TRAITS_SS2, formula = ~ (fish_SS2:trophic) * (relevel(isolation_SS2, ref = "120"):trophic), method = "manyglm")
trait_120_480_SS2 <- fit_SS2_trait_pred_interaction$coefficients[(1+dim(TRAITS_SS2)[1]+6):(1+dim(TRAITS_SS2)[1]+6+1)]; names(trait_120_480_SS2) <- c("Non_Pred", "Pred")
trait_120_480_SS2_se <- fit_SS2_trait_pred_interaction$stderr.coefficients[(1+dim(TRAITS_SS2)[1]+6):(1+dim(TRAITS_SS2)[1]+6+1)]; names(trait_120_480_SS2_se) <- c("Non_Pred", "Pred")
trait_120_480_SS2_lower <- trait_120_480_SS2 - z*trait_120_480_SS2_se
trait_120_480_SS2_upper <- trait_120_480_SS2 + z*trait_120_480_SS2_se

fit_SS2_trait_pred_interaction <- traitglm(L = com_SS2, R = env_SS2, Q = TRAITS_SS2, formula = ~ (fish_SS2:trophic) * (isolation_SS2:trophic), method = "manyglm")
trait_30_480_SS2 <- fit_SS2_trait_pred_interaction$coefficients[(1+dim(TRAITS_SS2)[1]+6):(1+dim(TRAITS_SS2)[1]+6+1)]; names(trait_30_480_SS2) <- c("Non_Pred", "Pred")
trait_30_480_SS2_se <- fit_SS2_trait_pred_interaction$stderr.coefficients[(1+dim(TRAITS_SS2)[1]+6):(1+dim(TRAITS_SS2)[1]+6+1)]; names(trait_30_480_SS2_se) <- c("Non_Pred", "Pred")
trait_30_480_SS2_lower <- trait_30_480_SS2 - z*trait_30_480_SS2_se
trait_30_480_SS2_upper <- trait_30_480_SS2 + z*trait_30_480_SS2_se



##SS3

#Effect from 30 m to 120m

fit_SS3_isolation_effect_30_120 <- manyglm(com_SS3_mvabund ~ relevel(fish_SS3, ref = "absent") * relevel(isolation_SS3, ref = "30"), family = "negative.binomial")

isolation_effect_SS3_30_120<-fit_SS3_isolation_effect_30_120$coefficients[3,]
isolation_effect_SS3_30_120_se <-fit_SS3_isolation_effect_30_120$stderr.coefficients[3,]
isolation_effect_SS3_30_120_se[which(isolation_effect_SS3_30_120_se>100)] <- fit_SS3_isolation_effect_30_120$stderr.coefficients[1,][which(isolation_effect_SS3_30_120_se>100)]
fit_SS3_isolation_effect_30_120 <- manyglm(com_SS3_mvabund ~ relevel(fish_SS3, ref = "absent") * relevel(isolation_SS3, ref = "120"), family = "negative.binomial")
isolation_effect_SS3_30_120_se[which(isolation_effect_SS3_30_120_se>100)] <- fit_SS3_isolation_effect_30_120$stderr.coefficients[1,][which(isolation_effect_SS3_30_120_se>100)]

isolation_effect_SS3_30_120[which(isolation_effect_SS3_30_120_se>100)] <- 0
isolation_effect_SS3_30_120_se[which(isolation_effect_SS3_30_120_se>100)] <- 0

isolation_effect_SS3_30_120
isolation_effect_SS3_30_120_se

isolation_effect_SS3_upper_30_120 <- isolation_effect_SS3_30_120 + (z*isolation_effect_SS3_30_120_se)
isolation_effect_SS3_lower_30_120 <- isolation_effect_SS3_30_120 - (z*isolation_effect_SS3_30_120_se)




#Effect from 120 m to 480m

fit_SS3_isolation_effect_120_480 <- manyglm(com_SS3_mvabund ~ relevel(fish_SS3, ref = "absent") * relevel(isolation_SS3, ref = "120"), family = "negative.binomial")

isolation_effect_SS3_120_480<-fit_SS3_isolation_effect_120_480$coefficients[4,]
isolation_effect_SS3_120_480_se <-fit_SS3_isolation_effect_120_480$stderr.coefficients[4,]
isolation_effect_SS3_120_480_se[which(isolation_effect_SS3_120_480_se>100)] <- fit_SS3_isolation_effect_120_480$stderr.coefficients[1,][which(isolation_effect_SS3_120_480_se>100)]
fit_SS3_isolation_effect_120_480 <- manyglm(com_SS3_mvabund ~ relevel(fish_SS3, ref = "absent") * relevel(isolation_SS3, ref = "480"), family = "negative.binomial")
isolation_effect_SS3_120_480_se[which(isolation_effect_SS3_120_480_se>100)] <- fit_SS3_isolation_effect_120_480$stderr.coefficients[1,][which(isolation_effect_SS3_120_480_se>100)]

isolation_effect_SS3_120_480[which(isolation_effect_SS3_120_480_se>100)] <- 0
isolation_effect_SS3_120_480_se[which(isolation_effect_SS3_120_480_se>100)] <- 0

isolation_effect_SS3_120_480
isolation_effect_SS3_120_480_se

isolation_effect_SS3_upper_120_480 <- isolation_effect_SS3_120_480 + (z*isolation_effect_SS3_120_480_se)
isolation_effect_SS3_lower_120_480 <- isolation_effect_SS3_120_480 - (z*isolation_effect_SS3_120_480_se)



#Effect from 30 m to 480m

fit_SS3_isolation_effect_30_480 <- manyglm(com_SS3_mvabund ~ relevel(fish_SS3, ref = "absent") * relevel(isolation_SS3, ref = "30"), family = "negative.binomial")

isolation_effect_SS3_30_480<-fit_SS3_isolation_effect_30_480$coefficients[4,]
isolation_effect_SS3_30_480_se <-fit_SS3_isolation_effect_30_480$stderr.coefficients[4,]
isolation_effect_SS3_30_480_se[which(isolation_effect_SS3_30_480_se>100)] <- fit_SS3_isolation_effect_30_480$stderr.coefficients[1,][which(isolation_effect_SS3_30_480_se>100)]
fit_SS3_isolation_effect_30_480 <- manyglm(com_SS3_mvabund ~ relevel(fish_SS3, ref = "absent") * relevel(isolation_SS3, ref = "480"), family = "negative.binomial")
isolation_effect_SS3_30_480_se[which(isolation_effect_SS3_30_480_se>100)] <- fit_SS3_isolation_effect_30_480$stderr.coefficients[1,][which(isolation_effect_SS3_30_480_se>100)]

isolation_effect_SS3_30_480[which(isolation_effect_SS3_30_480_se>100)] <- 0
isolation_effect_SS3_30_480_se[which(isolation_effect_SS3_30_480_se>100)] <- 0

isolation_effect_SS3_30_480
isolation_effect_SS3_30_480_se

isolation_effect_SS3_upper_30_480 <- isolation_effect_SS3_30_480 + (z*isolation_effect_SS3_30_480_se)
isolation_effect_SS3_lower_30_480 <- isolation_effect_SS3_30_480 - (z*isolation_effect_SS3_30_480_se)


fit_SS3_trait_pred_interaction <- traitglm(L = com_SS3, R = env_SS3, Q = TRAITS_SS3, formula = ~ (fish_SS3:trophic) * (isolation_SS3:trophic), method = "manyglm")
trait_30_120_SS3 <- fit_SS3_trait_pred_interaction$coefficients[(1+dim(TRAITS_SS3)[1]+4):(1+dim(TRAITS_SS3)[1]+4+1)]; names(trait_30_120_SS3) <- c("Non_Pred", "Pred")
trait_30_120_SS3_se <-fit_SS3_trait_pred_interaction$stderr.coefficients[(1+dim(TRAITS_SS3)[1]+4):(1+dim(TRAITS_SS3)[1]+4+1)]; names(trait_30_120_SS3_se) <- c("Non_Pred", "Pred")
trait_30_120_SS3_lower <- trait_30_120_SS3 - z*trait_30_120_SS3_se
trait_30_120_SS3_upper <- trait_30_120_SS3 + z*trait_30_120_SS3_se

fit_SS3_trait_pred_interaction <- traitglm(L = com_SS3, R = env_SS3, Q = TRAITS_SS3, formula = ~ (fish_SS3:trophic) * (relevel(isolation_SS3, ref = "120"):trophic), method = "manyglm")
trait_120_480_SS3 <- fit_SS3_trait_pred_interaction$coefficients[(1+dim(TRAITS_SS3)[1]+6):(1+dim(TRAITS_SS3)[1]+6+1)]; names(trait_120_480_SS3) <- c("Non_Pred", "Pred")
trait_120_480_SS3_se <- fit_SS3_trait_pred_interaction$stderr.coefficients[(1+dim(TRAITS_SS3)[1]+6):(1+dim(TRAITS_SS3)[1]+6+1)]; names(trait_120_480_SS3_se) <- c("Non_Pred", "Pred")
trait_120_480_SS3_lower <- trait_120_480_SS3 - z*trait_120_480_SS3_se
trait_120_480_SS3_upper <- trait_120_480_SS3 + z*trait_120_480_SS3_se

fit_SS3_trait_pred_interaction <- traitglm(L = com_SS3, R = env_SS3, Q = TRAITS_SS3, formula = ~ (fish_SS3:trophic) * (isolation_SS3:trophic), method = "manyglm")
trait_30_480_SS3 <- fit_SS3_trait_pred_interaction$coefficients[(1+dim(TRAITS_SS3)[1]+6):(1+dim(TRAITS_SS3)[1]+6+1)]; names(trait_30_480_SS3) <- c("Non_Pred", "Pred")
trait_30_480_SS3_se <- fit_SS3_trait_pred_interaction$stderr.coefficients[(1+dim(TRAITS_SS3)[1]+6):(1+dim(TRAITS_SS3)[1]+6+1)]; names(trait_30_480_SS3_se) <- c("Non_Pred", "Pred")
trait_30_480_SS3_lower <- trait_30_480_SS3 - z*trait_30_480_SS3_se
trait_30_480_SS3_upper <- trait_30_480_SS3 + z*trait_30_480_SS3_se



#######################################################___________________________ISOLATION IN FISH PONDS


##SS1

#Effect from 30 m to 120m

fit_SS1_isolation_effect_fish_30_120 <- manyglm(com_SS1_mvabund ~ relevel(fish_SS1, ref = "present") * relevel(isolation_SS1, ref = "30"), family = "negative.binomial")

isolation_effect_fish_SS1_30_120<-fit_SS1_isolation_effect_fish_30_120$coefficients[3,]
isolation_effect_fish_SS1_30_120_se <-fit_SS1_isolation_effect_fish_30_120$stderr.coefficients[3,]
isolation_effect_fish_SS1_30_120_se[which(isolation_effect_fish_SS1_30_120_se>100)] <- fit_SS1_isolation_effect_fish_30_120$stderr.coefficients[1,][which(isolation_effect_fish_SS1_30_120_se>100)]
fit_SS1_isolation_effect_fish_30_120 <- manyglm(com_SS1_mvabund ~ relevel(fish_SS1, ref = "present") * relevel(isolation_SS1, ref = "120"), family = "negative.binomial")
isolation_effect_fish_SS1_30_120_se[which(isolation_effect_fish_SS1_30_120_se>100)] <- fit_SS1_isolation_effect_fish_30_120$stderr.coefficients[1,][which(isolation_effect_fish_SS1_30_120_se>100)]

isolation_effect_fish_SS1_30_120[which(isolation_effect_fish_SS1_30_120_se>100)] <- 0
isolation_effect_fish_SS1_30_120_se[which(isolation_effect_fish_SS1_30_120_se>100)] <- 0

isolation_effect_fish_SS1_30_120
isolation_effect_fish_SS1_30_120_se

isolation_effect_fish_SS1_upper_30_120 <- isolation_effect_fish_SS1_30_120 + (z*isolation_effect_fish_SS1_30_120_se)
isolation_effect_fish_SS1_lower_30_120 <- isolation_effect_fish_SS1_30_120 - (z*isolation_effect_fish_SS1_30_120_se)




#Effect from 120 m to 480m

fit_SS1_isolation_effect_fish_120_480 <- manyglm(com_SS1_mvabund ~ relevel(fish_SS1, ref = "present") * relevel(isolation_SS1, ref = "120"), family = "negative.binomial")

isolation_effect_fish_SS1_120_480<-fit_SS1_isolation_effect_fish_120_480$coefficients[4,]
isolation_effect_fish_SS1_120_480_se <-fit_SS1_isolation_effect_fish_120_480$stderr.coefficients[4,]
isolation_effect_fish_SS1_120_480_se[which(isolation_effect_fish_SS1_120_480_se>100)] <- fit_SS1_isolation_effect_fish_120_480$stderr.coefficients[1,][which(isolation_effect_fish_SS1_120_480_se>100)]
fit_SS1_isolation_effect_fish_120_480 <- manyglm(com_SS1_mvabund ~ relevel(fish_SS1, ref = "present") * relevel(isolation_SS1, ref = "480"), family = "negative.binomial")
isolation_effect_fish_SS1_120_480_se[which(isolation_effect_fish_SS1_120_480_se>100)] <- fit_SS1_isolation_effect_fish_120_480$stderr.coefficients[1,][which(isolation_effect_fish_SS1_120_480_se>100)]

isolation_effect_fish_SS1_120_480[which(isolation_effect_fish_SS1_120_480_se>100)] <- 0
isolation_effect_fish_SS1_120_480_se[which(isolation_effect_fish_SS1_120_480_se>100)] <- 0

isolation_effect_fish_SS1_120_480
isolation_effect_fish_SS1_120_480_se

isolation_effect_fish_SS1_upper_120_480 <- isolation_effect_fish_SS1_120_480 + (z*isolation_effect_fish_SS1_120_480_se)
isolation_effect_fish_SS1_lower_120_480 <- isolation_effect_fish_SS1_120_480 - (z*isolation_effect_fish_SS1_120_480_se)



#Effect from 30 m to 480m

fit_SS1_isolation_effect_fish_30_480 <- manyglm(com_SS1_mvabund ~ relevel(fish_SS1, ref = "present") * relevel(isolation_SS1, ref = "30"), family = "negative.binomial")

isolation_effect_fish_SS1_30_480<-fit_SS1_isolation_effect_fish_30_480$coefficients[4,]
isolation_effect_fish_SS1_30_480_se <-fit_SS1_isolation_effect_fish_30_480$stderr.coefficients[4,]
isolation_effect_fish_SS1_30_480_se[which(isolation_effect_fish_SS1_30_480_se>100)] <- fit_SS1_isolation_effect_fish_30_480$stderr.coefficients[1,][which(isolation_effect_fish_SS1_30_480_se>100)]
fit_SS1_isolation_effect_fish_30_480 <- manyglm(com_SS1_mvabund ~ relevel(fish_SS1, ref = "present") * relevel(isolation_SS1, ref = "480"), family = "negative.binomial")
isolation_effect_fish_SS1_30_480_se[which(isolation_effect_fish_SS1_30_480_se>100)] <- fit_SS1_isolation_effect_fish_30_480$stderr.coefficients[1,][which(isolation_effect_fish_SS1_30_480_se>100)]

isolation_effect_fish_SS1_30_480[which(isolation_effect_fish_SS1_30_480_se>100)] <- 0
isolation_effect_fish_SS1_30_480_se[which(isolation_effect_fish_SS1_30_480_se>100)] <- 0

isolation_effect_fish_SS1_30_480
isolation_effect_fish_SS1_30_480_se

isolation_effect_fish_SS1_upper_30_480 <- isolation_effect_fish_SS1_30_480 + (z*isolation_effect_fish_SS1_30_480_se)
isolation_effect_fish_SS1_lower_30_480 <- isolation_effect_fish_SS1_30_480 - (z*isolation_effect_fish_SS1_30_480_se)



##SS2

#Effect from 30 m to 120m

fit_SS2_isolation_effect_fish_30_120 <- manyglm(com_SS2_mvabund ~ relevel(fish_SS2, ref = "present") * relevel(isolation_SS2, ref = "30"), family = "negative.binomial")

isolation_effect_fish_SS2_30_120<-fit_SS2_isolation_effect_fish_30_120$coefficients[3,]
isolation_effect_fish_SS2_30_120_se <-fit_SS2_isolation_effect_fish_30_120$stderr.coefficients[3,]
isolation_effect_fish_SS2_30_120_se[which(isolation_effect_fish_SS2_30_120_se>100)] <- fit_SS2_isolation_effect_fish_30_120$stderr.coefficients[1,][which(isolation_effect_fish_SS2_30_120_se>100)]
fit_SS2_isolation_effect_fish_30_120 <- manyglm(com_SS2_mvabund ~ relevel(fish_SS2, ref = "present") * relevel(isolation_SS2, ref = "120"), family = "negative.binomial")
isolation_effect_fish_SS2_30_120_se[which(isolation_effect_fish_SS2_30_120_se>100)] <- fit_SS2_isolation_effect_fish_30_120$stderr.coefficients[1,][which(isolation_effect_fish_SS2_30_120_se>100)]

isolation_effect_fish_SS2_30_120[which(isolation_effect_fish_SS2_30_120_se>100)] <- 0
isolation_effect_fish_SS2_30_120_se[which(isolation_effect_fish_SS2_30_120_se>100)] <- 0

isolation_effect_fish_SS2_30_120
isolation_effect_fish_SS2_30_120_se

isolation_effect_fish_SS2_upper_30_120 <- isolation_effect_fish_SS2_30_120 + (z*isolation_effect_fish_SS2_30_120_se)
isolation_effect_fish_SS2_lower_30_120 <- isolation_effect_fish_SS2_30_120 - (z*isolation_effect_fish_SS2_30_120_se)




#Effect from 120 m to 480m

fit_SS2_isolation_effect_fish_120_480 <- manyglm(com_SS2_mvabund ~ relevel(fish_SS2, ref = "present") * relevel(isolation_SS2, ref = "120"), family = "negative.binomial")

isolation_effect_fish_SS2_120_480<-fit_SS2_isolation_effect_fish_120_480$coefficients[4,]
isolation_effect_fish_SS2_120_480_se <-fit_SS2_isolation_effect_fish_120_480$stderr.coefficients[4,]
isolation_effect_fish_SS2_120_480_se[which(isolation_effect_fish_SS2_120_480_se>100)] <- fit_SS2_isolation_effect_fish_120_480$stderr.coefficients[1,][which(isolation_effect_fish_SS2_120_480_se>100)]
fit_SS2_isolation_effect_fish_120_480 <- manyglm(com_SS2_mvabund ~ relevel(fish_SS2, ref = "present") * relevel(isolation_SS2, ref = "480"), family = "negative.binomial")
isolation_effect_fish_SS2_120_480_se[which(isolation_effect_fish_SS2_120_480_se>100)] <- fit_SS2_isolation_effect_fish_120_480$stderr.coefficients[1,][which(isolation_effect_fish_SS2_120_480_se>100)]

isolation_effect_fish_SS2_120_480[which(isolation_effect_fish_SS2_120_480_se>100)] <- 0
isolation_effect_fish_SS2_120_480_se[which(isolation_effect_fish_SS2_120_480_se>100)] <- 0

isolation_effect_fish_SS2_120_480
isolation_effect_fish_SS2_120_480_se

isolation_effect_fish_SS2_upper_120_480 <- isolation_effect_fish_SS2_120_480 + (z*isolation_effect_fish_SS2_120_480_se)
isolation_effect_fish_SS2_lower_120_480 <- isolation_effect_fish_SS2_120_480 - (z*isolation_effect_fish_SS2_120_480_se)



#Effect from 30 m to 480m

fit_SS2_isolation_effect_fish_30_480 <- manyglm(com_SS2_mvabund ~ relevel(fish_SS2, ref = "present") * relevel(isolation_SS2, ref = "30"), family = "negative.binomial")

isolation_effect_fish_SS2_30_480<-fit_SS2_isolation_effect_fish_30_480$coefficients[4,]
isolation_effect_fish_SS2_30_480_se <-fit_SS2_isolation_effect_fish_30_480$stderr.coefficients[4,]
isolation_effect_fish_SS2_30_480_se[which(isolation_effect_fish_SS2_30_480_se>100)] <- fit_SS2_isolation_effect_fish_30_480$stderr.coefficients[1,][which(isolation_effect_fish_SS2_30_480_se>100)]
fit_SS2_isolation_effect_fish_30_480 <- manyglm(com_SS2_mvabund ~ relevel(fish_SS2, ref = "present") * relevel(isolation_SS2, ref = "480"), family = "negative.binomial")
isolation_effect_fish_SS2_30_480_se[which(isolation_effect_fish_SS2_30_480_se>100)] <- fit_SS2_isolation_effect_fish_30_480$stderr.coefficients[1,][which(isolation_effect_fish_SS2_30_480_se>100)]

isolation_effect_fish_SS2_30_480[which(isolation_effect_fish_SS2_30_480_se>100)] <- 0
isolation_effect_fish_SS2_30_480_se[which(isolation_effect_fish_SS2_30_480_se>100)] <- 0

isolation_effect_fish_SS2_30_480
isolation_effect_fish_SS2_30_480_se

isolation_effect_fish_SS2_upper_30_480 <- isolation_effect_fish_SS2_30_480 + (z*isolation_effect_fish_SS2_30_480_se)
isolation_effect_fish_SS2_lower_30_480 <- isolation_effect_fish_SS2_30_480 - (z*isolation_effect_fish_SS2_30_480_se)


fit_SS2_trait_pred_interaction <- traitglm(L = com_SS2, R = env_SS2, Q = TRAITS_SS2, formula = ~ (relevel(fish_SS2, ref = "present"):trophic) * (isolation_SS2:trophic), method = "manyglm")
trait_30_120_fish_SS2 <- fit_SS2_trait_pred_interaction$coefficients[(1+dim(TRAITS_SS2)[1]+4):(1+dim(TRAITS_SS2)[1]+4+1)]; names(trait_30_120_fish_SS2) <- c("Non_Pred", "Pred")
trait_30_120_fish_SS2_se <-fit_SS2_trait_pred_interaction$stderr.coefficients[(1+dim(TRAITS_SS2)[1]+4):(1+dim(TRAITS_SS2)[1]+4+1)]; names(trait_30_120_fish_SS2_se) <- c("Non_Pred", "Pred")
trait_30_120_fish_SS2_lower <- trait_30_120_fish_SS2 - z*trait_30_120_fish_SS2_se
trait_30_120_fish_SS2_upper <- trait_30_120_fish_SS2 + z*trait_30_120_fish_SS2_se

fit_SS2_trait_pred_interaction <- traitglm(L = com_SS2, R = env_SS2, Q = TRAITS_SS2, formula = ~ (relevel(fish_SS2, ref = "present"):trophic) * (relevel(isolation_SS2, ref = "120"):trophic), method = "manyglm")
trait_120_480_fish_SS2 <- fit_SS2_trait_pred_interaction$coefficients[(1+dim(TRAITS_SS2)[1]+6):(1+dim(TRAITS_SS2)[1]+6+1)]; names(trait_120_480_fish_SS2) <- c("Non_Pred", "Pred")
trait_120_480_fish_SS2_se <- fit_SS2_trait_pred_interaction$stderr.coefficients[(1+dim(TRAITS_SS2)[1]+6):(1+dim(TRAITS_SS2)[1]+6+1)]; names(trait_120_480_fish_SS2_se) <- c("Non_Pred", "Pred")
trait_120_480_fish_SS2_lower <- trait_120_480_fish_SS2 - z*trait_120_480_fish_SS2_se
trait_120_480_fish_SS2_upper <- trait_120_480_fish_SS2 + z*trait_120_480_fish_SS2_se

fit_SS2_trait_pred_interaction <- traitglm(L = com_SS2, R = env_SS2, Q = TRAITS_SS2, formula = ~ (relevel(fish_SS2, ref = "present"):trophic) * (isolation_SS2:trophic), method = "manyglm")
trait_30_480_fish_SS2 <- fit_SS2_trait_pred_interaction$coefficients[(1+dim(TRAITS_SS2)[1]+6):(1+dim(TRAITS_SS2)[1]+6+1)]; names(trait_30_480_fish_SS2) <- c("Non_Pred", "Pred")
trait_30_480_fish_SS2_se <- fit_SS2_trait_pred_interaction$stderr.coefficients[(1+dim(TRAITS_SS2)[1]+6):(1+dim(TRAITS_SS2)[1]+6+1)]; names(trait_30_480_fish_SS2_se) <- c("Non_Pred", "Pred")
trait_30_480_fish_SS2_lower <- trait_30_480_fish_SS2 - z*trait_30_480_fish_SS2_se
trait_30_480_fish_SS2_upper <- trait_30_480_fish_SS2 + z*trait_30_480_fish_SS2_se



##SS3

#Effect from 30 m to 120m

fit_SS3_isolation_effect_fish_30_120 <- manyglm(com_SS3_mvabund ~ relevel(fish_SS3, ref = "present") * relevel(isolation_SS3, ref = "30"), family = "negative.binomial")

isolation_effect_fish_SS3_30_120<-fit_SS3_isolation_effect_fish_30_120$coefficients[3,]
isolation_effect_fish_SS3_30_120_se <-fit_SS3_isolation_effect_fish_30_120$stderr.coefficients[3,]
isolation_effect_fish_SS3_30_120_se[which(isolation_effect_fish_SS3_30_120_se>100)] <- fit_SS3_isolation_effect_fish_30_120$stderr.coefficients[1,][which(isolation_effect_fish_SS3_30_120_se>100)]
fit_SS3_isolation_effect_fish_30_120 <- manyglm(com_SS3_mvabund ~ relevel(fish_SS3, ref = "present") * relevel(isolation_SS3, ref = "120"), family = "negative.binomial")
isolation_effect_fish_SS3_30_120_se[which(isolation_effect_fish_SS3_30_120_se>100)] <- fit_SS3_isolation_effect_fish_30_120$stderr.coefficients[1,][which(isolation_effect_fish_SS3_30_120_se>100)]

isolation_effect_fish_SS3_30_120[which(isolation_effect_fish_SS3_30_120_se>100)] <- 0
isolation_effect_fish_SS3_30_120_se[which(isolation_effect_fish_SS3_30_120_se>100)] <- 0

isolation_effect_fish_SS3_30_120
isolation_effect_fish_SS3_30_120_se

isolation_effect_fish_SS3_upper_30_120 <- isolation_effect_fish_SS3_30_120 + (z*isolation_effect_fish_SS3_30_120_se)
isolation_effect_fish_SS3_lower_30_120 <- isolation_effect_fish_SS3_30_120 - (z*isolation_effect_fish_SS3_30_120_se)




#Effect from 120 m to 480m

fit_SS3_isolation_effect_fish_120_480 <- manyglm(com_SS3_mvabund ~ relevel(fish_SS3, ref = "present") * relevel(isolation_SS3, ref = "120"), family = "negative.binomial")

isolation_effect_fish_SS3_120_480<-fit_SS3_isolation_effect_fish_120_480$coefficients[4,]
isolation_effect_fish_SS3_120_480_se <-fit_SS3_isolation_effect_fish_120_480$stderr.coefficients[4,]
isolation_effect_fish_SS3_120_480_se[which(isolation_effect_fish_SS3_120_480_se>100)] <- fit_SS3_isolation_effect_fish_120_480$stderr.coefficients[1,][which(isolation_effect_fish_SS3_120_480_se>100)]
fit_SS3_isolation_effect_fish_120_480 <- manyglm(com_SS3_mvabund ~ relevel(fish_SS3, ref = "present") * relevel(isolation_SS3, ref = "480"), family = "negative.binomial")
isolation_effect_fish_SS3_120_480_se[which(isolation_effect_fish_SS3_120_480_se>100)] <- fit_SS3_isolation_effect_fish_120_480$stderr.coefficients[1,][which(isolation_effect_fish_SS3_120_480_se>100)]

isolation_effect_fish_SS3_120_480[which(isolation_effect_fish_SS3_120_480_se>100)] <- 0
isolation_effect_fish_SS3_120_480_se[which(isolation_effect_fish_SS3_120_480_se>100)] <- 0

isolation_effect_fish_SS3_120_480
isolation_effect_fish_SS3_120_480_se

isolation_effect_fish_SS3_upper_120_480 <- isolation_effect_fish_SS3_120_480 + (z*isolation_effect_fish_SS3_120_480_se)
isolation_effect_fish_SS3_lower_120_480 <- isolation_effect_fish_SS3_120_480 - (z*isolation_effect_fish_SS3_120_480_se)



#Effect from 30 m to 480m

fit_SS3_isolation_effect_fish_30_480 <- manyglm(com_SS3_mvabund ~ relevel(fish_SS3, ref = "present") * relevel(isolation_SS3, ref = "30"), family = "negative.binomial")

isolation_effect_fish_SS3_30_480<-fit_SS3_isolation_effect_fish_30_480$coefficients[4,]
isolation_effect_fish_SS3_30_480_se <-fit_SS3_isolation_effect_fish_30_480$stderr.coefficients[4,]
isolation_effect_fish_SS3_30_480_se[which(isolation_effect_fish_SS3_30_480_se>100)] <- fit_SS3_isolation_effect_fish_30_480$stderr.coefficients[1,][which(isolation_effect_fish_SS3_30_480_se>100)]
fit_SS3_isolation_effect_fish_30_480 <- manyglm(com_SS3_mvabund ~ relevel(fish_SS3, ref = "present") * relevel(isolation_SS3, ref = "480"), family = "negative.binomial")
isolation_effect_fish_SS3_30_480_se[which(isolation_effect_fish_SS3_30_480_se>100)] <- fit_SS3_isolation_effect_fish_30_480$stderr.coefficients[1,][which(isolation_effect_fish_SS3_30_480_se>100)]

isolation_effect_fish_SS3_30_480[which(isolation_effect_fish_SS3_30_480_se>100)] <- 0
isolation_effect_fish_SS3_30_480_se[which(isolation_effect_fish_SS3_30_480_se>100)] <- 0

isolation_effect_fish_SS3_30_480
isolation_effect_fish_SS3_30_480_se

isolation_effect_fish_SS3_upper_30_480 <- isolation_effect_fish_SS3_30_480 + (z*isolation_effect_fish_SS3_30_480_se)
isolation_effect_fish_SS3_lower_30_480 <- isolation_effect_fish_SS3_30_480 - (z*isolation_effect_fish_SS3_30_480_se)


fit_SS3_trait_pred_interaction <- traitglm(L = com_SS3, R = env_SS3, Q = TRAITS_SS3, formula = ~ (relevel(fish_SS3, ref = "present"):trophic) * (isolation_SS3:trophic), method = "manyglm", col.intercepts = T)
trait_30_120_fish_SS3 <- fit_SS3_trait_pred_interaction$coefficients[(1+dim(TRAITS_SS3)[1]+4):(1+dim(TRAITS_SS3)[1]+4+1)]; names(trait_30_120_fish_SS3) <- c("Non_Pred", "Pred")
trait_30_120_fish_SS3_se <-fit_SS3_trait_pred_interaction$stderr.coefficients[(1+dim(TRAITS_SS3)[1]+4):(1+dim(TRAITS_SS3)[1]+4+1)]; names(trait_30_120_fish_SS3_se) <- c("Non_Pred", "Pred")
trait_30_120_fish_SS3_lower <- trait_30_120_fish_SS3 - z*trait_30_120_fish_SS3_se
trait_30_120_fish_SS3_upper <- trait_30_120_fish_SS3 + z*trait_30_120_fish_SS3_se

fit_SS3_trait_pred_interaction <- traitglm(L = com_SS3, R = env_SS3, Q = TRAITS_SS3, formula = ~ (relevel(fish_SS3, ref = "present"):trophic) * (relevel(isolation_SS3, ref = "120"):trophic), method = "manyglm", col.intercepts = T)
trait_120_480_fish_SS3 <- fit_SS3_trait_pred_interaction$coefficients[(1+dim(TRAITS_SS3)[1]+6):(1+dim(TRAITS_SS3)[1]+6+1)]; names(trait_120_480_fish_SS3) <- c("Non_Pred", "Pred")
trait_120_480_fish_SS3_se <- fit_SS3_trait_pred_interaction$stderr.coefficients[(1+dim(TRAITS_SS3)[1]+6):(1+dim(TRAITS_SS3)[1]+6+1)]; names(trait_120_480_fish_SS3_se) <- c("Non_Pred", "Pred")
trait_120_480_fish_SS3_lower <- trait_120_480_fish_SS3 - z*trait_120_480_fish_SS3_se
trait_120_480_fish_SS3_upper <- trait_120_480_fish_SS3 + z*trait_120_480_fish_SS3_se

fit_SS3_trait_pred_interaction <- traitglm(L = com_SS3, R = env_SS3, Q = TRAITS_SS3, formula = ~ (relevel(fish_SS3, ref = "present"):trophic) * (isolation_SS3:trophic), method = "manyglm", col.intercepts = T)
trait_30_480_fish_SS3 <- fit_SS3_trait_pred_interaction$coefficients[(1+dim(TRAITS_SS3)[1]+6):(1+dim(TRAITS_SS3)[1]+6+1)]; names(trait_30_480_fish_SS3) <- c("Non_Pred", "Pred")
trait_30_480_fish_SS3_se <- fit_SS3_trait_pred_interaction$stderr.coefficients[(1+dim(TRAITS_SS3)[1]+6):(1+dim(TRAITS_SS3)[1]+6+1)]; names(trait_30_480_fish_SS3_se) <- c("Non_Pred", "Pred")
trait_30_480_fish_SS3_lower <- trait_30_480_fish_SS3 - z*trait_30_480_fish_SS3_se
trait_30_480_fish_SS3_upper <- trait_30_480_fish_SS3 + z*trait_30_480_fish_SS3_se



##################################################################################================================================================
