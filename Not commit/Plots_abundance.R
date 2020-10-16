#This is for ploting abundances of species in each sampling survey


z<-qnorm(0.975, mean = 0, sd = 1)

#Plots by sampling survey
fit_time_1 <- manyglm(com_incomplete_mvabund ~ relevel(AM_incomplete, ref = "1"), family = "negative.binomial")
fit_time_2 <- manyglm(com_incomplete_mvabund ~ relevel(AM_incomplete, ref = "2"), family = "negative.binomial")
fit_time_3 <- manyglm(com_incomplete_mvabund ~ relevel(AM_incomplete, ref = "3"), family = "negative.binomial")

time_1_UPPER <- exp((fit_time_1$coefficients[1,]+(fit_time_1$stderr.coefficients[1,])*z)); time_1_UPPER[which(time_1_UPPER > 100000)] <- 0
time_1_LOWER <- exp((fit_time_1$coefficients[1,]-(fit_time_1$stderr.coefficients[1,])*z)); time_1_LOWER[which(time_1_LOWER < 0.000001)] <- 0
time_1 <- exp(fit_time_1$coefficients[1,]); time_1[which(time_1_LOWER < 0.000001)] <- 0

time_2_UPPER <- exp((fit_time_2$coefficients[1,]+(fit_time_2$stderr.coefficients[1,])*z)); time_2_UPPER[which(time_2_UPPER > 100000)] <- 0
time_2_LOWER <- exp((fit_time_2$coefficients[1,]-(fit_time_2$stderr.coefficients[1,])*z)); time_2_LOWER[which(time_2_LOWER < 0.000001)] <- 0
time_2 <- exp(fit_time_2$coefficients[1,]); time_2[which(time_2_LOWER < 0.000001)] <- 0


time_3_UPPER <- exp((fit_time_3$coefficients[1,]+(fit_time_3$stderr.coefficients[1,])*z)); time_3_UPPER[which(time_3_UPPER > 100000)] <- 0
time_3_LOWER <- exp((fit_time_3$coefficients[1,]-(fit_time_3$stderr.coefficients[1,])*z)); time_3_LOWER[which(time_3_LOWER < 0.000001)] <- 0
time_3 <- exp(fit_time_3$coefficients[1,]); time_3[which(time_3_LOWER < 0.000001)] <- 0

time_1 <- data.frame(time_1_LOWER,time_1,time_1_UPPER)
time_2 <- data.frame(time_2_LOWER,time_2,time_2_UPPER)
time_3 <- data.frame(time_3_LOWER,time_3,time_3_UPPER)

time_1_pred <- time_1[which(TRAITS$trophic=="Pr"),]
time_1_cons <- time_1[which(TRAITS$trophic=="Non_Pred"),]
time_1_pred <- time_1_pred[order(TRAITS$total_ab[which(TRAITS$trophic=="Pr")], decreasing = T),]
time_1_cons <- time_1_cons[order(TRAITS$total_ab[which(TRAITS$trophic=="Non_Pred")], decreasing = T),]
time_1 <- rbind(time_1_pred,time_1_cons)

time_2_pred <- time_2[which(TRAITS$trophic=="Pr"),]
time_2_cons <- time_2[which(TRAITS$trophic=="Non_Pred"),]
time_2_pred <- time_2_pred[order(TRAITS$total_ab[which(TRAITS$trophic=="Pr")], decreasing = T),]
time_2_cons <- time_2_cons[order(TRAITS$total_ab[which(TRAITS$trophic=="Non_Pred")], decreasing = T),]
time_2 <- rbind(time_2_pred,time_2_cons)

time_3_pred <- time_3[which(TRAITS$trophic=="Pr"),]
time_3_cons <- time_3[which(TRAITS$trophic=="Non_Pred"),]
time_3_pred <- time_3_pred[order(TRAITS$total_ab[which(TRAITS$trophic=="Pr")], decreasing = T),]
time_3_cons <- time_3_cons[order(TRAITS$total_ab[which(TRAITS$trophic=="Non_Pred")], decreasing = T),]
time_3 <- rbind(time_3_pred,time_3_cons)

#svg("Time_abundance.svg", width = 6, height = 20*0.8, pointsize = 12, family = "sans", bg = "transparent")
layout(matrix(c(1:20), 10, 2, byrow = TRUE))
par(mar = c(1,2,2,2))

colfunc<-colorRampPalette(c("darkslategray3","darkslategray"))
cols <- colfunc(5)

for(i in 1:dim(time_1)[1]){
  
  First <- 3; Second <- 3; Third <- 3
  if(time_1[i,2] < time_2[i,1] & time_2[i,2] > time_1[i,3]) {First <- First - 1}
  if(time_1[i,2] > time_2[i,3] & time_2[i,2] < time_1[i,1]) {First <- First + 1}
  if(time_1[i,2] < time_3[i,1] & time_3[i,2] > time_1[i,3]) {First <- First - 1}
  if(time_1[i,2] > time_3[i,3] & time_3[i,2] < time_1[i,1]) {First <- First + 1}
  if(time_2[i,2] < time_1[i,1] & time_1[i,2] > time_2[i,3]) {Second <- Second - 1}
  if(time_2[i,2] > time_1[i,3] & time_1[i,2] < time_2[i,1]) {Second <- Second + 1}
  if(time_2[i,2] < time_3[i,1] & time_3[i,2] > time_2[i,3]) {Second <- Second - 1}
  if(time_2[i,2] > time_3[i,3] & time_3[i,2] < time_2[i,1]) {Second <- Second + 1}
  if(time_3[i,2] < time_1[i,1] & time_1[i,2] > time_3[i,3]) {Third <- Third - 1}
  if(time_3[i,2] > time_1[i,3] & time_1[i,2] < time_3[i,1]) {Third <- Third + 1}
  if(time_3[i,2] < time_2[i,1] & time_2[i,2] > time_3[i,3]) {Third <- Third - 1}
  if(time_3[i,2] > time_2[i,3] & time_2[i,2] < time_3[i,1]) {Third <- Third + 1}
  col_sp <- c(First, Second, Third) - min(c(First, Second, Third))
  for(j in 1:3){
    if (col_sp[j] == 0) {col_sp[j]<- cols[1]}
    if (col_sp[j] == 1) {col_sp[j]<- cols[2]}
    if (col_sp[j] == 2) {col_sp[j]<- cols[3]}
    if (col_sp[j] == 3) {col_sp[j]<- cols[4]}
    if (col_sp[j] == 4) {col_sp[j]<- cols[5]}
  }
  if (time_1[i,2] == 0) {col_sp[1] <- "grey70"}
  if (time_2[i,2] == 0) {col_sp[2] <- "grey70"}
  if (time_3[i,2] == 0) {col_sp[3] <- "grey70"}
  #col_sp <- as.numeric(as.factor(c(First, Second, Third)))
  #col_sp[which(col_sp == "1")] <- "darkslategray3"
  #col_sp[which(col_sp == "2")] <- "darkslategray4"
  #col_sp[which(col_sp == "3")] <- "darkslategray"
  

plot(c(NA,NA),ylim = c(0,max(c(time_3[i,3], time_2[i,3],time_1[i,3]))), xlim = c(0.25,3.25), type = "p", xaxt = "n",ylab = "Abundance",xlab = "Sampling Survey", main = rownames(time_1)[i])
#abline(h = c(time_1[i,2],time_2[i,2], time_3[i,2]), lty = 2, col = "gray75")
lines(y = c(time_1[i,2],time_2[i,2]),x = c(1:2), type = "l", lty = 2, col = "darkslategray4", lwd  = 2)
lines(y = c(time_2[i,2],time_3[i,2]),x = c(2:3), type = "l", lty = 2, col = "darkslategray4", lwd  = 2)
points(y = c(time_1[i,2],time_2[i,2], time_3[i,2]), x = c(1,2,3), cex = 2, pch = 16, col = col_sp)
arrows(y0 = c(time_1[i,1],time_2[i,1], time_3[i,1]), y1 = c(time_1[i,3],time_2[i,3], time_3[i,3]), x1 = c(1:3), x0 = c(1:3), 
       code = 3, angle = 90, length = 0.05,col = col_sp, lwd = 2)
axis(1, at = c(1:3), labels = NULL, las = 1, cex.axis = 1)
box(lty=1, lwd=3)
}
#dev.off()





#FISH AND ISOLATION

#AM1

fit_AM1_30_fishless <- manyglm(com_AM1_mvabund ~ relevel(peixe_AM1, ref = "ausente") * relevel(isolation_AM1, ref = "30"), family = "negative.binomial")
fit_AM1_30_fish <- manyglm(com_AM1_mvabund ~ relevel(peixe_AM1, ref = "presente") * relevel(isolation_AM1, ref = "30"), family = "negative.binomial")

fit_AM1_120_fishless <- manyglm(com_AM1_mvabund ~ relevel(peixe_AM1, ref = "ausente") * relevel(isolation_AM1, ref = "120"), family = "negative.binomial")
fit_AM1_120_fish <- manyglm(com_AM1_mvabund ~ relevel(peixe_AM1, ref = "presente") * relevel(isolation_AM1, ref = "120"), family = "negative.binomial")

fit_AM1_480_fishless <- manyglm(com_AM1_mvabund ~ relevel(peixe_AM1, ref = "ausente") * relevel(isolation_AM1, ref = "480"), family = "negative.binomial")
fit_AM1_480_fish <- manyglm(com_AM1_mvabund ~ relevel(peixe_AM1, ref = "presente") * relevel(isolation_AM1, ref = "480"), family = "negative.binomial")


AM1_30_fishless_UPPER <- exp((fit_AM1_30_fishless$coefficients[1,]+(fit_AM1_30_fishless$stderr.coefficients[1,])*z)); AM1_30_fishless_UPPER[which(AM1_30_fishless_UPPER > 100000)] <- 0
AM1_30_fishless_LOWER <- exp((fit_AM1_30_fishless$coefficients[1,]-(fit_AM1_30_fishless$stderr.coefficients[1,])*z)); AM1_30_fishless_LOWER[which(AM1_30_fishless_LOWER < 0.000001)] <- 0
AM1_30_fishless <- exp(fit_AM1_30_fishless$coefficients[1,]); AM1_30_fishless[which(AM1_30_fishless_LOWER < 0.000001)] <- 0

AM1_30_fish_UPPER <- exp((fit_AM1_30_fish$coefficients[1,]+(fit_AM1_30_fish$stderr.coefficients[1,])*z)); AM1_30_fish_UPPER[which(AM1_30_fish_UPPER > 100000)] <- 0
AM1_30_fish_LOWER <- exp((fit_AM1_30_fish$coefficients[1,]-(fit_AM1_30_fish$stderr.coefficients[1,])*z)); AM1_30_fish_LOWER[which(AM1_30_fish_LOWER < 0.000001)] <- 0
AM1_30_fish <- exp(fit_AM1_30_fish$coefficients[1,]); AM1_30_fish[which(AM1_30_fish_LOWER < 0.000001)] <- 0


AM1_120_fishless_UPPER <- exp((fit_AM1_120_fishless$coefficients[1,]+(fit_AM1_120_fishless$stderr.coefficients[1,])*z)); AM1_120_fishless_UPPER[which(AM1_120_fishless_UPPER > 100000)] <- 0
AM1_120_fishless_LOWER <- exp((fit_AM1_120_fishless$coefficients[1,]-(fit_AM1_120_fishless$stderr.coefficients[1,])*z)); AM1_120_fishless_LOWER[which(AM1_120_fishless_LOWER < 0.000001)] <- 0
AM1_120_fishless <- exp(fit_AM1_120_fishless$coefficients[1,]); AM1_120_fishless[which(AM1_120_fishless_LOWER < 0.000001)] <- 0

AM1_120_fish_UPPER <- exp((fit_AM1_120_fish$coefficients[1,]+(fit_AM1_120_fish$stderr.coefficients[1,])*z)); AM1_120_fish_UPPER[which(AM1_120_fish_UPPER > 100000)] <- 0
AM1_120_fish_LOWER <- exp((fit_AM1_120_fish$coefficients[1,]-(fit_AM1_120_fish$stderr.coefficients[1,])*z)); AM1_120_fish_LOWER[which(AM1_120_fish_LOWER < 0.000001)] <- 0
AM1_120_fish <- exp(fit_AM1_120_fish$coefficients[1,]); AM1_120_fish[which(AM1_120_fish_LOWER < 0.000001)] <- 0


AM1_480_fishless_UPPER <- exp((fit_AM1_480_fishless$coefficients[1,]+(fit_AM1_480_fishless$stderr.coefficients[1,])*z)); AM1_480_fishless_UPPER[which(AM1_480_fishless_UPPER > 100000)] <- 0
AM1_480_fishless_LOWER <- exp((fit_AM1_480_fishless$coefficients[1,]-(fit_AM1_480_fishless$stderr.coefficients[1,])*z)); AM1_480_fishless_LOWER[which(AM1_480_fishless_LOWER < 0.000001)] <- 0
AM1_480_fishless <- exp(fit_AM1_480_fishless$coefficients[1,]); AM1_480_fishless[which(AM1_480_fishless_LOWER < 0.000001)] <- 0

AM1_480_fish_UPPER <- exp((fit_AM1_480_fish$coefficients[1,]+(fit_AM1_480_fish$stderr.coefficients[1,])*z)); AM1_480_fish_UPPER[which(AM1_480_fish_UPPER > 100000)] <- 0
AM1_480_fish_LOWER <- exp((fit_AM1_480_fish$coefficients[1,]-(fit_AM1_480_fish$stderr.coefficients[1,])*z)); AM1_480_fish_LOWER[which(AM1_480_fish_LOWER < 0.000001)] <- 0
AM1_480_fish <- exp(fit_AM1_480_fish$coefficients[1,]); AM1_480_fish[which(AM1_480_fish_LOWER < 0.000001)] <- 0


AM1_30_fishless <- data.frame(AM1_30_fishless_LOWER,AM1_30_fishless,AM1_30_fishless_UPPER)
AM1_30_fish <- data.frame(AM1_30_fish_LOWER,AM1_30_fish,AM1_30_fish_UPPER)

AM1_120_fishless <- data.frame(AM1_120_fishless_LOWER,AM1_120_fishless,AM1_120_fishless_UPPER)
AM1_120_fish <- data.frame(AM1_120_fish_LOWER,AM1_120_fish,AM1_120_fish_UPPER)

AM1_480_fishless <- data.frame(AM1_480_fishless_LOWER,AM1_480_fishless,AM1_480_fishless_UPPER)
AM1_480_fish <- data.frame(AM1_480_fish_LOWER,AM1_480_fish,AM1_480_fish_UPPER)


AM1_30_fishless_pred <- AM1_30_fishless[which(TRAITS_AM1$trophic=="Pr"),]
AM1_30_fishless_cons <- AM1_30_fishless[which(TRAITS_AM1$trophic=="Non_Pred"),]
AM1_30_fishless_pred <- AM1_30_fishless_pred[order(TRAITS_AM1$total_ab[which(TRAITS_AM1$trophic=="Pr")], decreasing = T),]
AM1_30_fishless_cons <- AM1_30_fishless_cons[order(TRAITS_AM1$total_ab[which(TRAITS_AM1$trophic=="Non_Pred")], decreasing = T),]
AM1_30_fishless <- rbind(AM1_30_fishless_pred,AM1_30_fishless_cons)

AM1_30_fish_pred <- AM1_30_fish[which(TRAITS_AM1$trophic=="Pr"),]
AM1_30_fish_cons <- AM1_30_fish[which(TRAITS_AM1$trophic=="Non_Pred"),]
AM1_30_fish_pred <- AM1_30_fish_pred[order(TRAITS_AM1$total_ab[which(TRAITS_AM1$trophic=="Pr")], decreasing = T),]
AM1_30_fish_cons <- AM1_30_fish_cons[order(TRAITS_AM1$total_ab[which(TRAITS_AM1$trophic=="Non_Pred")], decreasing = T),]
AM1_30_fish <- rbind(AM1_30_fish_pred,AM1_30_fish_cons)


AM1_120_fishless_pred <- AM1_120_fishless[which(TRAITS_AM1$trophic=="Pr"),]
AM1_120_fishless_cons <- AM1_120_fishless[which(TRAITS_AM1$trophic=="Non_Pred"),]
AM1_120_fishless_pred <- AM1_120_fishless_pred[order(TRAITS_AM1$total_ab[which(TRAITS_AM1$trophic=="Pr")], decreasing = T),]
AM1_120_fishless_cons <- AM1_120_fishless_cons[order(TRAITS_AM1$total_ab[which(TRAITS_AM1$trophic=="Non_Pred")], decreasing = T),]
AM1_120_fishless <- rbind(AM1_120_fishless_pred,AM1_120_fishless_cons)

AM1_120_fish_pred <- AM1_120_fish[which(TRAITS_AM1$trophic=="Pr"),]
AM1_120_fish_cons <- AM1_120_fish[which(TRAITS_AM1$trophic=="Non_Pred"),]
AM1_120_fish_pred <- AM1_120_fish_pred[order(TRAITS_AM1$total_ab[which(TRAITS_AM1$trophic=="Pr")], decreasing = T),]
AM1_120_fish_cons <- AM1_120_fish_cons[order(TRAITS_AM1$total_ab[which(TRAITS_AM1$trophic=="Non_Pred")], decreasing = T),]
AM1_120_fish <- rbind(AM1_120_fish_pred,AM1_120_fish_cons)


AM1_480_fishless_pred <- AM1_480_fishless[which(TRAITS_AM1$trophic=="Pr"),]
AM1_480_fishless_cons <- AM1_480_fishless[which(TRAITS_AM1$trophic=="Non_Pred"),]
AM1_480_fishless_pred <- AM1_480_fishless_pred[order(TRAITS_AM1$total_ab[which(TRAITS_AM1$trophic=="Pr")], decreasing = T),]
AM1_480_fishless_cons <- AM1_480_fishless_cons[order(TRAITS_AM1$total_ab[which(TRAITS_AM1$trophic=="Non_Pred")], decreasing = T),]
AM1_480_fishless <- rbind(AM1_480_fishless_pred,AM1_480_fishless_cons)

AM1_480_fish_pred <- AM1_480_fish[which(TRAITS_AM1$trophic=="Pr"),]
AM1_480_fish_cons <- AM1_480_fish[which(TRAITS_AM1$trophic=="Non_Pred"),]
AM1_480_fish_pred <- AM1_480_fish_pred[order(TRAITS_AM1$total_ab[which(TRAITS_AM1$trophic=="Pr")], decreasing = T),]
AM1_480_fish_cons <- AM1_480_fish_cons[order(TRAITS_AM1$total_ab[which(TRAITS_AM1$trophic=="Non_Pred")], decreasing = T),]
AM1_480_fish <- rbind(AM1_480_fish_pred,AM1_480_fish_cons)


dim(AM1_30_fishless)

#svg("AM1.svg", width = 6, height = 20*0.8, pointsize = 12, family = "sans", bg = "transparent")
layout(matrix(c(1:20), 10, 2, byrow = TRUE))
par(mar = c(1,2,2,2))

colfunc_fishless<-colorRampPalette(c("sienna2","sienna4"))
cols_fishless <- colfunc_fishless(5)

colfunc_fish<-colorRampPalette(c("dodgerblue2","dodgerblue4"))
cols_fish <- colfunc_fish(5)

for(i in 1:dim(AM1_30_fishless)[1]){
  
  Low_fishless <- 3; Moderate_fishless <- 3; High_fishless <- 3
  if(AM1_30_fishless[i,2] < AM1_120_fishless[i,1] & AM1_120_fishless[i,2] > AM1_30_fishless[i,3]) {Low_fishless <- Low_fishless - 1}
  if(AM1_30_fishless[i,2] > AM1_120_fishless[i,3] & AM1_120_fishless[i,2] < AM1_30_fishless[i,1]) {Low_fishless <- Low_fishless + 1}
  if(AM1_30_fishless[i,2] < AM1_480_fishless[i,1] & AM1_480_fishless[i,2] > AM1_30_fishless[i,3]) {Low_fishless <- Low_fishless - 1}
  if(AM1_30_fishless[i,2] > AM1_480_fishless[i,3] & AM1_480_fishless[i,2] < AM1_30_fishless[i,1]) {Low_fishless <- Low_fishless + 1}
  if(AM1_120_fishless[i,2] < AM1_30_fishless[i,1] & AM1_30_fishless[i,2] > AM1_120_fishless[i,3]) {Moderate_fishless <- Moderate_fishless - 1}
  if(AM1_120_fishless[i,2] > AM1_30_fishless[i,3] & AM1_30_fishless[i,2] < AM1_120_fishless[i,1]) {Moderate_fishless <- Moderate_fishless + 1}
  if(AM1_120_fishless[i,2] < AM1_480_fishless[i,1] & AM1_480_fishless[i,2] > AM1_120_fishless[i,3]) {Moderate_fishless <- Moderate_fishless - 1}
  if(AM1_120_fishless[i,2] > AM1_480_fishless[i,3] & AM1_480_fishless[i,2] < AM1_120_fishless[i,1]) {Moderate_fishless <- Moderate_fishless + 1}
  if(AM1_480_fishless[i,2] < AM1_30_fishless[i,1] & AM1_30_fishless[i,2] > AM1_480_fishless[i,3]) {High_fishless <- High_fishless - 1}
  if(AM1_480_fishless[i,2] > AM1_30_fishless[i,3] & AM1_30_fishless[i,2] < AM1_480_fishless[i,1]) {High_fishless <- High_fishless + 1}
  if(AM1_480_fishless[i,2] < AM1_120_fishless[i,1] & AM1_120_fishless[i,2] > AM1_480_fishless[i,3]) {High_fishless <- High_fishless - 1}
  if(AM1_480_fishless[i,2] > AM1_120_fishless[i,3] & AM1_120_fishless[i,2] < AM1_480_fishless[i,1]) {High_fishless <- High_fishless + 1}
  col_sp_fishless <- c(Low_fishless, Moderate_fishless, High_fishless) - min(c(Low_fishless, Moderate_fishless, High_fishless))
  for(j in 1:3){
    if (col_sp_fishless[j] == 0) {col_sp_fishless[j]<- cols_fishless[1]}
    if (col_sp_fishless[j] == 1) {col_sp_fishless[j]<- cols_fishless[2]}
    if (col_sp_fishless[j] == 2) {col_sp_fishless[j]<- cols_fishless[3]}
    if (col_sp_fishless[j] == 3) {col_sp_fishless[j]<- cols_fishless[4]}
    if (col_sp_fishless[j] == 4) {col_sp_fishless[j]<- cols_fishless[5]}
  }
  if (AM1_30_fishless[i,2] == 0) {col_sp_fishless[1] <- "grey70"}
  if (AM1_120_fishless[i,2] == 0) {col_sp_fishless[2] <- "grey70"}
  if (AM1_480_fishless[i,2] == 0) {col_sp_fishless[3] <- "grey70"}
  #col_sp_fishless <- as.numeric(as.factor(c(Low_fishless, Moderate_fishless, High_fishless)))
  #col_sp_fishless[which(col_sp_fishless == "1")] <- "sienna2"
  #col_sp_fishless[which(col_sp_fishless == "2")] <- "sienna3"
  #col_sp_fishless[which(col_sp_fishless == "3")] <- "sienna4"
  
  
  Low_fish <- 3; Moderate_fish <- 3; High_fish <- 3
  if(AM1_30_fish[i,2] < AM1_120_fish[i,1] & AM1_120_fish[i,2] > AM1_30_fish[i,3]) {Low_fish <- Low_fish - 1}
  if(AM1_30_fish[i,2] > AM1_120_fish[i,3] & AM1_120_fish[i,2] < AM1_30_fish[i,1]) {Low_fish <- Low_fish + 1}
  if(AM1_30_fish[i,2] < AM1_480_fish[i,1] & AM1_480_fish[i,2] > AM1_30_fish[i,3]) {Low_fish <- Low_fish - 1}
  if(AM1_30_fish[i,2] > AM1_480_fish[i,3] & AM1_480_fish[i,2] < AM1_30_fish[i,1]) {Low_fish <- Low_fish + 1}
  if(AM1_120_fish[i,2] < AM1_30_fish[i,1] & AM1_30_fish[i,2] > AM1_120_fish[i,3]) {Moderate_fish <- Moderate_fish - 1}
  if(AM1_120_fish[i,2] > AM1_30_fish[i,3] & AM1_30_fish[i,2] < AM1_120_fish[i,1]) {Moderate_fish <- Moderate_fish + 1}
  if(AM1_120_fish[i,2] < AM1_480_fish[i,1] & AM1_480_fish[i,2] > AM1_120_fish[i,3]) {Moderate_fish <- Moderate_fish - 1}
  if(AM1_120_fish[i,2] > AM1_480_fish[i,3] & AM1_480_fish[i,2] < AM1_120_fish[i,1]) {Moderate_fish <- Moderate_fish + 1}
  if(AM1_480_fish[i,2] < AM1_30_fish[i,1] & AM1_30_fish[i,2] > AM1_480_fish[i,3]) {High_fish <- High_fish - 1}
  if(AM1_480_fish[i,2] > AM1_30_fish[i,3] & AM1_30_fish[i,2] < AM1_480_fish[i,1]) {High_fish <- High_fish + 1}
  if(AM1_480_fish[i,2] < AM1_120_fish[i,1] & AM1_120_fish[i,2] > AM1_480_fish[i,3]) {High_fish <- High_fish - 1}
  if(AM1_480_fish[i,2] > AM1_120_fish[i,3] & AM1_120_fish[i,2] < AM1_480_fish[i,1]) {High_fish <- High_fish + 1}
  col_sp_fish <- c(Low_fish, Moderate_fish, High_fish) - min(c(Low_fish, Moderate_fish, High_fish))
  for(j in 1:3){
    if (col_sp_fish[j] == 0) {col_sp_fish[j]<- cols_fish[1]}
    if (col_sp_fish[j] == 1) {col_sp_fish[j]<- cols_fish[2]}
    if (col_sp_fish[j] == 2) {col_sp_fish[j]<- cols_fish[3]}
    if (col_sp_fish[j] == 3) {col_sp_fish[j]<- cols_fish[4]}
    if (col_sp_fish[j] == 4) {col_sp_fish[j]<- cols_fish[5]}
  }
  if (AM1_30_fish[i,2] == 0) {col_sp_fish[1] <- "grey70"}
  if (AM1_120_fish[i,2] == 0) {col_sp_fish[2] <- "grey70"}
  if (AM1_480_fish[i,2] == 0) {col_sp_fish[3] <- "grey70"}
  #col_sp_fish <- as.numeric(as.factor(c(Low_fish, Moderate_fish, High_fish)))
  #col_sp_fish[which(col_sp_fish == "1")] <- "dodgerblue2"
  #col_sp_fish[which(col_sp_fish == "2")] <- "dodgerblue3"
  #col_sp_fish[which(col_sp_fish == "3")] <- "dodgerblue4"
  
  
  
  plot(c(NA,NA),ylim = c(0,max(c(AM1_480_fishless[i,3], AM1_120_fishless[i,3],AM1_30_fishless[i,3],AM1_480_fish[i,3], AM1_120_fish[i,3],AM1_30_fish[i,3]))), xlim = c(0.25,3.055), type = "p", xaxt = "n",ylab = "Abundance",xlab = "Isolation", main = rownames(AM1_30_fishless)[i])
  #abline(h = c(AM1_30_fishless[i,2],AM1_120_fishless[i,2], AM1_480_fishless[i,2],AM1_480_fishless[i,2], AM1_120_fishless[i,2],AM1_30_fishless[i,2]), lty = 2, col = "gray75")
  
  lines(y = c(AM1_30_fishless[i,2],AM1_120_fishless[i,2]),x = c(1.05:2.05), type = "l", lty = 2, col = "sienna3", lwd  = 2)
  lines(y = c(AM1_120_fishless[i,2],AM1_480_fishless[i,2]),x = c(2.05:3.05), type = "l", lty = 2, col = "sienna3", lwd  = 2)
  lines(y = c(AM1_30_fish[i,2],AM1_120_fish[i,2]),x = c(0.95:1.95), type = "l", lty = 2, col = "dodgerblue3", lwd  = 2)
  lines(y = c(AM1_120_fish[i,2],AM1_480_fish[i,2]),x = c(1.95:2.95), type = "l", lty = 2, col = "dodgerblue3", lwd  = 2)
  
  points(y = c(AM1_30_fishless[i,2],AM1_120_fishless[i,2], AM1_480_fishless[i,2]), x = c(1.05,2.05,3.05), cex = 2, pch = 16, col = col_sp_fishless)
  points(y = c(AM1_30_fish[i,2],AM1_120_fish[i,2], AM1_480_fish[i,2]), x = c(0.95,1.95,2.95), cex = 2, pch = 17, col = col_sp_fish)
  
  arrows(y0 = c(AM1_30_fishless[i,1],AM1_120_fishless[i,1], AM1_480_fishless[i,1]), y1 = c(AM1_30_fishless[i,3],AM1_120_fishless[i,3], AM1_480_fishless[i,3]), x1 = c(1.05,2.05,3.05), x0 = c(1.05,2.05,3.05), 
         code = 3, angle = 90, length = 0.05,col = col_sp_fishless, lwd = 2)
  arrows(y0 = c(AM1_30_fish[i,1],AM1_120_fish[i,1], AM1_480_fish[i,1]), y1 = c(AM1_30_fish[i,3],AM1_120_fish[i,3], AM1_480_fish[i,3]), x1 = c(0.95,1.95,2.95), x0 = c(0.95,1.95,2.95), 
         code = 3, angle = 90, length = 0.05,col = col_sp_fish, lwd = 2)
  axis(1, at = c(1:3), labels = c("30 m","120 m","480 m"), las = 1, cex.axis = 1)
  box(lty=1, lwd=3)
}
#dev.off()








#AM2

fit_AM2_30_fishless <- manyglm(com_AM2_mvabund ~ relevel(peixe_AM2, ref = "ausente") * relevel(isolation_AM2, ref = "30"), family = "negative.binomial")
fit_AM2_30_fish <- manyglm(com_AM2_mvabund ~ relevel(peixe_AM2, ref = "presente") * relevel(isolation_AM2, ref = "30"), family = "negative.binomial")

fit_AM2_120_fishless <- manyglm(com_AM2_mvabund ~ relevel(peixe_AM2, ref = "ausente") * relevel(isolation_AM2, ref = "120"), family = "negative.binomial")
fit_AM2_120_fish <- manyglm(com_AM2_mvabund ~ relevel(peixe_AM2, ref = "presente") * relevel(isolation_AM2, ref = "120"), family = "negative.binomial")

fit_AM2_480_fishless <- manyglm(com_AM2_mvabund ~ relevel(peixe_AM2, ref = "ausente") * relevel(isolation_AM2, ref = "480"), family = "negative.binomial")
fit_AM2_480_fish <- manyglm(com_AM2_mvabund ~ relevel(peixe_AM2, ref = "presente") * relevel(isolation_AM2, ref = "480"), family = "negative.binomial")


AM2_30_fishless_UPPER <- exp((fit_AM2_30_fishless$coefficients[1,]+(fit_AM2_30_fishless$stderr.coefficients[1,])*z)); AM2_30_fishless_UPPER[which(AM2_30_fishless_UPPER > 100000)] <- 0
AM2_30_fishless_LOWER <- exp((fit_AM2_30_fishless$coefficients[1,]-(fit_AM2_30_fishless$stderr.coefficients[1,])*z)); AM2_30_fishless_LOWER[which(AM2_30_fishless_LOWER < 0.000001)] <- 0
AM2_30_fishless <- exp(fit_AM2_30_fishless$coefficients[1,]); AM2_30_fishless[which(AM2_30_fishless_LOWER < 0.000001)] <- 0

AM2_30_fish_UPPER <- exp((fit_AM2_30_fish$coefficients[1,]+(fit_AM2_30_fish$stderr.coefficients[1,])*z)); AM2_30_fish_UPPER[which(AM2_30_fish_UPPER > 100000)] <- 0
AM2_30_fish_LOWER <- exp((fit_AM2_30_fish$coefficients[1,]-(fit_AM2_30_fish$stderr.coefficients[1,])*z)); AM2_30_fish_LOWER[which(AM2_30_fish_LOWER < 0.000001)] <- 0
AM2_30_fish <- exp(fit_AM2_30_fish$coefficients[1,]); AM2_30_fish[which(AM2_30_fish_LOWER < 0.000001)] <- 0


AM2_120_fishless_UPPER <- exp((fit_AM2_120_fishless$coefficients[1,]+(fit_AM2_120_fishless$stderr.coefficients[1,])*z)); AM2_120_fishless_UPPER[which(AM2_120_fishless_UPPER > 100000)] <- 0
AM2_120_fishless_LOWER <- exp((fit_AM2_120_fishless$coefficients[1,]-(fit_AM2_120_fishless$stderr.coefficients[1,])*z)); AM2_120_fishless_LOWER[which(AM2_120_fishless_LOWER < 0.000001)] <- 0
AM2_120_fishless <- exp(fit_AM2_120_fishless$coefficients[1,]); AM2_120_fishless[which(AM2_120_fishless_LOWER < 0.000001)] <- 0

AM2_120_fish_UPPER <- exp((fit_AM2_120_fish$coefficients[1,]+(fit_AM2_120_fish$stderr.coefficients[1,])*z)); AM2_120_fish_UPPER[which(AM2_120_fish_UPPER > 100000)] <- 0
AM2_120_fish_LOWER <- exp((fit_AM2_120_fish$coefficients[1,]-(fit_AM2_120_fish$stderr.coefficients[1,])*z)); AM2_120_fish_LOWER[which(AM2_120_fish_LOWER < 0.000001)] <- 0
AM2_120_fish <- exp(fit_AM2_120_fish$coefficients[1,]); AM2_120_fish[which(AM2_120_fish_LOWER < 0.000001)] <- 0


AM2_480_fishless_UPPER <- exp((fit_AM2_480_fishless$coefficients[1,]+(fit_AM2_480_fishless$stderr.coefficients[1,])*z)); AM2_480_fishless_UPPER[which(AM2_480_fishless_UPPER > 100000)] <- 0
AM2_480_fishless_LOWER <- exp((fit_AM2_480_fishless$coefficients[1,]-(fit_AM2_480_fishless$stderr.coefficients[1,])*z)); AM2_480_fishless_LOWER[which(AM2_480_fishless_LOWER < 0.000001)] <- 0
AM2_480_fishless <- exp(fit_AM2_480_fishless$coefficients[1,]); AM2_480_fishless[which(AM2_480_fishless_LOWER < 0.000001)] <- 0

AM2_480_fish_UPPER <- exp((fit_AM2_480_fish$coefficients[1,]+(fit_AM2_480_fish$stderr.coefficients[1,])*z)); AM2_480_fish_UPPER[which(AM2_480_fish_UPPER > 100000)] <- 0
AM2_480_fish_LOWER <- exp((fit_AM2_480_fish$coefficients[1,]-(fit_AM2_480_fish$stderr.coefficients[1,])*z)); AM2_480_fish_LOWER[which(AM2_480_fish_LOWER < 0.000001)] <- 0
AM2_480_fish <- exp(fit_AM2_480_fish$coefficients[1,]); AM2_480_fish[which(AM2_480_fish_LOWER < 0.000001)] <- 0


AM2_30_fishless <- data.frame(AM2_30_fishless_LOWER,AM2_30_fishless,AM2_30_fishless_UPPER)
AM2_30_fish <- data.frame(AM2_30_fish_LOWER,AM2_30_fish,AM2_30_fish_UPPER)

AM2_120_fishless <- data.frame(AM2_120_fishless_LOWER,AM2_120_fishless,AM2_120_fishless_UPPER)
AM2_120_fish <- data.frame(AM2_120_fish_LOWER,AM2_120_fish,AM2_120_fish_UPPER)

AM2_480_fishless <- data.frame(AM2_480_fishless_LOWER,AM2_480_fishless,AM2_480_fishless_UPPER)
AM2_480_fish <- data.frame(AM2_480_fish_LOWER,AM2_480_fish,AM2_480_fish_UPPER)


AM2_30_fishless_pred <- AM2_30_fishless[which(TRAITS_AM2$trophic=="Pr"),]
AM2_30_fishless_cons <- AM2_30_fishless[which(TRAITS_AM2$trophic=="Non_Pred"),]
AM2_30_fishless_pred <- AM2_30_fishless_pred[order(TRAITS_AM2$total_ab[which(TRAITS_AM2$trophic=="Pr")], decreasing = T),]
AM2_30_fishless_cons <- AM2_30_fishless_cons[order(TRAITS_AM2$total_ab[which(TRAITS_AM2$trophic=="Non_Pred")], decreasing = T),]
AM2_30_fishless <- rbind(AM2_30_fishless_pred,AM2_30_fishless_cons)

AM2_30_fish_pred <- AM2_30_fish[which(TRAITS_AM2$trophic=="Pr"),]
AM2_30_fish_cons <- AM2_30_fish[which(TRAITS_AM2$trophic=="Non_Pred"),]
AM2_30_fish_pred <- AM2_30_fish_pred[order(TRAITS_AM2$total_ab[which(TRAITS_AM2$trophic=="Pr")], decreasing = T),]
AM2_30_fish_cons <- AM2_30_fish_cons[order(TRAITS_AM2$total_ab[which(TRAITS_AM2$trophic=="Non_Pred")], decreasing = T),]
AM2_30_fish <- rbind(AM2_30_fish_pred,AM2_30_fish_cons)


AM2_120_fishless_pred <- AM2_120_fishless[which(TRAITS_AM2$trophic=="Pr"),]
AM2_120_fishless_cons <- AM2_120_fishless[which(TRAITS_AM2$trophic=="Non_Pred"),]
AM2_120_fishless_pred <- AM2_120_fishless_pred[order(TRAITS_AM2$total_ab[which(TRAITS_AM2$trophic=="Pr")], decreasing = T),]
AM2_120_fishless_cons <- AM2_120_fishless_cons[order(TRAITS_AM2$total_ab[which(TRAITS_AM2$trophic=="Non_Pred")], decreasing = T),]
AM2_120_fishless <- rbind(AM2_120_fishless_pred,AM2_120_fishless_cons)

AM2_120_fish_pred <- AM2_120_fish[which(TRAITS_AM2$trophic=="Pr"),]
AM2_120_fish_cons <- AM2_120_fish[which(TRAITS_AM2$trophic=="Non_Pred"),]
AM2_120_fish_pred <- AM2_120_fish_pred[order(TRAITS_AM2$total_ab[which(TRAITS_AM2$trophic=="Pr")], decreasing = T),]
AM2_120_fish_cons <- AM2_120_fish_cons[order(TRAITS_AM2$total_ab[which(TRAITS_AM2$trophic=="Non_Pred")], decreasing = T),]
AM2_120_fish <- rbind(AM2_120_fish_pred,AM2_120_fish_cons)


AM2_480_fishless_pred <- AM2_480_fishless[which(TRAITS_AM2$trophic=="Pr"),]
AM2_480_fishless_cons <- AM2_480_fishless[which(TRAITS_AM2$trophic=="Non_Pred"),]
AM2_480_fishless_pred <- AM2_480_fishless_pred[order(TRAITS_AM2$total_ab[which(TRAITS_AM2$trophic=="Pr")], decreasing = T),]
AM2_480_fishless_cons <- AM2_480_fishless_cons[order(TRAITS_AM2$total_ab[which(TRAITS_AM2$trophic=="Non_Pred")], decreasing = T),]
AM2_480_fishless <- rbind(AM2_480_fishless_pred,AM2_480_fishless_cons)

AM2_480_fish_pred <- AM2_480_fish[which(TRAITS_AM2$trophic=="Pr"),]
AM2_480_fish_cons <- AM2_480_fish[which(TRAITS_AM2$trophic=="Non_Pred"),]
AM2_480_fish_pred <- AM2_480_fish_pred[order(TRAITS_AM2$total_ab[which(TRAITS_AM2$trophic=="Pr")], decreasing = T),]
AM2_480_fish_cons <- AM2_480_fish_cons[order(TRAITS_AM2$total_ab[which(TRAITS_AM2$trophic=="Non_Pred")], decreasing = T),]
AM2_480_fish <- rbind(AM2_480_fish_pred,AM2_480_fish_cons)


dim(AM2_30_fishless)

#svg("AM2.svg", width = 6, height = 20*0.8, pointsize = 12, family = "sans", bg = "transparent")
layout(matrix(c(1:20), 10, 2, byrow = TRUE))
par(mar = c(1,2,2,2))

colfunc_fishless<-colorRampPalette(c("sienna2","sienna4"))
cols_fishless <- colfunc_fishless(5)

colfunc_fish<-colorRampPalette(c("dodgerblue2","dodgerblue4"))
cols_fish <- colfunc_fish(5)

for(i in 1:dim(AM2_30_fishless)[1]){
  
  Low_fishless <- 3; Moderate_fishless <- 3; High_fishless <- 3
  if(AM2_30_fishless[i,2] < AM2_120_fishless[i,1] & AM2_120_fishless[i,2] > AM2_30_fishless[i,3]) {Low_fishless <- Low_fishless - 1}
  if(AM2_30_fishless[i,2] > AM2_120_fishless[i,3] & AM2_120_fishless[i,2] < AM2_30_fishless[i,1]) {Low_fishless <- Low_fishless + 1}
  if(AM2_30_fishless[i,2] < AM2_480_fishless[i,1] & AM2_480_fishless[i,2] > AM2_30_fishless[i,3]) {Low_fishless <- Low_fishless - 1}
  if(AM2_30_fishless[i,2] > AM2_480_fishless[i,3] & AM2_480_fishless[i,2] < AM2_30_fishless[i,1]) {Low_fishless <- Low_fishless + 1}
  if(AM2_120_fishless[i,2] < AM2_30_fishless[i,1] & AM2_30_fishless[i,2] > AM2_120_fishless[i,3]) {Moderate_fishless <- Moderate_fishless - 1}
  if(AM2_120_fishless[i,2] > AM2_30_fishless[i,3] & AM2_30_fishless[i,2] < AM2_120_fishless[i,1]) {Moderate_fishless <- Moderate_fishless + 1}
  if(AM2_120_fishless[i,2] < AM2_480_fishless[i,1] & AM2_480_fishless[i,2] > AM2_120_fishless[i,3]) {Moderate_fishless <- Moderate_fishless - 1}
  if(AM2_120_fishless[i,2] > AM2_480_fishless[i,3] & AM2_480_fishless[i,2] < AM2_120_fishless[i,1]) {Moderate_fishless <- Moderate_fishless + 1}
  if(AM2_480_fishless[i,2] < AM2_30_fishless[i,1] & AM2_30_fishless[i,2] > AM2_480_fishless[i,3]) {High_fishless <- High_fishless - 1}
  if(AM2_480_fishless[i,2] > AM2_30_fishless[i,3] & AM2_30_fishless[i,2] < AM2_480_fishless[i,1]) {High_fishless <- High_fishless + 1}
  if(AM2_480_fishless[i,2] < AM2_120_fishless[i,1] & AM2_120_fishless[i,2] > AM2_480_fishless[i,3]) {High_fishless <- High_fishless - 1}
  if(AM2_480_fishless[i,2] > AM2_120_fishless[i,3] & AM2_120_fishless[i,2] < AM2_480_fishless[i,1]) {High_fishless <- High_fishless + 1}
  col_sp_fishless <- c(Low_fishless, Moderate_fishless, High_fishless) - min(c(Low_fishless, Moderate_fishless, High_fishless))
  for(j in 1:3){
    if (col_sp_fishless[j] == 0) {col_sp_fishless[j]<- cols_fishless[1]}
    if (col_sp_fishless[j] == 1) {col_sp_fishless[j]<- cols_fishless[2]}
    if (col_sp_fishless[j] == 2) {col_sp_fishless[j]<- cols_fishless[3]}
    if (col_sp_fishless[j] == 3) {col_sp_fishless[j]<- cols_fishless[4]}
    if (col_sp_fishless[j] == 4) {col_sp_fishless[j]<- cols_fishless[5]}
  }
  if (AM2_30_fishless[i,2] == 0) {col_sp_fishless[1] <- "grey70"}
  if (AM2_120_fishless[i,2] == 0) {col_sp_fishless[2] <- "grey70"}
  if (AM2_480_fishless[i,2] == 0) {col_sp_fishless[3] <- "grey70"}
  #col_sp_fishless <- as.numeric(as.factor(c(Low_fishless, Moderate_fishless, High_fishless)))
  #col_sp_fishless[which(col_sp_fishless == "1")] <- "sienna2"
  #col_sp_fishless[which(col_sp_fishless == "2")] <- "sienna3"
  #col_sp_fishless[which(col_sp_fishless == "3")] <- "sienna4"
  
  
  Low_fish <- 3; Moderate_fish <- 3; High_fish <- 3
  if(AM2_30_fish[i,2] < AM2_120_fish[i,1] & AM2_120_fish[i,2] > AM2_30_fish[i,3]) {Low_fish <- Low_fish - 1}
  if(AM2_30_fish[i,2] > AM2_120_fish[i,3] & AM2_120_fish[i,2] < AM2_30_fish[i,1]) {Low_fish <- Low_fish + 1}
  if(AM2_30_fish[i,2] < AM2_480_fish[i,1] & AM2_480_fish[i,2] > AM2_30_fish[i,3]) {Low_fish <- Low_fish - 1}
  if(AM2_30_fish[i,2] > AM2_480_fish[i,3] & AM2_480_fish[i,2] < AM2_30_fish[i,1]) {Low_fish <- Low_fish + 1}
  if(AM2_120_fish[i,2] < AM2_30_fish[i,1] & AM2_30_fish[i,2] > AM2_120_fish[i,3]) {Moderate_fish <- Moderate_fish - 1}
  if(AM2_120_fish[i,2] > AM2_30_fish[i,3] & AM2_30_fish[i,2] < AM2_120_fish[i,1]) {Moderate_fish <- Moderate_fish + 1}
  if(AM2_120_fish[i,2] < AM2_480_fish[i,1] & AM2_480_fish[i,2] > AM2_120_fish[i,3]) {Moderate_fish <- Moderate_fish - 1}
  if(AM2_120_fish[i,2] > AM2_480_fish[i,3] & AM2_480_fish[i,2] < AM2_120_fish[i,1]) {Moderate_fish <- Moderate_fish + 1}
  if(AM2_480_fish[i,2] < AM2_30_fish[i,1] & AM2_30_fish[i,2] > AM2_480_fish[i,3]) {High_fish <- High_fish - 1}
  if(AM2_480_fish[i,2] > AM2_30_fish[i,3] & AM2_30_fish[i,2] < AM2_480_fish[i,1]) {High_fish <- High_fish + 1}
  if(AM2_480_fish[i,2] < AM2_120_fish[i,1] & AM2_120_fish[i,2] > AM2_480_fish[i,3]) {High_fish <- High_fish - 1}
  if(AM2_480_fish[i,2] > AM2_120_fish[i,3] & AM2_120_fish[i,2] < AM2_480_fish[i,1]) {High_fish <- High_fish + 1}
  col_sp_fish <- c(Low_fish, Moderate_fish, High_fish) - min(c(Low_fish, Moderate_fish, High_fish))
  for(j in 1:3){
    if (col_sp_fish[j] == 0) {col_sp_fish[j]<- cols_fish[1]}
    if (col_sp_fish[j] == 1) {col_sp_fish[j]<- cols_fish[2]}
    if (col_sp_fish[j] == 2) {col_sp_fish[j]<- cols_fish[3]}
    if (col_sp_fish[j] == 3) {col_sp_fish[j]<- cols_fish[4]}
    if (col_sp_fish[j] == 4) {col_sp_fish[j]<- cols_fish[5]}
  }
  if (AM2_30_fish[i,2] == 0) {col_sp_fish[1] <- "grey70"}
  if (AM2_120_fish[i,2] == 0) {col_sp_fish[2] <- "grey70"}
  if (AM2_480_fish[i,2] == 0) {col_sp_fish[3] <- "grey70"}
  #col_sp_fish <- as.numeric(as.factor(c(Low_fish, Moderate_fish, High_fish)))
  #col_sp_fish[which(col_sp_fish == "1")] <- "dodgerblue2"
  #col_sp_fish[which(col_sp_fish == "2")] <- "dodgerblue3"
  #col_sp_fish[which(col_sp_fish == "3")] <- "dodgerblue4"
  
  
  
  plot(c(NA,NA),ylim = c(0,max(c(AM2_480_fishless[i,3], AM2_120_fishless[i,3],AM2_30_fishless[i,3],AM2_480_fish[i,3], AM2_120_fish[i,3],AM2_30_fish[i,3]))), xlim = c(0.25,3.055), type = "p", xaxt = "n",ylab = "Abundance",xlab = "Isolation", main = rownames(AM2_30_fishless)[i])
  #abline(h = c(AM2_30_fishless[i,2],AM2_120_fishless[i,2], AM2_480_fishless[i,2],AM2_480_fishless[i,2], AM2_120_fishless[i,2],AM2_30_fishless[i,2]), lty = 2, col = "gray75")
  
  lines(y = c(AM2_30_fishless[i,2],AM2_120_fishless[i,2]),x = c(1.05:2.05), type = "l", lty = 2, col = "sienna3", lwd  = 2)
  lines(y = c(AM2_120_fishless[i,2],AM2_480_fishless[i,2]),x = c(2.05:3.05), type = "l", lty = 2, col = "sienna3", lwd  = 2)
  lines(y = c(AM2_30_fish[i,2],AM2_120_fish[i,2]),x = c(0.95:1.95), type = "l", lty = 2, col = "dodgerblue3", lwd  = 2)
  lines(y = c(AM2_120_fish[i,2],AM2_480_fish[i,2]),x = c(1.95:2.95), type = "l", lty = 2, col = "dodgerblue3", lwd  = 2)
  
  points(y = c(AM2_30_fishless[i,2],AM2_120_fishless[i,2], AM2_480_fishless[i,2]), x = c(1.05,2.05,3.05), cex = 2, pch = 16, col = col_sp_fishless)
  points(y = c(AM2_30_fish[i,2],AM2_120_fish[i,2], AM2_480_fish[i,2]), x = c(0.95,1.95,2.95), cex = 2, pch = 17, col = col_sp_fish)
  
  arrows(y0 = c(AM2_30_fishless[i,1],AM2_120_fishless[i,1], AM2_480_fishless[i,1]), y1 = c(AM2_30_fishless[i,3],AM2_120_fishless[i,3], AM2_480_fishless[i,3]), x1 = c(1.05,2.05,3.05), x0 = c(1.05,2.05,3.05), 
         code = 3, angle = 90, length = 0.05,col = col_sp_fishless, lwd = 2)
  arrows(y0 = c(AM2_30_fish[i,1],AM2_120_fish[i,1], AM2_480_fish[i,1]), y1 = c(AM2_30_fish[i,3],AM2_120_fish[i,3], AM2_480_fish[i,3]), x1 = c(0.95,1.95,2.95), x0 = c(0.95,1.95,2.95), 
         code = 3, angle = 90, length = 0.05,col = col_sp_fish, lwd = 2)
  axis(1, at = c(1:3), labels = c("30 m","120 m","480 m"), las = 1, cex.axis = 1)
  box(lty=1, lwd=3)
}
#dev.off()




#AM3

fit_AM3_30_fishless <- manyglm(com_AM3_mvabund ~ relevel(peixe_AM3, ref = "ausente") * relevel(isolation_AM3, ref = "30"), family = "negative.binomial")
fit_AM3_30_fish <- manyglm(com_AM3_mvabund ~ relevel(peixe_AM3, ref = "presente") * relevel(isolation_AM3, ref = "30"), family = "negative.binomial")

fit_AM3_120_fishless <- manyglm(com_AM3_mvabund ~ relevel(peixe_AM3, ref = "ausente") * relevel(isolation_AM3, ref = "120"), family = "negative.binomial")
fit_AM3_120_fish <- manyglm(com_AM3_mvabund ~ relevel(peixe_AM3, ref = "presente") * relevel(isolation_AM3, ref = "120"), family = "negative.binomial")

fit_AM3_480_fishless <- manyglm(com_AM3_mvabund ~ relevel(peixe_AM3, ref = "ausente") * relevel(isolation_AM3, ref = "480"), family = "negative.binomial")
fit_AM3_480_fish <- manyglm(com_AM3_mvabund ~ relevel(peixe_AM3, ref = "presente") * relevel(isolation_AM3, ref = "480"), family = "negative.binomial")


AM3_30_fishless_UPPER <- exp((fit_AM3_30_fishless$coefficients[1,]+(fit_AM3_30_fishless$stderr.coefficients[1,])*z)); AM3_30_fishless_UPPER[which(AM3_30_fishless_UPPER > 100000)] <- 0
AM3_30_fishless_LOWER <- exp((fit_AM3_30_fishless$coefficients[1,]-(fit_AM3_30_fishless$stderr.coefficients[1,])*z)); AM3_30_fishless_LOWER[which(AM3_30_fishless_LOWER < 0.000001)] <- 0
AM3_30_fishless <- exp(fit_AM3_30_fishless$coefficients[1,]); AM3_30_fishless[which(AM3_30_fishless_LOWER < 0.000001)] <- 0

AM3_30_fish_UPPER <- exp((fit_AM3_30_fish$coefficients[1,]+(fit_AM3_30_fish$stderr.coefficients[1,])*z)); AM3_30_fish_UPPER[which(AM3_30_fish_UPPER > 100000)] <- 0
AM3_30_fish_LOWER <- exp((fit_AM3_30_fish$coefficients[1,]-(fit_AM3_30_fish$stderr.coefficients[1,])*z)); AM3_30_fish_LOWER[which(AM3_30_fish_LOWER < 0.000001)] <- 0
AM3_30_fish <- exp(fit_AM3_30_fish$coefficients[1,]); AM3_30_fish[which(AM3_30_fish_LOWER < 0.000001)] <- 0


AM3_120_fishless_UPPER <- exp((fit_AM3_120_fishless$coefficients[1,]+(fit_AM3_120_fishless$stderr.coefficients[1,])*z)); AM3_120_fishless_UPPER[which(AM3_120_fishless_UPPER > 100000)] <- 0
AM3_120_fishless_LOWER <- exp((fit_AM3_120_fishless$coefficients[1,]-(fit_AM3_120_fishless$stderr.coefficients[1,])*z)); AM3_120_fishless_LOWER[which(AM3_120_fishless_LOWER < 0.000001)] <- 0
AM3_120_fishless <- exp(fit_AM3_120_fishless$coefficients[1,]); AM3_120_fishless[which(AM3_120_fishless_LOWER < 0.000001)] <- 0

AM3_120_fish_UPPER <- exp((fit_AM3_120_fish$coefficients[1,]+(fit_AM3_120_fish$stderr.coefficients[1,])*z)); AM3_120_fish_UPPER[which(AM3_120_fish_UPPER > 100000)] <- 0
AM3_120_fish_LOWER <- exp((fit_AM3_120_fish$coefficients[1,]-(fit_AM3_120_fish$stderr.coefficients[1,])*z)); AM3_120_fish_LOWER[which(AM3_120_fish_LOWER < 0.000001)] <- 0
AM3_120_fish <- exp(fit_AM3_120_fish$coefficients[1,]); AM3_120_fish[which(AM3_120_fish_LOWER < 0.000001)] <- 0


AM3_480_fishless_UPPER <- exp((fit_AM3_480_fishless$coefficients[1,]+(fit_AM3_480_fishless$stderr.coefficients[1,])*z)); AM3_480_fishless_UPPER[which(AM3_480_fishless_UPPER > 100000)] <- 0
AM3_480_fishless_LOWER <- exp((fit_AM3_480_fishless$coefficients[1,]-(fit_AM3_480_fishless$stderr.coefficients[1,])*z)); AM3_480_fishless_LOWER[which(AM3_480_fishless_LOWER < 0.000001)] <- 0
AM3_480_fishless <- exp(fit_AM3_480_fishless$coefficients[1,]); AM3_480_fishless[which(AM3_480_fishless_LOWER < 0.000001)] <- 0

AM3_480_fish_UPPER <- exp((fit_AM3_480_fish$coefficients[1,]+(fit_AM3_480_fish$stderr.coefficients[1,])*z)); AM3_480_fish_UPPER[which(AM3_480_fish_UPPER > 100000)] <- 0
AM3_480_fish_LOWER <- exp((fit_AM3_480_fish$coefficients[1,]-(fit_AM3_480_fish$stderr.coefficients[1,])*z)); AM3_480_fish_LOWER[which(AM3_480_fish_LOWER < 0.000001)] <- 0
AM3_480_fish <- exp(fit_AM3_480_fish$coefficients[1,]); AM3_480_fish[which(AM3_480_fish_LOWER < 0.000001)] <- 0


AM3_30_fishless <- data.frame(AM3_30_fishless_LOWER,AM3_30_fishless,AM3_30_fishless_UPPER)
AM3_30_fish <- data.frame(AM3_30_fish_LOWER,AM3_30_fish,AM3_30_fish_UPPER)

AM3_120_fishless <- data.frame(AM3_120_fishless_LOWER,AM3_120_fishless,AM3_120_fishless_UPPER)
AM3_120_fish <- data.frame(AM3_120_fish_LOWER,AM3_120_fish,AM3_120_fish_UPPER)

AM3_480_fishless <- data.frame(AM3_480_fishless_LOWER,AM3_480_fishless,AM3_480_fishless_UPPER)
AM3_480_fish <- data.frame(AM3_480_fish_LOWER,AM3_480_fish,AM3_480_fish_UPPER)


AM3_30_fishless_pred <- AM3_30_fishless[which(TRAITS_AM3$trophic=="Pr"),]
AM3_30_fishless_cons <- AM3_30_fishless[which(TRAITS_AM3$trophic=="Non_Pred"),]
AM3_30_fishless_pred <- AM3_30_fishless_pred[order(TRAITS_AM3$total_ab[which(TRAITS_AM3$trophic=="Pr")], decreasing = T),]
AM3_30_fishless_cons <- AM3_30_fishless_cons[order(TRAITS_AM3$total_ab[which(TRAITS_AM3$trophic=="Non_Pred")], decreasing = T),]
AM3_30_fishless <- rbind(AM3_30_fishless_pred,AM3_30_fishless_cons)

AM3_30_fish_pred <- AM3_30_fish[which(TRAITS_AM3$trophic=="Pr"),]
AM3_30_fish_cons <- AM3_30_fish[which(TRAITS_AM3$trophic=="Non_Pred"),]
AM3_30_fish_pred <- AM3_30_fish_pred[order(TRAITS_AM3$total_ab[which(TRAITS_AM3$trophic=="Pr")], decreasing = T),]
AM3_30_fish_cons <- AM3_30_fish_cons[order(TRAITS_AM3$total_ab[which(TRAITS_AM3$trophic=="Non_Pred")], decreasing = T),]
AM3_30_fish <- rbind(AM3_30_fish_pred,AM3_30_fish_cons)


AM3_120_fishless_pred <- AM3_120_fishless[which(TRAITS_AM3$trophic=="Pr"),]
AM3_120_fishless_cons <- AM3_120_fishless[which(TRAITS_AM3$trophic=="Non_Pred"),]
AM3_120_fishless_pred <- AM3_120_fishless_pred[order(TRAITS_AM3$total_ab[which(TRAITS_AM3$trophic=="Pr")], decreasing = T),]
AM3_120_fishless_cons <- AM3_120_fishless_cons[order(TRAITS_AM3$total_ab[which(TRAITS_AM3$trophic=="Non_Pred")], decreasing = T),]
AM3_120_fishless <- rbind(AM3_120_fishless_pred,AM3_120_fishless_cons)

AM3_120_fish_pred <- AM3_120_fish[which(TRAITS_AM3$trophic=="Pr"),]
AM3_120_fish_cons <- AM3_120_fish[which(TRAITS_AM3$trophic=="Non_Pred"),]
AM3_120_fish_pred <- AM3_120_fish_pred[order(TRAITS_AM3$total_ab[which(TRAITS_AM3$trophic=="Pr")], decreasing = T),]
AM3_120_fish_cons <- AM3_120_fish_cons[order(TRAITS_AM3$total_ab[which(TRAITS_AM3$trophic=="Non_Pred")], decreasing = T),]
AM3_120_fish <- rbind(AM3_120_fish_pred,AM3_120_fish_cons)


AM3_480_fishless_pred <- AM3_480_fishless[which(TRAITS_AM3$trophic=="Pr"),]
AM3_480_fishless_cons <- AM3_480_fishless[which(TRAITS_AM3$trophic=="Non_Pred"),]
AM3_480_fishless_pred <- AM3_480_fishless_pred[order(TRAITS_AM3$total_ab[which(TRAITS_AM3$trophic=="Pr")], decreasing = T),]
AM3_480_fishless_cons <- AM3_480_fishless_cons[order(TRAITS_AM3$total_ab[which(TRAITS_AM3$trophic=="Non_Pred")], decreasing = T),]
AM3_480_fishless <- rbind(AM3_480_fishless_pred,AM3_480_fishless_cons)

AM3_480_fish_pred <- AM3_480_fish[which(TRAITS_AM3$trophic=="Pr"),]
AM3_480_fish_cons <- AM3_480_fish[which(TRAITS_AM3$trophic=="Non_Pred"),]
AM3_480_fish_pred <- AM3_480_fish_pred[order(TRAITS_AM3$total_ab[which(TRAITS_AM3$trophic=="Pr")], decreasing = T),]
AM3_480_fish_cons <- AM3_480_fish_cons[order(TRAITS_AM3$total_ab[which(TRAITS_AM3$trophic=="Non_Pred")], decreasing = T),]
AM3_480_fish <- rbind(AM3_480_fish_pred,AM3_480_fish_cons)


dim(AM3_30_fishless)

#svg("AM3.svg", width = 6, height = 20*0.8, pointsize = 12, family = "sans", bg = "transparent")
layout(matrix(c(1:20), 10, 2, byrow = TRUE))
par(mar = c(1,2,2,2))


colfunc_fishless<-colorRampPalette(c("sienna2","sienna4"))
cols_fishless <- colfunc_fishless(5)

colfunc_fish<-colorRampPalette(c("dodgerblue2","dodgerblue4"))
cols_fish <- colfunc_fish(5)

for(i in 1:dim(AM3_30_fishless)[1]){
  
  Low_fishless <- 3; Moderate_fishless <- 3; High_fishless <- 3
  if(AM3_30_fishless[i,2] < AM3_120_fishless[i,1] & AM3_120_fishless[i,2] > AM3_30_fishless[i,3]) {Low_fishless <- Low_fishless - 1}
  if(AM3_30_fishless[i,2] > AM3_120_fishless[i,3] & AM3_120_fishless[i,2] < AM3_30_fishless[i,1]) {Low_fishless <- Low_fishless + 1}
  if(AM3_30_fishless[i,2] < AM3_480_fishless[i,1] & AM3_480_fishless[i,2] > AM3_30_fishless[i,3]) {Low_fishless <- Low_fishless - 1}
  if(AM3_30_fishless[i,2] > AM3_480_fishless[i,3] & AM3_480_fishless[i,2] < AM3_30_fishless[i,1]) {Low_fishless <- Low_fishless + 1}
  if(AM3_120_fishless[i,2] < AM3_30_fishless[i,1] & AM3_30_fishless[i,2] > AM3_120_fishless[i,3]) {Moderate_fishless <- Moderate_fishless - 1}
  if(AM3_120_fishless[i,2] > AM3_30_fishless[i,3] & AM3_30_fishless[i,2] < AM3_120_fishless[i,1]) {Moderate_fishless <- Moderate_fishless + 1}
  if(AM3_120_fishless[i,2] < AM3_480_fishless[i,1] & AM3_480_fishless[i,2] > AM3_120_fishless[i,3]) {Moderate_fishless <- Moderate_fishless - 1}
  if(AM3_120_fishless[i,2] > AM3_480_fishless[i,3] & AM3_480_fishless[i,2] < AM3_120_fishless[i,1]) {Moderate_fishless <- Moderate_fishless + 1}
  if(AM3_480_fishless[i,2] < AM3_30_fishless[i,1] & AM3_30_fishless[i,2] > AM3_480_fishless[i,3]) {High_fishless <- High_fishless - 1}
  if(AM3_480_fishless[i,2] > AM3_30_fishless[i,3] & AM3_30_fishless[i,2] < AM3_480_fishless[i,1]) {High_fishless <- High_fishless + 1}
  if(AM3_480_fishless[i,2] < AM3_120_fishless[i,1] & AM3_120_fishless[i,2] > AM3_480_fishless[i,3]) {High_fishless <- High_fishless - 1}
  if(AM3_480_fishless[i,2] > AM3_120_fishless[i,3] & AM3_120_fishless[i,2] < AM3_480_fishless[i,1]) {High_fishless <- High_fishless + 1}
  col_sp_fishless <- c(Low_fishless, Moderate_fishless, High_fishless) - min(c(Low_fishless, Moderate_fishless, High_fishless))
  for(j in 1:3){
    if (col_sp_fishless[j] == 0) {col_sp_fishless[j]<- cols_fishless[1]}
    if (col_sp_fishless[j] == 1) {col_sp_fishless[j]<- cols_fishless[2]}
    if (col_sp_fishless[j] == 2) {col_sp_fishless[j]<- cols_fishless[3]}
    if (col_sp_fishless[j] == 3) {col_sp_fishless[j]<- cols_fishless[4]}
    if (col_sp_fishless[j] == 4) {col_sp_fishless[j]<- cols_fishless[5]}
  }
  if (AM3_30_fishless[i,2] == 0) {col_sp_fishless[1] <- "grey70"}
  if (AM3_120_fishless[i,2] == 0) {col_sp_fishless[2] <- "grey70"}
  if (AM3_480_fishless[i,2] == 0) {col_sp_fishless[3] <- "grey70"}
  #col_sp_fishless <- as.numeric(as.factor(c(Low_fishless, Moderate_fishless, High_fishless)))
  #col_sp_fishless[which(col_sp_fishless == "1")] <- "sienna2"
  #col_sp_fishless[which(col_sp_fishless == "2")] <- "sienna3"
  #col_sp_fishless[which(col_sp_fishless == "3")] <- "sienna4"
  
  
  Low_fish <- 3; Moderate_fish <- 3; High_fish <- 3
  if(AM3_30_fish[i,2] < AM3_120_fish[i,1] & AM3_120_fish[i,2] > AM3_30_fish[i,3]) {Low_fish <- Low_fish - 1}
  if(AM3_30_fish[i,2] > AM3_120_fish[i,3] & AM3_120_fish[i,2] < AM3_30_fish[i,1]) {Low_fish <- Low_fish + 1}
  if(AM3_30_fish[i,2] < AM3_480_fish[i,1] & AM3_480_fish[i,2] > AM3_30_fish[i,3]) {Low_fish <- Low_fish - 1}
  if(AM3_30_fish[i,2] > AM3_480_fish[i,3] & AM3_480_fish[i,2] < AM3_30_fish[i,1]) {Low_fish <- Low_fish + 1}
  if(AM3_120_fish[i,2] < AM3_30_fish[i,1] & AM3_30_fish[i,2] > AM3_120_fish[i,3]) {Moderate_fish <- Moderate_fish - 1}
  if(AM3_120_fish[i,2] > AM3_30_fish[i,3] & AM3_30_fish[i,2] < AM3_120_fish[i,1]) {Moderate_fish <- Moderate_fish + 1}
  if(AM3_120_fish[i,2] < AM3_480_fish[i,1] & AM3_480_fish[i,2] > AM3_120_fish[i,3]) {Moderate_fish <- Moderate_fish - 1}
  if(AM3_120_fish[i,2] > AM3_480_fish[i,3] & AM3_480_fish[i,2] < AM3_120_fish[i,1]) {Moderate_fish <- Moderate_fish + 1}
  if(AM3_480_fish[i,2] < AM3_30_fish[i,1] & AM3_30_fish[i,2] > AM3_480_fish[i,3]) {High_fish <- High_fish - 1}
  if(AM3_480_fish[i,2] > AM3_30_fish[i,3] & AM3_30_fish[i,2] < AM3_480_fish[i,1]) {High_fish <- High_fish + 1}
  if(AM3_480_fish[i,2] < AM3_120_fish[i,1] & AM3_120_fish[i,2] > AM3_480_fish[i,3]) {High_fish <- High_fish - 1}
  if(AM3_480_fish[i,2] > AM3_120_fish[i,3] & AM3_120_fish[i,2] < AM3_480_fish[i,1]) {High_fish <- High_fish + 1}
  col_sp_fish <- c(Low_fish, Moderate_fish, High_fish) - min(c(Low_fish, Moderate_fish, High_fish))
  for(j in 1:3){
    if (col_sp_fish[j] == 0) {col_sp_fish[j]<- cols_fish[1]}
    if (col_sp_fish[j] == 1) {col_sp_fish[j]<- cols_fish[2]}
    if (col_sp_fish[j] == 2) {col_sp_fish[j]<- cols_fish[3]}
    if (col_sp_fish[j] == 3) {col_sp_fish[j]<- cols_fish[4]}
    if (col_sp_fish[j] == 4) {col_sp_fish[j]<- cols_fish[5]}
  }
  if (AM3_30_fish[i,2] == 0) {col_sp_fish[1] <- "grey70"}
  if (AM3_120_fish[i,2] == 0) {col_sp_fish[2] <- "grey70"}
  if (AM3_480_fish[i,2] == 0) {col_sp_fish[3] <- "grey70"}
  #col_sp_fish <- as.numeric(as.factor(c(Low_fish, Moderate_fish, High_fish)))
  #col_sp_fish[which(col_sp_fish == "1")] <- "dodgerblue2"
  #col_sp_fish[which(col_sp_fish == "2")] <- "dodgerblue3"
  #col_sp_fish[which(col_sp_fish == "3")] <- "dodgerblue4"
  
  
  
  plot(c(NA,NA),ylim = c(0,max(c(AM3_480_fishless[i,3], AM3_120_fishless[i,3],AM3_30_fishless[i,3],AM3_480_fish[i,3], AM3_120_fish[i,3],AM3_30_fish[i,3]))), xlim = c(0.25,3.055), type = "p", xaxt = "n",ylab = "Abundance",xlab = "Isolation", main = rownames(AM3_30_fishless)[i])
  #abline(h = c(AM3_30_fishless[i,2],AM3_120_fishless[i,2], AM3_480_fishless[i,2],AM3_480_fishless[i,2], AM3_120_fishless[i,2],AM3_30_fishless[i,2]), lty = 2, col = "gray75")
  
  lines(y = c(AM3_30_fishless[i,2],AM3_120_fishless[i,2]),x = c(1.05:2.05), type = "l", lty = 2, col = "sienna3", lwd  = 2)
  lines(y = c(AM3_120_fishless[i,2],AM3_480_fishless[i,2]),x = c(2.05:3.05), type = "l", lty = 2, col = "sienna3", lwd  = 2)
  lines(y = c(AM3_30_fish[i,2],AM3_120_fish[i,2]),x = c(0.95:1.95), type = "l", lty = 2, col = "dodgerblue3", lwd  = 2)
  lines(y = c(AM3_120_fish[i,2],AM3_480_fish[i,2]),x = c(1.95:2.95), type = "l", lty = 2, col = "dodgerblue3", lwd  = 2)
  
  points(y = c(AM3_30_fishless[i,2],AM3_120_fishless[i,2], AM3_480_fishless[i,2]), x = c(1.05,2.05,3.05), cex = 2, pch = 16, col = col_sp_fishless)
  points(y = c(AM3_30_fish[i,2],AM3_120_fish[i,2], AM3_480_fish[i,2]), x = c(0.95,1.95,2.95), cex = 2, pch = 17, col = col_sp_fish)
  
  arrows(y0 = c(AM3_30_fishless[i,1],AM3_120_fishless[i,1], AM3_480_fishless[i,1]), y1 = c(AM3_30_fishless[i,3],AM3_120_fishless[i,3], AM3_480_fishless[i,3]), x1 = c(1.05,2.05,3.05), x0 = c(1.05,2.05,3.05), 
         code = 3, angle = 90, length = 0.05,col = col_sp_fishless, lwd = 2)
  arrows(y0 = c(AM3_30_fish[i,1],AM3_120_fish[i,1], AM3_480_fish[i,1]), y1 = c(AM3_30_fish[i,3],AM3_120_fish[i,3], AM3_480_fish[i,3]), x1 = c(0.95,1.95,2.95), x0 = c(0.95,1.95,2.95), 
         code = 3, angle = 90, length = 0.05,col = col_sp_fish, lwd = 2)
  axis(1, at = c(1:3), labels = c("30 m","120 m","480 m"), las = 1, cex.axis = 1)
  box(lty=1, lwd=3)
}
#dev.off()


