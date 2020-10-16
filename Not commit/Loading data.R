library(vegan)

planilha <- read.csv("Not commit/All_data.csv", header = T)
fish <- read.csv("Not commit/Fish.csv", stringsAsFactors = T, row.names = 1)
traits <- read.csv("Not commit/Traits.csv", header = T, stringsAsFactors = T)


#Constucting matrices------------------------------------------------------------------------------------------------
#Fish and Isolation variables####
coord <- fish[,2:3]
fish <- fish$fish


isolation <- c(rep("30",8),rep("120",8),rep("480",8))
isolation <- as.factor(isolation)
levels(isolation)
isolation <- relevel(isolation, ref="30")


fish<- c(as.character(fish),as.character(fish),as.character(fish))
isolation <- c(as.character(isolation),as.character(isolation),as.character(isolation))
fish <- as.factor(fish)
isolation <- as.factor(isolation)

SS <- na.omit(as.factor(planilha$SS))
poca <- planilha[3:dim(planilha)[1],1]
ID <- as.factor(poca)




#Constructing Y matrix####
com <- planilha[,c(3:length(planilha))]
species_inf <- t(com[c(1:2),])
com <- com[c(3:dim(planilha)[1]),]

for(i in 1:dim(com)[2]){
  com[,i] <- as.numeric(as.character(com[,i]))
}


fish<-fish[which(is.na(com$B_Callibaetis) == F)]
isolation<-isolation[which(is.na(com$B_Callibaetis) == F)]
isolation <- relevel(isolation, ref="30")
SS<-SS[which(is.na(com$B_Callibaetis) == F)]
ID<-ID[which(is.na(com$B_Callibaetis) == F)]
com <- na.omit(com)
com_orig <- data.frame(ID,com)



####Removind ponds lost in SS3 for balanced design
ID
#C4
#C3
#B3
#A4

com_incomplete <- com[which(ID != "A4" & ID != "B3" & ID != "C3" & ID != "C4"),]
com_incomplete_oc <- decostand(com_incomplete, method = "pa")
com_incomplete <- com_incomplete[,which(colSums(com_incomplete_oc) > 3)]
com_oc <- decostand(com, method = "pa")

#write.csv(traits,"Traits Exp 1.csv")
#write.csv(data.frame(ID,com),"communities EXP1.csv")
treatments <- data.frame(ID,survey = SS, fish, isolation)
#write.csv(treatments,"treatments EXP1.csv")


com <- com[,which(colSums(com_oc) > 3)]
com_oc <- com_oc[,which(colSums(com_oc) > 3)]


isolation_incomplete <- isolation[which(ID != "A4" & ID != "B3" & ID != "C3" & ID != "C4")]
fish_incomplete <- fish[which(ID != "A4" & ID != "B3" & ID != "C3" & ID != "C4")]
SS_incomplete <- SS[which(ID != "A4" & ID != "B3" & ID != "C3" & ID != "C4")]
ID_incomplete <- ID[which(ID != "A4" & ID != "B3" & ID != "C3" & ID != "C4")]
ID_incomplete <- as.factor(as.character(ID_incomplete))



##Construction Trait Matrix#####

traits<- traits[which(traits$species!= "Chironominae" &
                        traits$species!= "Tanypodinae"),]


########################################################################
########################################################################
########################################################################
#Basic information#

total_samp <- colSums(com_orig[,2:dim(com_orig)[2]])
total_samp_SS1 <- colSums(com_orig[,2:dim(com_orig)[2]][which(SS == "1"),])
total_samp_SS2 <- colSums(com_orig[,2:dim(com_orig)[2]][which(SS == "2"),])
total_samp_SS3 <- colSums(com_orig[,2:dim(com_orig)[2]][which(SS == "3"),])


species_inf <- data.frame(total_samp,species_inf)
colnames(species_inf) <- c("ab", "order", "family")
species_inf_unordered <- species_inf
species_inf <- species_inf[order(total_samp),]

species_inf[order(species_inf$order, partial = species_inf$family),]

table(species_inf$order, species_inf$family)
unique(species_inf[,2:3])

by_order <- sort(tapply(species_inf$ab, species_inf$order, sum))
by_order_percent <-  by_order/sum(by_order)
length(by_order)

by_family <- sort(tapply(species_inf$ab, species_inf$family, sum))
by_family_percent <-  round(by_family/sum(by_family),4)
length(by_family)

species_inf
########################################################################
########################################################################
########################################################################



dim(com_incomplete_oc)
dim(traits)
traits$volume_log <- log(traits$volume+1)
traits$family <- species_inf_unordered$family
traits$order <- species_inf_unordered$order
traits$total_ab <- total_samp
traits$total_ab_log <- log(total_samp)
traits$total_ab_SS1 <- total_samp_SS1
traits$total_ab_SS2 <- total_samp_SS2
traits$total_ab_SS3 <- total_samp_SS3
traits$total_ab_SS1_log <- log(total_samp_SS1+1)
traits$total_ab_SS2_log <- log(total_samp_SS2+1)
traits$total_ab_SS3_log <- log(total_samp_SS3+1)


TRAITS <- traits[which(colSums(com_incomplete_oc)>3),]
dim(com_incomplete)
rownames(TRAITS) <- TRAITS$species
TRAITS <- TRAITS[,2:ncol(TRAITS)]


TRAITS <- TRAITS[colSums(com_oc)>2,]





#####Constructing Y and environmental matrices for each sampling survey#####

isolation_SS1 <- isolation[which(SS == "1")]
isolation_SS2 <- isolation[which(SS == "2")]
isolation_SS3 <- isolation[which(SS == "3")]
fish_SS1 <- fish[which(SS == "1")]
fish_SS2 <- fish[which(SS == "2")]
fish_SS3 <- fish[which(SS == "3")]

fish_isolation_SS3 <- rep(NA, length(isolation_SS3))
for(i in 1:length(isolation_SS3)){
  fish_isolation_SS3[i] <- paste(isolation_SS3[i], fish_SS3[i], sep = " ")
}
fish_isolation_SS3 <- factor(fish_isolation_SS3,levels = c("30 absent","120 absent","480 absent","30 present","120 present","480 present"))

fish_isolation_SS2 <- rep(NA, length(isolation_SS2))
for(i in 1:length(isolation_SS2)){
  fish_isolation_SS2[i] <- paste(isolation_SS2[i], fish_SS2[i], sep = " ")
}
fish_isolation_SS2 <- factor(fish_isolation_SS2,levels = c("30 absent","120 absent","480 absent","30 present","120 present","480 present"))

fish_isolation_SS1 <- rep(NA, length(isolation_SS1))
for(i in 1:length(isolation_SS1)){
  fish_isolation_SS1[i] <- paste(isolation_SS1[i], fish_SS1[i], sep = " ")
}
fish_isolation_SS1 <- factor(fish_isolation_SS1,levels = c("30 absent","120 absent","480 absent","30 present","120 present","480 present"))


com_SS1 <- com[which(SS == "1"), ]
com_SS1_oc <- decostand(com_SS1, method = "pa")
com_SS1 <- com_SS1[,which(colSums(com_SS1_oc) > 2)]

TRAITS_SS1 <- TRAITS[colSums(com_SS1_oc)>2,]
TRAITS_SS1


com_SS2 <- com[which(SS == "2"), ]
com_SS2_oc <- decostand(com_SS2, method = "pa")
com_SS2 <- com_SS2[,which(colSums(com_SS2_oc) > 2)]

TRAITS_SS2 <- TRAITS[colSums(com_SS2_oc)>2,]
TRAITS_SS2

com_SS3 <- com[which(SS == "3"), ]
com_SS3_oc <- decostand(com_SS3, method = "pa")
com_SS3 <- com_SS3[,which(colSums(com_SS3_oc) > 2)]

TRAITS_SS3 <- TRAITS[colSums(com_SS3_oc)>2,]
TRAITS_SS3





env <- data.frame(fish_incomplete, isolation_incomplete, SS_incomplete)


isolation_30_SS1 <- rep("+120", length(isolation_SS1))
isolation_30_SS1[which(isolation_SS1 == "30")] <- "30"

isolation_480_SS1 <- rep("-480", length(isolation_SS1))
isolation_480_SS1[which(isolation_SS1 == "480")] <- "480"

env_SS1 <- data.frame(fish_SS1, isolation_SS1, isolation_30_SS1,  isolation_480_SS1)




isolation_30_SS2 <- rep("+120", length(isolation_SS2))
isolation_30_SS2[which(isolation_SS2 == "30")] <- "30"

isolation_480_SS2 <- rep("-480", length(isolation_SS2))
isolation_480_SS2[which(isolation_SS2 == "480")] <- "480"

env_SS2 <- data.frame(fish_SS2, isolation_SS2, isolation_30_SS2,  isolation_480_SS2)




isolation_30_SS3 <- rep("+120", length(isolation_SS3))
isolation_30_SS3[which(isolation_SS3 == "30")] <- "30"

isolation_480_SS3 <- rep("-480", length(isolation_SS3))
isolation_480_SS3[which(isolation_SS3 == "480")] <- "480"

env_SS3 <- data.frame(fish_SS3, isolation_SS3, isolation_30_SS3,  isolation_480_SS3)








####################################### ABUNDANCE

abundance_predators <- rowSums(com[,which(TRAITS$trophic == "Pr")])
abundance_consumers <- rowSums(com[,which(TRAITS$trophic == "Non_Pred")])


