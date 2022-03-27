setwd("~/")
#install.packages(c("openxlsx","data.table","ggplot2","broom","car","ggpubr"))

library(openxlsx)
library(data.table)
library(ggplot2)
library(car)
library(broom)
library(ggpubr)
library(ggpattern)
library()
data <- read.xlsx(xlsxFile = "Copy_of_Results_pot_experiment_2021_2022.xlsx",sheet = "Experiment")
data <- as.data.table(data)
measure <- data[-21,c(1,2,3,4,5,9,13,18,23,28,33,38,43,48,53,58,63,68,73,78,83,88,93,98,103)]
cols <- names(measure)[5:25]
measure[,(cols) := round(.SD,1), .SDcols=cols]
info <- measure[c(1:39),c(1,2,3,4)]
info$plant_id <- as.factor(info$plant_id)

measure <- measure[c(1:39),c(-2,-3,-4)]
colnames(measure) <- c("plant_id",1:21)
measure[26,13] <- 9
measure[26,15] <- 8.9
measure_2 <- melt.data.table(measure,id.vars = "plant_id")
measure_2$plant_id <- as.factor(measure_2$plant_id)
measure_2$variable <- as.numeric(measure_2$variable)
measure_2
ggplot(measure_2, aes(x = variable,y = value)) +
  geom_point(aes(col = plant_id)) +
  geom_smooth(col = "black") +
  xlab("measuring point")+ylab("height(cm)")+ggtitle(label = "plant height over time")+
  facet_wrap(~plant_id)
measure_2
measure_2 <- merge(x = measure_2,y = info,by = "plant_id")
b <- matrix(nrow = 39,ncol = 20)
measure_matrix <- as.matrix(x = measure)
for (i in 2:21){ # i is number of colums
  for (j in 1:39){ # j is number of rows - 1
    b[j,i-1] <- (measure_matrix[j,i+1] - measure_matrix[j,i] )/ measure_matrix[j,i]
    #print(i)
    #print(j)
  }
}
change <- as.data.table(b)
change$plant_id <- as.factor(measure$plant_id)
change <- melt.data.table(change,id.vars = "plant_id")
change_descriptive_statistics <- change[, .(mean(value), max(value), min(value), sd(value), median(value)), by = .(plant_id)]
colnames(change_descriptive_statistics) <-  c("plant_id","mean","max","min","sd","median")
change_descriptive_statistics$COF <- change_descriptive_statistics$sd/change_descriptive_statistics$mean
change_descriptive_statistics$skew <- 3*(change_descriptive_statistics$mean - change_descriptive_statistics$median)/change_descriptive_statistics$sd
change_descriptive_statistics$meanmaxratio <- change_descriptive_statistics$mean/change_descriptive_statistics$max
change_descriptive_statistics$meanminratio <- change_descriptive_statistics$mean/change_descriptive_statistics$min
change_descriptive_statistics$range <- change_descriptive_statistics$max - change_descriptive_statistics$min
change$variable <- as.numeric(change$variable)
total_growth_rate <- change[,mean(value), by = plant_id]
total_growth_rate[V1 < 0, growth_factor := "negative"]
quantile(total_growth_rate$V1)
total_growth_rate[(V1 > 0) & (V1 < 0.01), growth_factor := "low growth"]
total_growth_rate[(V1 > 0.01) & (V1 < 0.03), growth_factor := "medium growth"]
total_growth_rate[V1 > 0.03, growth_factor := "high growth"]
total_growth_rate
change <- merge(change,total_growth_rate,by = "plant_id")
change_N <- change[growth_factor == "negative"]
change_N
ggplot(change_N, aes(x = variable,y = value)) +
  geom_point(aes(col = plant_id)) +
  geom_smooth(col = "black") +
  facet_wrap(~plant_id)
change_L <- change[growth_factor == "low growth"]
change_L
ggplot(change_L, aes(x = variable,y = value)) +
  geom_point(aes(col = plant_id)) +
  geom_smooth(col = "black") +
  facet_wrap(~plant_id)
change_M <- change[growth_factor == "medium growth"]
change_M
ggplot(change_M, aes(x = variable,y = value)) +
  geom_point(aes(col = plant_id)) +
  geom_smooth(col = "black") +
  facet_wrap(~plant_id)+
  ylim(c(0,0.5))+
  ggtitle("mean growth rate of fastest growing plants")+
  xlab("time period of measurment")+
  ylab("growth rate")
change_H <- change[growth_factor == "high growth"]
change_H
ggplot(change_H, aes(x = variable,y = value)) +
  geom_point(aes(col = plant_id)) +
  geom_smooth(col = "black") +
  facet_wrap(~plant_id)+
  ylim(c(0,0.5))+
  ggtitle("mean growth rate of fastest growing plants")+
  xlab("time period of measurment")+
  ylab("growth rate")
info
change_descriptive_statistics <- merge(x = change_descriptive_statistics,y = info,by = "plant_id")

change_descriptive_statistics$biochar <- as.factor(change_descriptive_statistics$biochar)
change_descriptive_statistics$greywater <- as.factor(change_descriptive_statistics$greywater)
change_descriptive_statistics <- change_descriptive_statistics[-1,]
change_descriptive_statistics[, c("species", "biochar","greywater")] <- lapply(change_descriptive_statistics[, c("species", "biochar","greywater")], factor)
change_descriptive_statistics
data_plot <- change_descriptive_statistics[,c(1,2,12,13,14)]
data_plot
model  <- lm(mean ~ species*biochar*greywater, data = data_plot)
total_growth <- data_plot[,sum(mean), by = .(species,biochar,greywater)]
total_growth
str(total_growth)
ggplot(total_growth, aes(greywater,V1,fill = biochar))+
  geom_bar(stat="identity",position = "dodge")+
  ylab("total mean growth rate")+
  facet_wrap(~species)
#library(gvlma)
#gvlma(model)
summary(model)
ggqqplot(residuals(model))
shapiro.test(residuals(model))
###shapiro between groups?
data_plot
leveneTest(mean ~ species*biochar*greywater,data = data_plot)
ggboxplot(data_plot, x = "greywater", y = "mean",  color = "biochar", palette = c("red", "black"), facet.by = "species")+ylab("mean growth rate")
plot.design(mean ~ ., data = data_plot)
op <- par(mfrow = c(3, 1))
with(data_plot, {
  interaction.plot(species, biochar, mean)
  interaction.plot(species, greywater, mean)
  interaction.plot(greywater, biochar, mean)
}
)
par(op)
fm <- aov(mean ~ species * biochar * greywater, data = data_plot)
summary(fm)
fm <- update(fm, . ~ . -species:biochar:greywater)
summary(fm)
plot(fm)

posthoc <- TukeyHSD(fm)

library(multcompView)
cld <- multcompLetters4(fm,posthoc)
cld$`species:biochar`
library(dplyr)
data_summary <- group_by(data_plot, species, biochar) %>%
  summarise(mean=mean(mean), quant=quantile(mean,probs = .75)) %>%
  arrange(desc(mean))
cld <- as.data.frame.list(cld$`species:biochar`)
cld$Letters
data_summary$Tukey <- cld$Letters
data_summary
data_plot
ggplot(data_plot, aes(species,mean, col = biochar)) + 
  geom_boxplot() +
  labs(x="species type Type", y="Weight (g)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(data = data_summary, aes(x = species, y = quant, label = cld, col = biochar))

ggboxplot(data_plot, x = "species", y = "mean",  color = "biochar")+ylab("mean growth rate")

########################

measure_3 <- data[-21,c(1,6,10,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84,89,94,99,104)]
measure_3 <- measure_3[1:39]
measure_3
measure_4 <- melt.data.table(measure_3,id.vars = "plant_id")
str(measure_4)
measure_4_1 <- measure_4[plant_id < 380]
measure_4_1
hist(measure_4_1$value)
str(info)
str(measure_4_1)
measure_4_1$plant_id <- as.factor(measure_4_1$plant_id)
measure_4_1$variable <- as.numeric(measure_4_1$variable)
measure_4_2 <- merge(x = measure_4_1,y = info,by = "plant_id")
measure_4_2 <- measure_4_2[species == "G.Macrorrhizum"]
hist(measure_4_2$value,xlab = "number of leaves",main = "Number of Leaves in G.Macrorrizum")
measure_4_2
r1 <- glm(value ~ biochar * greywater, family = poisson,data = measure_4_2)
summary(r1)
plot(r1)
ggboxplot(measure_4_2, x = "species", y = "value",  color = "biochar")+ylab("number of leaves")+facet_wrap(~greywater)

library(emmeans)
summary(emmeans(r1, pairwise ~ biochar*greywater, adjust="tukey", mode="linear.predictor", type="Score"))
(measure_4_2)
measure_4_2$biochar <- as.factor(measure_4_2$biochar)
measure_4_2$greywater <- as.factor(measure_4_2$greywater)
ggplot(measure_4_2, aes(x = variable,y = value, fill = biochar)) +
  geom_smooth(col = "black") +
  facet_wrap(~greywater)+xlab("measurment point in experiment")+ylab("number of leaves")+ggtitle      ("Greywater and Biochar effect on number of leaves")

ggplot(measure_4, aes(x = variable,y = value)) +
  geom_point(aes(col = plant_id)) +
  geom_smooth(col = "black") +
  facet_wrap(~plant_id)


#####################################

measure_5 <- data[-21,c(1,7,8,11,12,15,16,20,21,25,26,30,31,35,36,40,41,45,46,50,51,55,56,60,61,65,66,70,71,75,76,80,81,85,86,90,91,95,96,100,101,105,106)]
measure_5 <- measure_5[1:39]
a <- data.table()
a$T1 <- measure_5[,2]+measure_5[,3]
a$T2 <- measure_5[,4]+measure_5[,5]
a$T3 <- measure_5[,6]+measure_5[,7]
a$T4 <- measure_5[,8]+measure_5[,9]
a$T5 <- measure_5[,10]+measure_5[,11]
a$T6 <- measure_5[,12]+measure_5[,13]
a$T7 <- measure_5[,14]+measure_5[,15]
a$T8 <- measure_5[,16]+measure_5[,17]
a$T9 <- measure_5[,18]+measure_5[,19]
a$T10 <- measure_5[,20]+measure_5[,21]
a$T11 <- measure_5[,22]+measure_5[,23]
a$T12 <- measure_5[,24]+measure_5[,25]
a$T13 <- measure_5[,26]+measure_5[,27]
a$T14 <- measure_5[,28]+measure_5[,29]
a$T15 <- measure_5[,30]+measure_5[,31]
a$T16 <- measure_5[,32]+measure_5[,33]
a$T17 <- measure_5[,34]+measure_5[,35]
a$T18 <- measure_5[,36]+measure_5[,37]
a$T19 <- measure_5[,38]+measure_5[,39]
a$T20 <- measure_5[,40]+measure_5[,41]
a$T21 <- measure_5[,42]+measure_5[,43]
a$plant_id <- info$plant_id
a$species <- info$species
measure_6 <- melt.data.table(a,id.vars = c("plant_id","species"))
measure_6
str(measure_6)
measure_6_1 <- measure_6[species == "A.maritima"]
measure_6_1
hist(measure_4_1$value)
str(info)
str(measure_4_1)
measure_6$plant_id <- as.factor(measure_6$plant_id)
measure_6$variable <- as.numeric(measure_6$variable)
measure_6_1

hist(measure_4_1$value)
measure_6_1 <- merge(x = measure_6,y = info,by = "plant_id")
measure_6_1 <- measure_6_1[species.x == "A.maritima"]
measure_6_1
info
measure_6_1
hist(measure_6_1$value, xlab = "number of flowers",main = "Number of flowers in A.maritima")
measure_6_1
r2 <- glm(value ~ biochar * greywater, family = poisson,data = measure_6_1)
summary(r2)
plot(r2)

measure_6_1[,1,2,3,4,6,7]
measure_6 <- measure_6[species == "A.maritima"]
measure_6_1$biochar <- as.factor(measure_6_1$biochar)
measure_6_1$greywater <- as.factor(measure_6_1$greywater)
ggplot(measure_6_1, aes(x = variable,y = value,color = biochar)) +
  geom_smooth()+
  facet_wrap(~greywater)+xlab("measurment point in experiment")+ylab("number of flowers")+ggtitle      ("Greywater effect on number of Flowers")

measure_6_1
ggplot(measure_6_1, aes(x = variable,y = value)) +
  geom_line(aes(col = plant_id))+ 
  facet_wrap(biochar~greywater)


##############drying data####################

data_experiment <- read.xlsx(xlsxFile = "Copy_of_Results_pot_experiment_2021_2022.xlsx",sheet = "Experiment")
data_drying <- read.xlsx(xlsxFile = "Copy_of_Copy_of_Results_pot_experiment_2021_2022_all.xlsx",sheet = "Final_results")
data_drying$`Type.of.biomass.(Leaves:.L,.Flowers:.F,.Rhizomes:.R)` <- as.factor(data_drying$`Type.of.biomass.(Leaves:.L,.Flowers:.F,.Rhizomes:.R)`)

data_drying <- as.data.table(data_drying)
data_drying

G_macro <- data_drying[Name == "G.Macrorrhizum" & `Type.of.biomass.(Leaves:.L,.Flowers:.F,.Rhizomes:.R)` == "L"][,c(1,2,4)]
no_of_leaves <- measure_3[,c(1,22)]
colnames(no_of_leaves) <- c("Number", "M21_no_L")
all <- merge(x = G_macro,y = no_of_leaves,by = "Number")
colnames(all) <- c("Number","Name","Dry_biomass_sixty_degrees_after_one_day","M21_no_L") 
plot(x = all$Dry_biomass_sixty_degrees_after_one_day,y = all$M21_no_L)
shapiro.test(all$Dry_biomass_sixty_degrees_after_one_day)
cor.test(x = all$Dry_biomass_sixty_degrees_after_one_day,y = all$M21_no_L,method = "spearman",alternative = "g")
A_maritina <- data_drying[Name == "A.maritima" & `Type.of.biomass.(Leaves:.L,.Flowers:.F,.Rhizomes:.R)` == "L"][,c(1,2,4)]
no_of_leaves <- measure_3[,c(1,22)]
colnames(no_of_leaves) <- c("Number", "M21_no_L")
all_2 <- merge(x = A_maritina,y = no_of_leaves,by = "Number")
colnames(all_2) <- c("Number","Name","Dry_biomass_sixty_degrees_after_one_day","M21_no_L") 
plot(x = all_2$Dry_biomass_sixty_degrees_after_one_day,y = all_2$M21_no_L)
shapiro.test(all_2$Dry_biomass_sixty_degrees_after_one_day)
shapiro.test(all_2$M21_no_L)
cor.test(x = all_2$Dry_biomass_sixty_degrees_after_one_day,y = all_2$M21_no_L,method = "spearman",alternative = "g")


A_maritina_2 <- data_drying[Name == "A.maritima"][,c(1:7)]
A_maritina_2$`GW.(Yes/No)` <- as.factor(A_maritina_2$`GW.(Yes/No)`)
A_maritina_2$`Biochar.(Yes/No)` <- as.factor(A_maritina_2$`Biochar.(Yes/No)`)

A_maritina_2$Number <- as.factor(A_maritina_2$Number)

colnames(A_maritina_2) <- c("Number","Name","Biomass_before_drying_grams","Dry_biomass_grams_sixty_degrees_after_one_day","Type_of_biomass","GW_Yes_No","Biochar_Yes_No")
ggplot(A_maritina_2,aes(x = Number,y = Dry_biomass_grams_sixty_degrees_after_one_day,fill = Type_of_biomass)) +
  geom_bar(stat = "identity",position = "dodge")+
  coord_flip()

#A_maritina_3 <- A_maritina_2[Type_of_biomass == "L" | Type_of_biomass == "F"] 
#A_maritina_3


#model <- lm(Dry_biomass_grams_sixty_degrees_after_one_day ~ Type_of_biomass * Biochar_Yes_No * GW_Yes_No, data = A_maritina_3)
#shapiro.test(residuals(model))
#leveneTest(Dry_biomass_grams_sixty_degrees_after_one_day ~ Type_of_biomass * Biochar_Yes_No * GW_Yes_No, data = A_maritina_2)
#fm <- aov(Dry_biomass_grams_sixty_degrees_after_one_day ~ Type_of_biomass * Biochar_Yes_No * GW_Yes_No, data = A_maritina_2)
#summary(fm)


#A_maritina_4 <- A_maritina_2[Type_of_biomass == "R"] 

#A_maritina_4
#mean(A_maritina_4$Dry_biomass_grams_sixty_degrees_after_one_day)
#sd(A_maritina_4$Dry_biomass_grams_sixty_degrees_after_one_day)
#model <- lm(Dry_biomass_grams_sixty_degrees_after_one_day ~ Biochar_Yes_No * GW_Yes_No, data = A_maritina_4)
#shapiro.test(residuals(model))
#leveneTest(Dry_biomass_grams_sixty_degrees_after_one_day ~  Biochar_Yes_No * GW_Yes_No, data = A_maritina_4)

#fm <- aov(Dry_biomass_grams_sixty_degrees_after_one_day ~ Biochar_Yes_No*GW_Yes_No, data = A_maritina_4)

#summary(fm) # no significance


G_macrorrhizum_2 <- data_drying[Name == "G.Macrorrhizum"][,c(1:7)]
G_macrorrhizum_2$`GW.(Yes/No)` <- as.factor(G_macrorrhizum_2$`GW.(Yes/No)`)
G_macrorrhizum_2$`Biochar.(Yes/No)` <- as.factor(G_macrorrhizum_2$`Biochar.(Yes/No)`)
colnames(G_macrorrhizum_2) <- c("Number","Name","Biomass_before_drying_grams","Dry_biomass_grams_sixty_degrees_after_one_day","Type_of_biomass","GW_Yes_No","Biochar_Yes_No")
G_macrorrhizum_2$Number <- as.factor(G_macrorrhizum_2$Number)
ggplot(G_macrorrhizum_2,aes(x = Number,y = Dry_biomass_grams_sixty_degrees_after_one_day,fill = Type_of_biomass)) +
  geom_bar(stat = "identity",
           position = "dodge")+
  coord_flip()
#G_macrorrhizum_3 <- G_macrorrhizum_2[Type_of_biomass == "L"] 
#G_macrorrhizum_3

#model  <- lm(Dry_biomass_grams_sixty_degrees_after_one_day ~ Biochar_Yes_No * GW_Yes_No, data = G_macrorrhizum_3)
#ggqqplot(residuals(model))
#shapiro.test(residuals(model))
#leveneTest(Dry_biomass_grams_sixty_degrees_after_one_day ~ Biochar_Yes_No * GW_Yes_No, data = G_macrorrhizum_3)
#fm <- aov(Dry_biomass_grams_sixty_degrees_after_one_day ~ Biochar_Yes_No * GW_Yes_No, data = G_macrorrhizum_3)
#summary(fm) # no significance

#G_macrorrhizum_4 <- G_macrorrhizum_2[Type_of_biomass == "R"] 
#G_macrorrhizum_4 <- G_macrorrhizum_4[Number != 372]
#hist(G_macrorrhizum_4$Dry_biomass_grams_sixty_degrees_after_one_day)
#shapiro.test(G_macrorrhizum_4$Dry_biomass_grams_sixty_degrees_after_one_day)
#model  <- lm(Dry_biomass_grams_sixty_degrees_after_one_day ~ Biochar_Yes_No * GW_Yes_No, data = G_macrorrhizum_4)
#ggqqplot(residuals(model))
#shapiro.test(residuals(model))
#leveneTest(Dry_biomass_grams_sixty_degrees_after_one_day ~ Biochar_Yes_No * GW_Yes_No, data = G_macrorrhizum_4)
#fm <- aov(Dry_biomass_grams_sixty_degrees_after_one_day ~ Biochar_Yes_No * GW_Yes_No, data = G_macrorrhizum_4)
#glm <- glm()
#summary(fm) # no significance

#####water mass
data_drying$water_mass <- data_drying$`Biomass.before.drying.(g)`- data_drying$`Dry.biomass.(g).60°C/1.day`
water_data <- data.table(data_drying$Number,data_drying$Name,data_drying$water_mass,data_drying$`Type.of.biomass.(Leaves:.L,.Flowers:.F,.Rhizomes:.R)`,data_drying$`GW.(Yes/No)`,data_drying$`Biochar.(Yes/No)`)

colnames(water_data) <- c("number","species","water_mass","Type_of_biomass","greywater","biochar")
water_data <- water_data[species != "S. byzatina"]
#water_data
#above_Soil_F <- water_data[(Type_of_biomass == "F") & species == "A.maritima"]
#above_Soil
#hist(above_Soil_F$water_mass)
#shapiro.test(G_macrorrhizum_4$Dry_biomass_grams_sixty_degrees_after_one_day)
#model  <- lm(water_mass ~ biochar * greywater, data = above_Soil_F)
#ggqqplot(residuals(model))
#shapiro.test(residuals(model))
#leveneTest(water_mass ~ biochar * greywater, data = above_Soil_F)
#fm <- aov(water_mass ~ biochar * greywater, data = above_Soil_F)
#summary(fm)

#above_Soil_L <- water_data[(Type_of_biomass == "L") & species == "A.maritima"]
#above_Soil_L
#hist(above_Soil_L$water_mass)
#shapiro.test(G_macrorrhizum_4$Dry_biomass_grams_sixty_degrees_after_one_day)
#model  <- lm(water_mass ~ biochar * greywater, data = above_Soil_L)
#ggqqplot(residuals(model))
#shapiro.test(residuals(model))
#leveneTest(water_mass ~ biochar * greywater, data = above_Soil_L)
#fm <- aov(water_mass ~ biochar * greywater, data = above_Soil_L)
#summary(fm)

#below_Soil_R <- water_data[(Type_of_biomass == "R") & species == "A.maritima"]

#below_Soil_R
#boxplot(below_Soil_R$water_mass)
#hist(below_Soil_R$water_mass)
#shapiro.test(G_macrorrhizum_4$Dry_biomass_grams_sixty_degrees_after_one_day)
#model  <- lm(water_mass ~ biochar * greywater, data = below_Soil_R)
#ggqqplot(residuals(model))
#shapiro.test(residuals(model))
#leveneTest(water_mass ~ biochar * greywater, data = below_Soil_R)
#fm <- aov(water_mass ~ biochar * greywater, data = below_Soil_R)
#summary(fm)

#above_Soil_F <- water_data[(Type_of_biomass == "F") & species == "A.maritima"]
#above_Soil
#hist(above_Soil_F$water_mass)
#shapiro.test(G_macrorrhizum_4$Dry_biomass_grams_sixty_degrees_after_one_day)
#model  <- lm(water_mass ~ biochar * greywater, data = above_Soil_F)
#ggqqplot(residuals(model))
#shapiro.test(residuals(model))
#leveneTest(water_mass ~ biochar * greywater, data = above_Soil_F)
#fm <- aov(water_mass ~ biochar * greywater, data = above_Soil_F)

#above_Soil_L <- water_data[(Type_of_biomass == "L") & species == "G.Macrorrhizum"]
#above_Soil_L
#hist(above_Soil_L$water_mass)
#shapiro.test(G_macrorrhizum_4$Dry_biomass_grams_sixty_degrees_after_one_day)
#model  <- lm(water_mass ~ biochar * greywater, data = above_Soil_L)
#ggqqplot(residuals(model))
#shapiro.test(residuals(model))
#leveneTest(water_mass ~ biochar * greywater, data = above_Soil_L)
#fm <- aov(water_mass ~ biochar * greywater, data = above_Soil_L)
#summary(fm)

#below_Soil_R <- water_data[(Type_of_biomass == "R") & species == "G.Macrorrhizum"]
#below_Soil_R
#hist(below_Soil_R$water_mass)
#shapiro.test(G_macrorrhizum_4$Dry_biomass_grams_sixty_degrees_after_one_day)
#model  <- lm(water_mass ~ biochar * greywater, data = below_Soil_R)
#ggqqplot(residuals(model))
#shapiro.test(residuals(model))
#leveneTest(water_mass ~ biochar * greywater, data = below_Soil_R)
#fm <- aov(water_mass ~ biochar * greywater, data = below_Soil_R)
#summary(fm)
###
#drying_data
water_data[Type_of_biomass == "R", location_biomass := "below_ground"]
water_data[(Type_of_biomass == "L") | (Type_of_biomass == "F"), location_biomass := "above_ground"]
water_data
water_data[,mean(water_mass), by = .(location_biomass,species,greywater,biochar)]

above_Soil_M <- water_data[(location_biomass == "above_ground") & species == "A.maritima"]
above_Soil_M[,sum(water_mass),by = number]
above_Soil_M <- above_Soil_M[,sum(water_mass),by = number]
info
colnames(above_Soil_M) <- c("plant_id","water_mass_above_ground")
above_Soil_M$plant_id <- as.factor(above_Soil_M$plant_id)
str(above_Soil_M)
str(info)
above_Soil_M <- merge(above_Soil_M, info,by = "plant_id")
hist(above_Soil_M$water_mass_above_ground)
above_Soil_M
shapiro.test(G_macrorrhizum_4$Dry_biomass_grams_sixty_degrees_after_one_day)

model  <- lm(water_mass_above_ground ~ biochar * greywater, data = above_Soil_M)
ggqqplot(residuals(model))
shapiro.test(residuals(model))
leveneTest(water_mass_above_ground ~ biochar * greywater, data = above_Soil_M)
fm <- aov(water_mass_above_ground ~ biochar * greywater, data = above_Soil_M)
summary(fm)
plot(fm)
posthoc <- TukeyHSD(fm)
above_Soil_M
data_summary <- group_by(above_Soil_M, greywater, biochar) %>%
  summarise(mean=mean(water_mass_above_ground), quant=quantile(water_mass_above_ground,probs = .75)) %>%
  arrange(desc(mean))
cld <- multcompLetters4(fm,posthoc)
cld
cld <- as.data.frame.list(cld$`biochar:greywater`)
cld$Letters
data_summary$Tukey <- cld$Letters
data_summary
data_plot
ggplot(data_plot, aes(species,mean, col = biochar)) + 
  geom_boxplot() +
  labs(x="species type Type", y="Weight (g)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(data = data_summary, aes(x = species, y = quant, label = cld, col = biochar))

ggboxplot(above_Soil_M, x = "biochar", y = "water_mass_above_ground",  color = "greywater")+ylab("water mass above ground")



below_Soil_M <- water_data[(location_biomass == "below_ground") & species == "A.maritima"]
above_Soil_M[,sum(water_mass),by = number]
below_Soil_M <- above_Soil_M[,sum(water_mass),by = number]
info
below_Soil_M
#colnames(above_Soil_M) <- c("plant_id","water_mass_above_ground")
#above_Soil_M$plant_id <- as.factor(above_Soil_M$plant_id)
#str(above_Soil_M)
#str(info)
#above_Soil_M <- merge(above_Soil_M, info,by = "plant_id")
hist(below_Soil_M$water_mass)
shapiro.test(G_macrorrhizum_4$Dry_biomass_grams_sixty_degrees_after_one_day)
model  <- lm(water_mass ~ biochar * greywater, data = below_Soil_M)
ggqqplot(residuals(model))
shapiro.test(residuals(model))
leveneTest(water_mass ~ biochar * greywater, data = below_Soil_M)
below_Soil_M$number <- as.factor(below_Soil_M$number)
fm <- aov(water_mass ~ biochar * greywater, data = below_Soil_M)
summary(fm)
str(below_Soil_M)
str(above_Soil_M)
above_Soil_G <- water_data[(location_biomass == "above_ground") & species == "G.Macrorrhizum"]
above_Soil_G

above_Soil_G <- above_Soil_G[,sum(water_mass),by = number]
above_Soil_G
info
colnames(above_Soil_G) <- c("plant_id","water_mass_above_ground")
above_Soil_G$plant_id <- as.factor(above_Soil_G$plant_id)
str(above_Soil_M)
str(info)
above_Soil_G <- merge(above_Soil_G, info,by = "plant_id")
above_Soil_G
hist(above_Soil_G$water_mass_above_ground)
shapiro.test(G_macrorrhizum_4$Dry_biomass_grams_sixty_degrees_after_one_day)

model  <- lm(water_mass_above_ground ~ biochar* greywater, data = above_Soil_G)
ggqqplot(residuals(model))
shapiro.test(residuals(model))
leveneTest(water_mass_above_ground ~ biochar* greywater, data = above_Soil_G)
fm <- aov(water_mass_above_ground ~ biochar* greywater, data = above_Soil_G)
summary(fm)
plot(fm)
above_Soil_G
posthoc <- TukeyHSD(fm)
above_Soil_M
data_summary <- group_by(above_Soil_G,biochar) %>%
  summarise(mean=mean(water_mass_above_ground), quant=quantile(water_mass_above_ground,probs = .75)) %>%
  arrange(desc(mean))
cld <- multcompLetters4(fm,posthoc)
cld$biochar
cld <- as.data.frame.list(cld$biochar)
cld$Letters
data_summary$Tukey <- cld$Letters
data_summary
data_plot
ggplot(data_plot, aes(species,mean, col = biochar)) + 
  geom_boxplot() +
  labs(x="species type Type", y="Weight (g)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(data = data_summary, aes(x = species, y = quant, label = cld, col = biochar))

ggboxplot(above_Soil_M, x = "biochar", y = "water_mass_above_ground")+ylab("water mass above ground")

below_Soil_G <- water_data[(location_biomass == "below_ground") & species == "G.Macrorrhizum"]

hist(below_Soil_G$water_mass)
shapiro.test(G_macrorrhizum_4$Dry_biomass_grams_sixty_degrees_after_one_day)
below_G <- below_Soil_G[number != 372]
below_G
model  <- lm(water_mass ~ biochar * greywater, data = below_G)
ggqqplot(residuals(model))
shapiro.test(residuals(model))
leveneTest(water_mass ~ biochar * greywater, data = below_G)
fm <- aov(water_mass ~ biochar * greywater, data = below_G)
summary(fm)
######################
data_drying <- read.xlsx(xlsxFile = "Copy_of_Copy_of_Results_pot_experiment_2021_2022_all.xlsx",sheet = "Final_results")
data_drying$`Type.of.biomass.(Leaves:.L,.Flowers:.F,.Rhizomes:.R)` <- as.factor(data_drying$`Type.of.biomass.(Leaves:.L,.Flowers:.F,.Rhizomes:.R)`)
data_drying <- as.data.table(data_drying)
data_drying[`Type.of.biomass.(Leaves:.L,.Flowers:.F,.Rhizomes:.R)` == "R", location_biomass := "below_ground"]
data_drying[(`Type.of.biomass.(Leaves:.L,.Flowers:.F,.Rhizomes:.R)` == "L") | (`Type.of.biomass.(Leaves:.L,.Flowers:.F,.Rhizomes:.R)` == "F"), location_biomass := "above_ground"]
water_data
data_drying <- data_drying[,c(1,2,4,6,7,10)]
data_drying
colnames(data_drying) <- c("number","species","dry_mass","greywater","biochar","location_biomass")
data_drying[,mean(dry_mass), by = .(location_biomass,species,greywater,biochar)]

above_Soil_M <- data_drying[(location_biomass == "above_ground") & species == "A.maritima"]
above_Soil_M[,sum(dry_mass),by = number]
above_Soil_M <- above_Soil_M[,sum(dry_mass),by = number]
above_Soil_M
info
colnames(above_Soil_M) <- c("plant_id","dry_mass_above_ground")
above_Soil_M$plant_id <- as.factor(above_Soil_M$plant_id)
str(above_Soil_M)
str(info)
above_Soil_M <- merge(above_Soil_M, info,by = "plant_id")
above_Soil_M
hist(above_Soil_M$dry_mass_above_ground)
above_Soil_M
shapiro.test(G_macrorrhizum_4$Dry_biomass_grams_sixty_degrees_after_one_day)
above
above_Soil_M
model  <- lm(dry_mass_above_ground ~ biochar * greywater, data = above_Soil_M)
ggqqplot(residuals(model))
shapiro.test(residuals(model))
leveneTest(dry_mass_above_ground ~ biochar * greywater, data = above_Soil_M)
fm <- aov(dry_mass_above_ground ~ biochar * greywater, data = above_Soil_M)
summary(fm)
plot(fm)

below_Soil_M <- data_drying[(location_biomass == "below_ground") & species == "A.maritima"]
below_Soil_M[,sum(water_mass),by = number]
below_Soil_M
#below_Soil_M <- below_Soil_M[,sum(dry_mass),by = number]
info
below_Soil_M
#colnames(above_Soil_M) <- c("plant_id","water_mass_above_ground")
#above_Soil_M$plant_id <- as.factor(above_Soil_M$plant_id)
#str(above_Soil_M)
#str(info)
#above_Soil_M <- merge(above_Soil_M, info,by = "plant_id")
hist(below_Soil_M$dry_mass)
shapiro.test(G_macrorrhizum_4$Dry_biomass_grams_sixty_degrees_after_one_day)
model  <- lm(dry_mass ~ biochar * greywater, data = below_Soil_M)
ggqqplot(residuals(model))
shapiro.test(residuals(model))
leveneTest(dry_mass ~ biochar * greywater, data = below_Soil_M)
fm <- aov(dry_mass ~ biochar * greywater, data = below_Soil_M)
summary(fm)
data_drying
above_Soil_G <- data_drying[(location_biomass == "above_ground") & species == "G.Macrorrhizum"]
above_Soil_G
above_Soil_M[,sum(dry_mass),by = number]

above_Soil_G <- above_Soil_G[,sum(dry_mass),by = number]
above_Soil_G
info
colnames(above_Soil_G) <- c("plant_id","dry_mass_above_ground")
above_Soil_G$plant_id <- as.factor(above_Soil_G$plant_id)
str(above_Soil_M)
str(info)
above_Soil_G <- merge(above_Soil_G, info,by = "plant_id")
above_Soil_G
hist(above_Soil_G$dry_mass_above_ground)
above_Soil_M
shapiro.test(G_macrorrhizum_4$Dry_biomass_grams_sixty_degrees_after_one_day)
above
model  <- lm(dry_mass_above_ground ~ biochar * greywater, data = above_Soil_G)
ggqqplot(residuals(model))
shapiro.test(residuals(model))
leveneTest(dry_mass_above_ground ~ biochar * greywater, data = above_Soil_G)
above_Soil_G
fm <- aov(dry_mass_above_ground ~ biochar * greywater, data = above_Soil_G)
summary(fm)
posthoc <- TukeyHSD(fm)

data_summary <- group_by(above_Soil_G, greywater, biochar) %>%
  summarise(mean=mean(water_mass_above_ground), quant=quantile(water_mass_above_ground,probs = .75)) %>%
  arrange(desc(mean))
cld <- multcompLetters4(fm,posthoc)
cld$
cld <- as.data.frame.list(cld$`biochar:greywater`)
cld$biochar
cld$Letters
data_summary$Tukey <- cld$Letters
data_summary
data_plot
ggplot(data_plot, aes(species,mean, col = biochar)) + 
  geom_boxplot() +
  labs(x="species type Type", y="Weight (g)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(data = data_summary, aes(x = species, y = quant, label = cld, col = biochar))

ggboxplot(above_Soil_M, x = "biochar", y = "water_mass_above_ground",  color = "greywater")+ylab("water mass above ground")


below_Soil_G <- data_drying[(location_biomass == "below_ground") & species == "G.Macrorrhizum"]
below_Soil_G[,sum(water_mass),by = number]
below_Soil_M
#below_Soil_M <- below_Soil_M[,sum(dry_mass),by = number]
info
below_Soil_M
#colnames(above_Soil_M) <- c("plant_id","water_mass_above_ground")
#above_Soil_M$plant_id <- as.factor(above_Soil_M$plant_id)
#str(above_Soil_M)
#str(info)
#above_Soil_M <- merge(above_Soil_M, info,by = "plant_id")
hist(below_Soil_G$dry_mass)
shapiro.test(G_macrorrhizum_4$Dry_biomass_grams_sixty_degrees_after_one_day)
model  <- lm(dry_mass ~ biochar * greywater, data = below_Soil_G)
ggqqplot(residuals(model))
shapiro.test(residuals(model))
#sapiro test fails futher analysis is needed
leveneTest(dry_mass ~ biochar * greywater, data = below_Soil_G)
fm <- aov(dry_mass ~ biochar * greywater, data = below_Soil_G)
summary(fm)
