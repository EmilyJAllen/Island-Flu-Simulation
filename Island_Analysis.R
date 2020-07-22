####################################
# R code for Island Research, Group 3
# Allendorf & Wang
###################################

library(readr)
library(ggplot2)
library(dplyr)
library(lme4)
library(car)
library(ez)
library(agricolae)
library(tidyverse)

# RANDOMIZATION ##################
set.seed(123)

# generating random cities from total island cities
samples2<- sample(x = c(1:36), size = 126, replace = T) 
samples2

sample(1:5,1) #example of generating a hamlet from a city of 5 hamlets

####################################
### LOADING & CLEANING DATA ########


Flu <- read_csv("Lec1_Group3_Final_Data.csv", 
                         col_types = cols(Sex = col_character()))

# replacing missing values with group means
Flu %>% group_by(Name) %>%
  mutate(dopamine_level=ifelse(is.na(dopamine_level),mean(dopamine_level,na.rm=TRUE),dopamine_level)) -> Flu
Flu %>% group_by(Name) %>%
  mutate(WBC_count=ifelse(is.na(WBC_count),mean(WBC_count,na.rm=TRUE),WBC_count)) -> Flu
Flu %>% group_by(Name) %>%
  mutate(serotonin_level=ifelse(is.na(serotonin_level),mean(serotonin_level,na.rm=TRUE),serotonin_level)) -> Flu

#assigning variables to factors
Flu$Activity <- as.factor(Flu$Activity)
Flu$Sex <- as.factor(Flu$Sex)
Flu$Name <- as.factor(Flu$Name)
Flu$days_since_injection <- as.numeric(Flu$days_since_injection) #keep this numeric, factor in models
Flu$Age_block <- as.factor(Flu$Age_block)

#removing extra data collection
Flu <- Flu[!(Flu$days_since_injection== 3),]




#### GROUP MEANS ####################
# used later in visualizations
######################################

# White Blood Cell count#
  #by Sex
WBC_by_days_sex = Flu %>% group_by(days_since_injection, Sex) %>% summarize(WBC_count = mean(WBC_count, na.rm= T))
WBC_by_days_sex

  #by activity 
WBC_by_days_act = Flu %>% group_by(Activity, days_since_injection)%>% 
  summarize(WBC_count = mean(WBC_count, na.rm=T))
WBC_by_days_act

  #by age 
WBC_by_days_age = Flu %>% filter(is.na(Age)==FALSE ) %>% group_by(Age_block, days_since_injection)%>% 
  summarize(WBC_count = mean(WBC_count, na.rm=T))
WBC_by_days_age

#Serotonin Levels #######
  #by sex
Ser_by_days_sex = Flu %>% group_by(days_since_injection, Sex)%>% 
  summarize(serotonin_level = mean(serotonin_level, na.rm=T))
Ser_by_days_sex

  #by activity
Ser_by_days_act = Flu %>% group_by(Activity,days_since_injection, Sex, Age_block)%>% 
  summarize(serotonin_level = mean(serotonin_level, na.rm=T))
Ser_by_days_act
  
  #by age 
Ser_by_days_age = Flu %>% group_by(Age_block,days_since_injection)%>% 
  summarize(serotonin_level = mean(serotonin_level, na.rm=T))
Ser_by_days_age

#Dopamine Levels ########
  #by sex
Dop_by_sex_days = Flu %>% group_by(days_since_injection, Sex)%>% 
  summarize(dopamine_level = mean(dopamine_level, na.rm = T))
Dop_by_sex_days

  #by days since injection
Dop_by_days_act = Flu %>% group_by(Activity,days_since_injection)%>% 
  summarize(dopamine_level = mean(dopamine_level, na.rm=T))
Dop_by_days_act

  #by age
Dop_by_days_age = Flu %>% group_by(Age_block,days_since_injection)%>% 
  summarize(dopamine_level = mean(dopamine_level, na.rm=T))
Dop_by_days_age

# Group by all predictors #
Flu_by_days = Flu %>% group_by(days_since_injection,Age_block, Sex, Activity )%>% 
  summarize(dopamine_level = mean(dopamine_level, na.rm=T),
            WBC_count = mean(WBC_count, na.rm = T),
            serotonin_level = mean(serotonin_level, na.rm = T))
Flu_by_days





#####################################
# Data Exploration ##################
#####################################

# longitudinal presentation ###

# serotonin, by activity
ggplot(data=Ser_by_days_act , aes((days_since_injection), serotonin_level, color=Activity))+ 
  geom_line(size = 1.3) + facet_grid(Age_block~Sex) +
  ggtitle("Mean Serotonin Levels Since Injection by Activity") +theme_light()
# serotonin, by sex
ggplot(data=Ser_by_days_sex , aes((days_since_injection), serotonin_level, color=Sex))+ 
  geom_line(size = 1.3) +ggtitle("Mean Serotonin Levels Since Injection by Sex") + theme_light()+
  xlab("Days Since Injection") + ylab("Serotonin (ng/mL) ")
# serotonin by age
ggplot(data=Ser_by_days_age , aes((days_since_injection), serotonin_level, color=Age_block))+ 
  geom_line(size = 1.3) +ggtitle("Mean Serotonin Levels Since Injection by Age") + theme_light()+
  xlab("Days Since Injection") + ylab("Serotonin (ng/mL) ")


# dopamine by activity 
ggplot(data=Dop_by_days_act , aes((days_since_injection), dopamine_level, color=Activity))+ 
  geom_line(size = 1.3) +ggtitle("Mean Dopamine Since Injection by Activity")+ theme_light() +
  xlab("Days Since Injection") + ylab("Dopamine (micrograms/h)")
# dopamine by sex 
ggplot(data=Dop_by_sex_days , aes((days_since_injection), dopamine_level, color=Sex))+ 
  geom_line(size = 1.3) +ggtitle("Mean Dopamine Levels Since Injection by Sex") + theme_light()+
  xlab("Days Since Injection") + ylab("Dopamine (micrograms/h) ")
# dopamine by age
ggplot(data=Dop_by_days_age , aes((days_since_injection), dopamine_level, color=Age_block))+ 
  geom_line(size = 1.3) +ggtitle("Mean Dopamine Levels Since Injection by Age") + theme_light()+
  xlab("Days Since Injection") + ylab("Dopamine (ng/mL) ")


# WBC by activity 
ggplot(data=WBC_by_days_act , aes((days_since_injection), WBC_count, color=Activity))+ 
  geom_line(size = 1.3) +ggtitle("Mean White Blood Cell Count Since Injection by Activity") +
  theme_light() + xlab("Days Since Injection") + ylab("White Blood Cells / Liter ")
# WBC by sex
ggplot(data=WBC_by_days_sex , aes((days_since_injection), WBC_count, color=Sex))+ 
  geom_line(size = 1.3) +ggtitle("Mean White Blood Cell Count Since Injection by Sex") +
  theme_light() + xlab("Days Since Injection") + ylab("White Blood Cells / Liter ")
# WBC by age
ggplot(data=WBC_by_days_age , aes((days_since_injection), WBC_count, color=Age_block))+ 
  geom_line(size = 1.3) +ggtitle("Mean White Blood Cell Count Since Injection by Age") +
  theme_light() + xlab("Days Since Injection") + ylab("White Blood Cells / Liter ")



# 2 X 2 interaction graphs ###

# White Blood Cell Count
# New facet label names for age
age_labs <- c("Under 30", "30 and Over")
names(age_labs) <- c("1", "2")

# New facet label names for sex
sex_labs <- c("Female", "Male")
names(sex_labs) <- c("F", "M")

WBC_by_days_act = Flu %>% group_by(Activity, days_since_injection, Age_block, Sex)%>% 
  summarize(WBC_count = mean(WBC_count, na.rm=T))
ggplot(data=WBC_by_days_act , aes((days_since_injection), WBC_count, color=Activity))+ 
  geom_line(size = 1) + facet_grid(Age_block~Sex, 
                                     labeller = labeller(Sex = sex_labs,Age_block = age_labs)) +
  theme_light() + xlab("Days Since Injection") + ylab("White Blood Cells / Liter ")


# Dopamine Level
Dop_by_days_act = Flu %>% group_by(Activity,days_since_injection, Age_block, Sex)%>% 
  summarize(dopamine_level = mean(dopamine_level, na.rm=T))
ggplot(data=Dop_by_days_act , aes((days_since_injection), dopamine_level, color=Activity))+ 
  geom_line(size = 1) + facet_grid(Age_block~Sex, labeller = labeller(Sex = sex_labs,
                                                                        Age_block = age_labs) ) +
  theme_light() +
  xlab("Days Since Injection") + ylab("Dopamine (micrograms/h)")


# Serotonin Level
Ser_by_days_act = Flu %>% group_by(Activity,days_since_injection, Sex, Age_block)%>% 
  summarize(serotonin_level = mean(serotonin_level, na.rm=T))
ggplot(data=Ser_by_days_act , aes((days_since_injection), serotonin_level, color=Activity))+ 
  geom_line(size = 1) + facet_grid(Age_block~Sex, labeller = labeller(Sex = sex_labs,
                                                                        Age_block = age_labs)) +
  theme_light()



#####################################
# MODEL #############################
#####################################

# Serotonin Models
lmmod1 <- lm(serotonin_level ~  Activity * as.factor(days_since_injection) * Sex * Age_block, data = Flu)

aov1 <- aov(serotonin_level ~ Activity * as.factor(days_since_injection) * Sex * Age_block+ Error(as.factor(Name)), data=Flu)

lmer1 <- lmer(serotonin_level ~ Activity * as.factor(days_since_injection) * Sex * Age_block+ (1|Name), data= Flu)

  #ICC
  ICC = (621.19) / ((621.19) + (31.97 ) ); ICC # 0.9510533
  
  #testing if subject differences are significant
  1-pchisq(2* ( logLik(lmer1)-logLik(lmmod1)), 1)
  
  #Diagnostic Plots
  aov1b <- aov(serotonin_level ~ as.factor(Activity)* Sex * days_since_injection, data=Flu)
  par(mfrow = c(2,2))
  plot(aov1b)
  
  

# WBC Count Models
lmmod2 <- lm(dopamine_level ~  Activity * as.factor(days_since_injection) * Sex * Age_block, data=Flu)

aov2 <- aov(WBC_count ~ Activity * as.factor(days_since_injection)* Sex * Age_block + Error(as.factor(Name)), data=Flu)

lmer2 <- lmer(WBC_count ~  Activity * as.factor(days_since_injection)* Sex * Age_block+(1|Name), data = Flu)

  #ICC
  ICC = 1.453e+18 / (1.453e+18+1.248e+17); ICC # 0.9209025
  
  #testing if subject differences are significant
  1-pchisq(2* ( logLik(lmer2)-logLik(lmmod2)), 1)
  
  #modified model
  
  aov2b<- aov(WBC_count ~ Activity * as.factor(days_since_injection)* Sex * Age_block, data = Flu)
  summary(aov2b)
  
  # Diagnostic Plots
  par(mfrow = c(2,2))
  plot(aov2b)

  
# Dopaine Models
lmmod3 <- lm(dopamine_level ~  as.factor(Activity) * as.factor(days_since_injection) * Sex * Age_block, data=Flu, na.rm = T)

aov3 <- aov(dopamine_level ~ as.factor(Activity) * as.factor(days_since_injection) * Sex * Age_block + Error(as.factor(Name)), data=Flu)

lmer3 <- lmer(dopamine_level ~  as.factor(Activity) * as.factor(days_since_injection)* Sex * Age_block+(1|Name), data = Flu )

  #ICC
  ICC = 4.0016   / (4.0016   +  0.2463) ; ICC # 0.9420184
  
  #testing if subject differences are significant
  1-pchisq(2* (logLik(lmer3) - logLik(lmmod3)), 1)
  
  #Diagostic Plots
  aov3 <- aov(dopamine_level ~ as.factor(Activity), data=Flu)
  par(mfrow = c(2,2))
  plot(aov3)


  
m1 <- aov(WBC_count ~ Activity*days_since_injection*Sex*Age_block + Error(factor(Name)), data = Flu)
summary(m1) 
  
#####################################
# GENERATING PRETTY ANOVA TABLES#####
#####################################
library(apaTables)
df <- read_csv("Lec1_Group3_Final_Data.csv")
df <- df[!(df$Sex=="F" & df$days_since_injection==3), ]
colnames(df)[5] <- "days"

m1 <- aov(WBC_count ~ as.factor(Activity)*as.factor(days)*as.factor(Sex)*factor(Age_block) + Error(factor(Name)), data = df)
m2 <- aov(serotonin_level ~as.factor(Activity)*as.factor(days)*as.factor(Sex)*factor(Age_block) + Error(factor(Name)), data = df)
m3 <- aov(dopamine_level ~ as.factor(Activity)*as.factor(days)*as.factor(Sex)*factor(Age_block) + Error(factor(Name)), data = df)

df %>% group_by(Name) %>%
mutate(dopamine_level=ifelse(is.na(dopamine_level),mean(dopamine_level,na.rm=TRUE),dopamine_level)) -> df2

df2 %>% group_by(Name) %>%
  mutate(count = n()) %>%
  filter(count == 5) %>% ungroup()-> df3

df3$days <- factor(df3$days)
df3$Age_block <- factor(df3$Age_block)
table3 <- ezANOVA(data = df3,dv = .(dopamine_level), wid = .(Name),
                  between = .(Activity, Sex, Age_block), within = .(days),
                        type = 2, detailed = TRUE)
# writes table to computer
# out_table_3 <- apa.ezANOVA.table(table3, 
#                                   filename = "Table3_APA.doc", 
#                                  table.title = "dopamine level ANOVA result",
#                                   table.number = 3)

table2 <- ezANOVA(data = df3,dv = .(serotonin_level), wid = .(Name),
                  between = .(Activity, Sex, Age_block), within = .(days),
                        type = 2, detailed = TRUE)
# out_table_2 <- apa.ezANOVA.table(table2, 
#                                   filename = "Table2_APA.doc", 
#                                  table.title = "serotonin level ANOVA result",
#                                   table.number = 2)

df3 %>% group_by(Name) %>%
mutate(WBC_count=ifelse(is.na(WBC_count),mean(WBC_count,na.rm=TRUE),WBC_count)) -> df4

table1 <- ezANOVA(data = df4,dv = .(WBC_count), wid = .(Name),
                  between = .(Activity, Sex, Age_block), within = .(days),
                        type = 2, detailed = TRUE)

# out_table_1 <- apa.ezANOVA.table(table1, 
#                                   filename = "Table1_APA.doc", 
#                                  table.title = "white blood cell count ANOVA result",
#                                   table.number = 1)



#####################################
# POST HOC GRAPHS####################
#####################################

# White Blood Cell Count 
TUKEY <- (TukeyHSD(aov(WBC_count~Activity*Sex*Age_block, data=Flu)))
TUKEY_data <- as.data.frame(TUKEY$`Activity:Sex:Age_block`)
TUKEY_data <- TUKEY_data[TUKEY_data$`p adj`<=.05,]
ggplot(TUKEY_data, aes(rownames(TUKEY_data), diff))+ geom_errorbar(aes(ymin = `lwr`, ymax = `upr`))+
  geom_point(cex= 5, pch = 15)+
  ylab("WBC Count Effect")+ xlab(" ")+
  theme_light()+ geom_hline(yintercept = 0,linetype = 88) +
  theme(axis.text.x = element_text(angle = -45, vjust = 0.5, hjust=.5))

# Dopamine 
TUKEY <- (TukeyHSD(aov(dopamine_level~Activity, data=Flu)))
TUKEY_data <- as.data.frame(TUKEY$Activity)
ggplot(TUKEY_data, aes(rownames(TUKEY_data), diff))+ geom_errorbar(aes(ymin = `lwr`, ymax = `upr`))+
  geom_point( cex= 5, pch = 15)+xlab(" ")+
  ylab("Dopamine Effect") + ggtitle("Effects Plot of Activity on Dopamine Level")+
  theme_light()+ geom_hline(yintercept = 0,linetype = 88) +
  theme(axis.text.x = element_text(angle = -25, vjust = 0.5, hjust=.5))

# Serotonin
library(gridExtra)
library(grid)

TUKEY <- TukeyHSD(aov(serotonin_level~as.factor(days_since_injection)* Activity, data=Flu %>% filter(Sex == "F")))
TUKEY_data <- as.data.frame(TUKEY$`Activity`)
#TUKEY_data <- TUKEY_data[TUKEY_data$`p adj`<=.05,]
ggplot(TUKEY_data, aes(rownames(TUKEY_data), diff))+ geom_errorbar(aes(ymin = `lwr`, ymax = `upr`))+
  geom_point(cex= 5, pch = 15)+
   ylab("Serotonin Effect") + xlab(" ")+
  theme_light()+ geom_hline(yintercept = 0,linetype = 88) +
  theme(axis.text.x = element_text(angle = -35, vjust = 0.5, hjust=.25))

TUKEY <- TukeyHSD(aov(serotonin_level~as.factor(days_since_injection)* Activity, data=Flu %>% filter(Sex == "M")))
TUKEY_data <- as.data.frame(TUKEY$Activity)
#TUKEY_data <- TUKEY_data[TUKEY_data$`p adj`<=.1,]
ggplot(TUKEY_data, aes(rownames(TUKEY_data), diff))+ geom_errorbar(aes(ymin = `lwr`, ymax = `upr`))+
  geom_point(cex= 5, pch = 15)+ xlab(" ")+
  ylab("Serotonin Effect") +
  theme_light()+ geom_hline(yintercept = 0,linetype = 88) +
  theme(axis.text.x = element_text(angle = -35, vjust = 0.5, hjust=.25))






# plotting highest ICC individual = diagnostics by individual
###############
ggplot(data= Flu, 
       aes(x = days_since_injection, y = WBC_count))+ geom_point() + 
  geom_line(aes(color = Name), show.legend = F)+ theme_light()+
  ggtitle("WBC Count for Each Individual Over Time") + xlab("Days Since Injection")+
  ylab("White Blood Cell Count/ L")

ggplot(data= Flu, 
       aes(x = days_since_injection, y = serotonin_level))+ geom_point() + 
  geom_line(aes(color = Name), show.legend = F)+ theme_light()+
  ggtitle("Serotonin Level for Each Individual Over Time") + xlab("Days Since Injection")+
  ylab("Serotonin (ng/mL)")

ggplot(data= Flu, 
       aes(x = days_since_injection, y = dopamine_level))+ geom_point() + 
  geom_line(aes(color = Name), show.legend = F)+ theme_light()+
  ggtitle("Dopamine Level for Each Individual Over Time") + xlab("Days Since Injection")+
  ylab("Dopamine Level (micrograms/h)")

