###### 1. Install and Load Required Packages ######
library(lme4)
library(lmerTest)
library(dplyr)
library(readxl)
library(psych)
library(tidyr)
library(tidyverse)
library(multilevel)
library(misty)

###### 2.Descriptive statistics ######
ILA_study3 <- read_excel("20241118/Nature Human Behaviour/final/code/study3/data/ILA_study3.xlsx")
usedata<-na.omit(ILA_study3)
# Centralize the measurement values of each participant
data_centered <- usedata %>%
  group_by(Number) %>%
  mutate(center_RIS = RIS - mean(RIS)) %>%
  mutate(center_stress = stress - mean(stress)) %>%
  mutate(center_depression = depression - mean(depression)) %>%
  ungroup()

count_data <- data_centered %>%
  group_by(Number) %>%
  summarise(count = n())


data_centered<- as.data.frame(data_centered)
mult.icc(data_centered[,c("RIS","stress","depression","SA","SM")],grpid=data_centered$Number)
#Calculate intra individual and inter individual correlations
multilevel.cor(data_centered[, c("RIS", "stress", "depression","SA","SM")],
               cluster = data_centered$Number, print = "all")

mean(usedata$RIS)
sd(usedata$RIS)
mean(usedata$stress)
sd(usedata$stress)
mean(usedata$depression)
sd(usedata$depression)
mean(usedata$SA)
sd(usedata$SA)
mean(usedata$SM)
sd(usedata$SM)


###### 3.The relationship between daily rest intolerance, daily stress, daily depression, and daily outcomes######
modelK1 <- lmer(SA ~ day+week+age+gender+ center_RIS+center_stress+center_depression + (1 | Number), data = data_centered)
summary(modelK1)
modelK2 <- lmer(SA ~ day+week+age+gender+ center_RIS+center_stress+center_depression + (1+day+week | Number), data = data_centered)
summary(modelK2)
modelK3 <- lmer(SA ~ day+week+age+gender+ center_RIS+center_stress+center_depression + (1+day+week+center_RIS+center_stress+center_depression | Number), data = data_centered)
summary(modelK3)
anova(modelK1,modelK2)
anova(modelK2,modelK3)

modelN1 <- lmer(SM ~ day+week+age+gender+ center_RIS+center_stress+center_depression + (1 | Number), data = data_centered)
summary(modelN1)
modelN2 <- lmer(SM ~ day+week+age+gender+ center_RIS+center_stress+center_depression + (1+day+week | Number), data = data_centered)
summary(modelN2)
modelN3 <- lmer(SM ~ day+week+age+gender+ center_RIS+center_stress+center_depression + (1+day+week+center_RIS+center_stress+center_depression | Number), data = data_centered)
summary(modelN3)
anova(modelN1,modelN2)
anova(modelN2,modelN3)

###### 4.Lagged effects of daily rest intolerance, stress, and depression on next-day outcomes######
# Firstly, ensure that the data is sorted by participant and date
data_centered1 <- data_centered[order(data_centered$Number, data_centered$day), ]
#Create lagged variables
data_centered1 <- data_centered1 %>%
  group_by(Number) %>% 
  mutate(
    lag1_center_RIS = lag(center_RIS, 1),
    lag1_center_stress = lag(center_stress, 1),
    lag1_center_depression = lag(center_depression, 1),
  ) %>%
  ungroup()

# Now using lagged variables to build models
modelN1_lag1 <- lmer(SM ~ day + week + age + gender + 
                       lag1_center_RIS + lag1_center_stress + lag1_center_depression + 
                       (1 | Number), 
                     data = data_centered1)

summary(modelN1_lag1)

modelA1_lag1 <- lmer(SA ~ day + week + age + gender + 
                       lag1_center_RIS + lag1_center_stress + lag1_center_depression + 
                       (1 | Number), 
                     data = data_centered1)

summary(modelA1_lag1)