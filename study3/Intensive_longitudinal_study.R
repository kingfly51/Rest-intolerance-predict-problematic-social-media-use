intensive_data<-read.csv("Statistic_second.csv",fileEncoding = "GBK")
library(tidyr)

trans_inten_data<- intensive_data %>%
  pivot_wider(id_cols = c(No, date),names_from = type, values_from = c(use_time_sum,use_time_count,
                                                                       num_start_sum,num_start_count,
                                                                       num_notification_sum,num_notification_count,
                                                                       use_time_mean,use_time_std,
                                                                       num_start_mean,num_start_std,
                                                                       num_notification_mean,num_notification_std))
#write.csv(trans_inten_data,"APP_wide_data.csv")
min_data <- trans_inten_data
min_data[, c(3:22,123:162)] <- min_data[, c(3:22,123:162)]/60000
library(writexl)
write_xlsx(min_data,"APP_wide_data_min.xlsx")
write.csv(min_data,"APP_wide_data_min.csv")




scale_data<-read.csv("D:\\Rdaima\\Smartphone_sleep\\晚测+晨测+前测+后测数据.csv",fileEncoding = "GBK")
library(readxl)
library(writexl)
phone_data <- read_excel("APP_wide_data_min_phonedata.xlsx")
merged_scale_phone <- merge(scale_data, phone_data, by = "唯一序号", all = TRUE)
write_xlsx(merged_scale_phone,"merged_scale_phone.xlsx")


######正式数据处理——————————
library(readxl)
merged_scale_phone <- read_excel("merged_scale_phone _删除异常值_2.xlsx")
#indata<-merged_scale_phone[!is.na(merged_scale_phone$No),]
indata<-merged_scale_phone[!is.na(merged_scale_phone$Number),]
#indata<-indata[,c(3,441,13:15,25,26,32,43,120,121,123,443,444,448,463,464,468,483,484,488,503,504,508,523,524,528,543,544,548,603,604,608)]
indata<-indata[,c(3,683,159,160,168,13:15,25,26,32,43,120,121,123)]
indata[indata == "非常不同意"] <- "1"
indata[indata == "不同意"] <- "2"
indata[indata == "比较不同意"] <- "3"
indata[indata == "一般"] <- "4"
indata[indata == "比较同意"] <- "5"
indata[indata == "同意"] <- "6"
indata[indata == "非常同意"] <- "7"
indata[indata == "一点也不"] <- "1"
indata[indata == "极度"] <- "10"
indata[indata == "非常不想"] <- "1"
indata[indata == "不想"] <- "2"
indata[indata == "比较不想"] <- "3"
indata[indata == "一般"] <- "4"
indata[indata == "比较想"] <- "5"
indata[indata == "想"] <- "6"
indata[indata == "非常想"] <- "7"
colnames(indata)[2:15]<-c("day","gender","age","education","sa1","sa2","sa3","ris1","ris2","stress","depression","sm1","sm2","short_vedio")
indata$ris1<-as.numeric(indata$ris1)
indata$ris2<-as.numeric(indata$ris2)
indata$sa1<-as.numeric(indata$sa1)
indata$sa2<-as.numeric(indata$sa2)
indata$sa3<-as.numeric(indata$sa3)
indata$sm1<-as.numeric(indata$sm1)
indata$sm2<-as.numeric(indata$sm2)

indata$stress<-as.numeric(indata$stress)
indata$depression<-as.numeric(indata$depression)
indata$short_vedio<-as.numeric(indata$short_vedio)
#indata$num_start_std_媒体共享<-as.numeric(indata$num_start_std_媒体共享)
#indata$num_notification_std_媒体共享<-as.numeric(indata$num_notification_std_媒体共享)
#indata$use_time_std_媒体共享<-as.numeric(indata$use_time_std_媒体共享)
indata$RIS<- indata$ris1+indata$ris2
indata$SA<- indata$sa1+indata$sa2+indata$sa3
indata$SM<- indata$sm1+indata$sm2
indata$Number<-as.factor(indata$Number)
indata$gender<-as.factor(indata$gender)
indata$week<-indata$day
indata$week[indata$week<8]<-1
indata$week[indata$week>7]<-2
#write.csv(indata,"indata.csv")
library(lme4)
library(lmerTest)
#usedata<-na.omit(indata)
usedata<-na.omit(indata)
#usedata<-indata
# 对每个被试的测量值进行中心化
data_centered <- usedata %>%
  group_by(Number) %>%
  mutate(center_RIS = RIS - mean(RIS)) %>%
  mutate(center_stress = stress - mean(stress)) %>%
  mutate(center_depression = depression - mean(depression)) %>%
  ungroup()
library(psych)
library(tidyr)
library(tidyverse)
count_data <- data_centered %>%
  group_by(Number) %>%
  summarise(count = n())

library(multilevel)
data_centered<- as.data.frame(data_centered)
mult.icc(data_centered[,c("RIS","stress","depression","SA","SM")],grpid=data_centered$Number)
#计算个体内和个体间相关
library(misty)
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

#usedata<-indata
#model <- lmer(Social_media ~ times+MPAI + (1 | Number), data = sleep_try)
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



usedata$predicted_SA <- predict(modelK)
ggplot(usedata,aes(x = RIS, y = SA, col = Number)) +
  geom_point() +
  geom_line(aes(y =predicted_SA, col = Number), size = 2)+
  guides(color = 'none')



library(ggplot2)
# 创建一个新的数据框用于存储预测值
usedata$predicted_SA <- predict(modelK)
# 为了绘制每个被试的线性回归线，我们需要首先计算回归系数
# 对于每个被试计算线性模型的拟合
fit_per_subject <- function(data) {
  lm_fit <- lm(SA ~ RIS, data = data)
  data.frame(intercept = coefficients(lm_fit)[1], slope = coefficients(lm_fit)[2])
}
# 对每个被试应用函数
library(dplyr)
subject_models <- usedata %>%
  group_by(Number) %>%
  do(fit_per_subject(.))
# 合并回归系数到原始数据中
usedata <- usedata %>%
  left_join(subject_models, by = "Number")
# 绘图 SA 与 RIS 的关系
ggplot(usedata, aes(x = RIS, y = SA)) +
  geom_point(alpha = 0.5) +  # 散点
  geom_abline(aes(intercept = intercept, slope = slope), color = "blue", alpha = 0.5) +  # 每个被试的回归线
  labs(title = "SA vs RIS with Subject-Specific Lines", x = "RIS", y = "SA") +
  theme_minimal()
# 绘图 SA 与 stress 的关系
ggplot(usedata, aes(x = stress, y = SA)) +
  geom_point(alpha = 0.5) +  # 散点
  geom_abline(aes(intercept = intercept, slope = slope), color = "blue", alpha = 0.5) +  # 每个被试的回归线
  labs(title = "SA vs Stress with Subject-Specific Lines", x = "Stress", y = "SA") +
  theme_minimal()
# 绘图 SA 与 depression 的关系
ggplot(usedata, aes(x = depression, y = SA)) +
  geom_point(alpha = 0.5) +  # 散点
  geom_abline(aes(intercept = intercept, slope = slope), color = "blue", alpha = 0.5) +  # 每个被试的回归线
  labs(title = "SA vs Depression with Subject-Specific Lines", x = "Depression", y = "SA") +
  theme_minimal()





# 创建一个新的数据框用于存储预测值
usedata$predicted_SA <- predict(modelK)
# 定义一个函数来为每个被试计算线性模型
fit_per_subject <- function(data) {
  lm_fit <- lm(SA ~ RIS, data = data)
  data.frame(intercept = coefficients(lm_fit)[1], slope = coefficients(lm_fit)[2])
}
# 对每个被试应用函数
library(dplyr)
subject_models <- usedata %>%
  group_by(Number) %>%
  do(fit_per_subject(.))
# 合并回归系数到原始数据中
usedata <- usedata %>%
  left_join(subject_models, by = "Number")
# 计算整体模型的拟合
overall_model <- lm(SA ~ RIS, data = usedata)
overall_intercept <- coefficients(overall_model)[1]
overall_slope <- coefficients(overall_model)[2]
# 绘图 SA 与 RIS 的关系
library(ggplot2)
ggplot(usedata, aes(x = RIS, y = SA)) +
  geom_point(alpha = 0.5) +  # 散点
  geom_abline(aes(intercept = intercept, slope = slope), color = "blue", alpha = 0.5) +  # 每个被试的回归线
  geom_abline(intercept = overall_intercept, slope = overall_slope, color = "red", size = 1) +  # 组水平的拟合线
  labs(title = "SA vs RIS with Subject-Specific Lines and Group Level Fit", x = "RIS", y = "SA") +
  theme_minimal() 
ggplot(usedata, aes(x = stress, y = SA)) +
  geom_point(alpha = 0.5) +  # 散点
  geom_abline(aes(intercept = intercept, slope = slope), color = "blue", alpha = 0.5) +  # 每个被试的回归线
  geom_abline(intercept = overall_intercept, slope = overall_slope, color = "red", size = 1) +  # 组水平的拟合线
  labs(title = "SA vs stress with Subject-Specific Lines and Group Level Fit", x = "Stress", y = "SA") +
  theme_minimal() 
ggplot(usedata, aes(x = depression, y = SA)) +
  geom_point(alpha = 0.5) +  # 散点
  geom_abline(aes(intercept = intercept, slope = slope), color = "blue", alpha = 0.5) +  # 每个被试的回归线
  geom_abline(intercept = overall_intercept, slope = overall_slope, color = "red", size = 1) +  # 组水平的拟合线
  labs(title = "SA vs depression with Subject-Specific Lines and Group Level Fit", x = "Depression", y = "SA") +
  theme_minimal() 



#######
# 首先确保数据按参与者和日期排序
data_centered1 <- data_centered[order(data_centered$Number, data_centered$day), ]

# 创建滞后变量
library(dplyr)
data_centered1 <- data_centered1 %>%
  group_by(Number) %>%  # 按参与者分组
  mutate(
    lag1_center_RIS = lag(center_RIS, 1),
    lag1_center_stress = lag(center_stress, 1),
    lag1_center_depression = lag(center_depression, 1),
  ) %>%
  ungroup()


# 现在使用滞后变量构建模型
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


