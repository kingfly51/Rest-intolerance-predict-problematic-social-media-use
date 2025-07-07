library(readxl)
combine_study2 <- read_excel("study2/study2_usedata.xlsx")
combine_study2$RIS<-as.numeric(combine_study2$RIS)
combine_study2$per_use<-as.numeric(combine_study2$per_use)
combine_study2<-combine_study2[!is.na(combine_study2$Education),]

#####2.1 Correlation among variables #####
library(psych)
corr_result <- corr.test(combine_study2[, c("RIS", "PSSS", "PHQ","ssmu","per_use","num_start")])
print(corr_result$r)
#               RIS         PSSS         PHQ         ssmu    per_use  num_start
#RIS       1.00000000  0.464306961  0.35162411  0.081256622 0.09959721 0.08068962
#PSSS      0.46430696  1.000000000  0.52297808 -0.007393047 0.06219705 0.05915220
#PHQ       0.35162411  0.522978081  1.00000000 -0.029864682 0.01434250 0.02006643
#ssmu      0.08125662 -0.007393047 -0.02986468  1.000000000 0.09809314 0.09326699
#per_use   0.09959721  0.062197052  0.01434250  0.098093135 1.00000000 0.53227719
#num_start 0.08068962  0.059152195  0.02006643  0.093266988 0.53227719 1.00000000

print(corr_result$p)
#                RIS         PSSS          PHQ       ssmu      per_use      num_start
#RIS       0.000000e+00 3.026403e-32 8.564252e-18 0.37462866 1.622033e-01 3.746287e-01
#PSSS      2.328002e-33 0.000000e+00 3.290478e-42 1.00000000 7.702624e-01 7.702624e-01
#PHQ       7.136877e-19 2.350341e-43 0.000000e+00 1.00000000 1.000000e+00 1.000000e+00
#ssmu      4.682858e-02 8.567087e-01 4.656601e-01 0.00000000 1.632544e-01 2.019420e-01
#per_use   1.474576e-02 1.283771e-01 7.261060e-01 0.01632544 0.000000e+00 5.979786e-44
#num_start 4.838964e-02 1.481881e-01 6.240357e-01 0.02243800 3.986524e-45 0.000000e+00

###plot
# import package
library(ggplot2)
library(ggsci)  

g1<-ggplot(combine_study2, aes(x = RIS, y = ssmu)) +
  geom_point(aes(color = "Data Points"), size = 3, alpha = 0.3) +  # 散点图
  geom_smooth(method = "lm", aes(color = "Linear Fit"), se = TRUE, linewidth = 2) +  # 直线拟合
  scale_color_npg() +  # 使用 Nature 配色
  labs(
    #title = "Scatter Plot with Linear Fit",
    x = "Rest Intolerance",
    y = "Subjective social media use",
    color = "Legend"  # 图例标题
  ) +
  theme_minimal() +  # 使用简洁主题
  theme(
    legend.position = "none",  # 移除图例
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # 标题样式
    axis.title = element_text(size = 10, face = "bold"),  # 轴标题样式
    axis.text = element_text(size = 10, face = "bold"),  # 轴刻度标签样式
    legend.title = element_text(size = 8, face = "bold"),  # 图例标题样式
    legend.text = element_text(size = 10)  # 图例文本样式
  )

g2<-ggplot(combine_study2, aes(x = RIS, y = per_use)) +
  geom_point(aes(color = "Data Points"), size = 3, alpha = 0.3) +  # 散点图
  geom_smooth(method = "lm", aes(color = "Linear Fit"), se = TRUE, linewidth = 2) +  # 直线拟合
  scale_color_npg() +  # 使用 Nature 配色
  labs(
    #title = "Scatter Plot with Linear Fit",
    x = "Rest Intolerance",
    y = "Objective social media use",
    color = "Legend"  # 图例标题
  ) +
  theme_minimal() +  # 使用简洁主题
  theme(
    legend.position = "none",  # 移除图例
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # 标题样式
    axis.title = element_text(size = 10, face = "bold"),  # 轴标题样式
    axis.text = element_text(size = 10, face = "bold"),  # 轴刻度标签样式
    legend.title = element_text(size = 8, face = "bold"),  # 图例标题样式
    legend.text = element_text(size = 10)  # 图例文本样式
  )

g3<-ggplot(combine_study2, aes(x = RIS, y = num_start)) +
  geom_point(aes(color = "Data Points"), size = 3, alpha = 0.3) +  # 散点图
  geom_smooth(method = "lm", aes(color = "Linear Fit"), se = TRUE, linewidth = 2) +  # 直线拟合
  scale_color_npg() +  # 使用 Nature 配色
  labs(
    #title = "Scatter Plot with Linear Fit",
    x = "Rest Intolerance",
    y = "Objective social media activation",
    color = "Legend"  # 图例标题
  ) +
  theme_minimal() +  # 使用简洁主题
  theme(
    legend.position = "none",  # 移除图例
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # 标题样式
    axis.title = element_text(size = 10, face = "bold"),  # 轴标题样式
    axis.text = element_text(size = 10, face = "bold"),  # 轴刻度标签样式
    legend.title = element_text(size = 8, face = "bold"),  # 图例标题样式
    legend.text = element_text(size = 10)  # 图例文本样式
  )

combined_plot <- g1 + g2 + g3
print(combined_plot)


##### 2.2 Social media use differences based on subgroups of high and low rest intolerance, stress, and depression scores #####
### Import packages
source("geom_flat_violin.R")
library(cowplot) 
library(readr)
library(tidyverse)
library(ggplot2)
library(gglayer)
library(ggpubr)
library(patchwork)

###Build RIS group
low_level <- quantile(combine_study2$RIS, probs=c(.27))  # 2.42857
high_level <- quantile(combine_study2$RIS, probs=c(.73))  # 3.57
low_group <- combine_study2[combine_study2$RIS<=low_level,]
high_group <- combine_study2[combine_study2$RIS>=high_level,]
low_group$RIS[low_group$RIS<=max(low_group$RIS)]<-"Low"
high_group$RIS[high_group$RIS>=min(high_group$RIS)]<-"High"
ris_group<-rbind(low_group,high_group)
ris_group$RIS<-as.factor(ris_group$RIS)



###age, gender, education t-test/chisq-test
t.test(low_group$Age, high_group$Age)
#t = -1.0792, df = 189.37, p-value = 0.2819
#95 percent confidence interval:
#  -3.3881135  0.9918632
#sample estimates:
#  mean of x mean of y 
#20.29101  21.48913 
chisq.test(table(ris_group$RIS, ris_group$Gender))
#X-squared = 4.5829, df = 1, p-value = 0.03229
table(ris_group$RIS, ris_group$Gender)
#      1   2
#High  42 142
#Low   63 126
t.test(low_group$Education, high_group$Education)
#t = 0.4776, df = 366.02, p-value = 0.6332
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.07933408  0.13023125
#sample estimates:
#  mean of x mean of y 
#2.063492  2.038043 

###Test the inter group differences of variables in high and low RIS
t.test(low_group$ssmu, high_group$ssmu)
#t = -2.1634, df = 361.24, p-value = 0.03117
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -9.5923880 -0.4571864
#sample estimates:
#  mean of x mean of y 
#43.31217  48.33696 
t.test(low_group$per_use, high_group$per_use)
#t = -2.7329, df = 368.27, p-value = 0.006582
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.09592572 -0.01564495
#sample estimates:
#  mean of x mean of y 
#0.1760143 0.2317996 

t.test(low_group$num_start, high_group$num_start)
#t = -2.8464, df = 351.23, p-value = 0.004682
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -24.768660  -4.526601
#sample estimates:
#  mean of x mean of y 
#41.23280  55.88043 


###Inverse Probability Weighting, IPW
library(WeightIt)
# 进行逆概率加权
weighted_data <- weightit(
  RIS ~ Age + Gender + Education,  # 模型公式
  data = ris_group,                     # 数据集
  method = "ps",                   # 使用倾向性评分（Propensity Score）
  estimand = "ATE"                 # 平均处理效应（Average Treatment Effect）
)
# 将权重添加到数据中
ris_group$weights <- weighted_data$weights
library(survey)
# 创建加权数据集
design <- svydesign(ids = ~1, data = ris_group, weights = ~weights)
# age
age_model <- svyglm(Age ~ RIS, design = design)
summary(age_model)
# gender
gender_table <- svytable(~ Gender + RIS, design = design)
print(gender_table)
#education
edu_model<- svyglm(Education ~ RIS, design = design)
print(edu_model)
# 绘制平衡图
library(cobalt)
# 绘制 SMD 图
love.plot(weighted_data, threshold = 0.1) +
  labs(
    title = "Standardized Mean Differences After Weighting",
    x = "Standardized Mean Difference (SMD)",
    y = "Variable"
  )

svglm1 <- svyglm(ssmu ~ RIS, design = design)
summary(svglm1)
#RISLow        -5.097      2.352  -2.167   0.0309 * 
svglm2 <- svyglm(per_use ~ RIS, design = design)
summary(svglm2)
#RISLow      -0.05302    0.02070  -2.561   0.0108 * 
svglm3 <- svyglm(num_start ~ RIS, design = design)
summary(svglm3)
#RISLow       -15.227      5.194  -2.931  0.00358 ** 


###To examine the partial correlation between RIS and SMU in the entire population after controlling for sociodemographic factors
library(ppcor)
library(ggsci)
partial_corr_1 <- pcor.test(combine_study2$RIS, combine_study2$ssmu, combine_study2[, c("Age","Gender","Education")])
print(partial_corr_1)
#estimate    p.value statistic   n gp  Method
#1 0.06974131 0.08892427  1.703892 599  3 pearson
partial_corr_2 <- pcor.test(combine_study2$RIS, combine_study2$per_use, combine_study2[, c("Age","Gender","Education")])
print(partial_corr_2)
#estimate    p.value statistic   n gp  Method
#1 0.08564244 0.03659528  2.094985 599  3 pearson
partial_corr_3 <- pcor.test(combine_study2$RIS, combine_study2$num_start, combine_study2[, c("Age","Gender","Education")])
print(partial_corr_3)
#estimate    p.value statistic   n gp  Method
#1 0.08033371 0.04996687  1.964251 599  3 pearson


#ris_ssmu   Residual after extracting control covariates
y<-lm(ssmu~Age+Gender+Education,data = combine_study2)
data_y<-data.frame(y=y$residuals)
combine_study3<-cbind(combine_study2,data_y)
# Generate partial correlation and significance
data_for_pcor <- combine_study2[, c("ssmu", "RIS", "Age", "Gender", "Education")]
pcor_result <- pcor(data_for_pcor, method = "pearson")
#ssmu+ris
partial_cor <- pcor_result$estimate["ssmu", "RIS"]
partial_p_value <- pcor_result$p.value["ssmu", "RIS"]
cor_text <- paste0("Partial r = ", round(partial_cor, 2), "\np = ", round(partial_p_value, 3))
#plot
g1p<-ggplot(combine_study3, aes(x = RIS, y = y)) +
  geom_point(aes(color = "Data Points"), size = 3, alpha = 0.3) +  # 散点图
  geom_smooth(method = "lm", aes(color = "Linear Fit"), se = TRUE, linewidth = 2) +  # 直线拟合
  scale_color_npg() +  # 使用 Nature 配色
  labs(
    #title = "Scatter Plot with Linear Fit",
    x = "Rest Intolerance",
    y = "Subjective social media use",
    color = "Legend"  # 图例标题
  ) +
  theme_minimal() +  # 使用简洁主题
  theme(
    legend.position = "none",  # 移除图例
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # 标题样式
    axis.title = element_text(size = 10, face = "bold"),  # 轴标题样式
    axis.text = element_text(size = 10, face = "bold"),  # 轴刻度标签样式
    legend.title = element_text(size = 8, face = "bold"),  # 图例标题样式
    legend.text = element_text(size = 10)  # 图例文本样式
  )+
  annotate("text", x = max(combine_study3$RIS) * 0.6, y = max(combine_study3$y) * 0.9, 
           label = cor_text, size = 5, color = "black", fontface = "bold")  # 添加偏相关注释


#ris_per_use   Residual after extracting control covariates
y<-lm(per_use~Age+Gender+Education,data = combine_study2)
data_y<-data.frame(y=y$residuals)
combine_study3<-cbind(combine_study2,data_y)
# Generate partial correlation and significance
data_for_pcor <- combine_study2[, c("per_use", "RIS", "Age", "Gender", "Education")]
pcor_result <- pcor(data_for_pcor, method = "pearson")
#per_use+ris
partial_cor <- pcor_result$estimate["per_use", "RIS"]
partial_p_value <- pcor_result$p.value["per_use", "RIS"]
cor_text <- paste0("Partial r = ", round(partial_cor, 2), "\np = ", round(partial_p_value, 3))

g2p<-ggplot(combine_study3, aes(x = RIS, y = y)) +
  geom_point(aes(color = "Data Points"), size = 3, alpha = 0.3) +  # 散点图
  geom_smooth(method = "lm", aes(color = "Linear Fit"), se = TRUE, linewidth = 2) +  # 直线拟合
  scale_color_npg() +  # 使用 Nature 配色
  labs(
    #title = "Scatter Plot with Linear Fit",
    x = "Rest Intolerance",
    y = "Objective social media use",
    color = "Legend"  # 图例标题
  ) +
  theme_minimal() +  # 使用简洁主题
  theme(
    legend.position = "none",  # 移除图例
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # 标题样式
    axis.title = element_text(size = 10, face = "bold"),  # 轴标题样式
    axis.text = element_text(size = 10, face = "bold"),  # 轴刻度标签样式
    legend.title = element_text(size = 8, face = "bold"),  # 图例标题样式
    legend.text = element_text(size = 10)  # 图例文本样式
  )+
  annotate("text", x = max(combine_study3$RIS) * 0.6, y = max(combine_study3$y) * 0.9, 
           label = cor_text, size = 5, color = "black", fontface = "bold")  # 添加偏相关注释

#ris~ num_start   Residual after extracting control covariates
y<-lm(num_start~Age+Gender+Education,data = combine_study2)
data_y<-data.frame(y=y$residuals)
combine_study3<-cbind(combine_study2,data_y)
# Generate partial correlation and significance
data_for_pcor <- combine_study2[, c("num_start", "RIS", "Age", "Gender", "Education")]
pcor_result <- pcor(data_for_pcor, method = "pearson")
#num_start+ris
partial_cor <- pcor_result$estimate["num_start", "RIS"]
partial_p_value <- pcor_result$p.value["num_start", "RIS"]
cor_text <- paste0("Partial r = ", round(partial_cor, 2), "\np = ", round(partial_p_value, 3))

g3p<-ggplot(combine_study3, aes(x = RIS, y = y)) +
  geom_point(aes(color = "Data Points"), size = 3, alpha = 0.3) +  # 散点图
  geom_smooth(method = "lm", aes(color = "Linear Fit"), se = TRUE, linewidth = 2) +  # 直线拟合
  scale_color_npg() +  # 使用 Nature 配色
  labs(
    #title = "Scatter Plot with Linear Fit",
    x = "Rest Intolerance",
    y = "Objective social media activation",
    color = "Legend"  # 图例标题
  ) +
  theme_minimal() +  # 使用简洁主题
  theme(
    legend.position = "none",  # 移除图例
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # 标题样式
    axis.title = element_text(size = 10, face = "bold"),  # 轴标题样式
    axis.text = element_text(size = 10, face = "bold"),  # 轴刻度标签样式
    legend.title = element_text(size = 8, face = "bold"),  # 图例标题样式
    legend.text = element_text(size = 10)  # 图例文本样式
  )+
  annotate("text", x = max(combine_study3$RIS) * 0.6, y = max(combine_study3$y) * 0.9, 
           label = cor_text, size = 5, color = "black", fontface = "bold")  # 添加偏相关注释


combined_gp123 <- g1p + g2p + g3p
print(combined_gp123)


### Cloud and Rain Map of RIS
##Rest Intolerance ~ Subjective social media use
p1<-ggplot(ris_group, aes(x = RIS, y = ssmu, fill = RIS)) +
  geom_flat_violin(aes(fill = RIS),position = position_nudge(x = .1, y = 0),
                   adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(RIS)-.15, y = ssmu, colour = RIS),position =
               position_jitter(width = .05), size = 1, shape = 20)+
  geom_boxplot(aes(x = RIS, y = ssmu, fill = RIS),outlier.shape = NA, alpha
               = .5, width = .1, colour = "black")+
  scale_colour_brewer(palette = "Dark2")+theme_cowplot()+
  scale_fill_brewer(palette = "Dark2")+
  labs(x = "Rest Intolerance", y = "Subjective social media use")+theme_cowplot()+
  theme(
    legend.position = "none",  # 移除图例
    text = element_text(size = 10,face = "bold"),  # 全局文本字号设置为 10
    axis.title = element_text(size = 10,face = "bold"),  # 轴标题字号
    axis.text = element_text(size = 8,face = "bold"),  # 轴刻度标签字号
    strip.text = element_text(size = 10,face = "bold")  # 分面文本字号（如果有分面）
  ) + #+guides(fill = guide_legend(title = "Relationship Depth"),color = guide_legend(title = "Relationship Depth"))
  stat_compare_means(aes(label = format(..p.format.., digits = 3)), method = "t.test", 
                     comparisons = list(c("High", "Low")),size = 3.5)  


##Rest Intolerance ~ Objective social media use
p2<-ggplot(ris_group, aes(x = RIS, y = per_use, fill = RIS)) +
  geom_flat_violin(aes(fill = RIS),position = position_nudge(x = .1, y = 0),
                   adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(RIS)-.15, y = per_use, colour = RIS),position =
               position_jitter(width = .05), size = 1, shape = 20)+
  geom_boxplot(aes(x = RIS, y = per_use, fill = RIS),outlier.shape = NA, alpha
               = .5, width = .1, colour = "black")+
  scale_colour_brewer(palette = "Dark2")+theme_cowplot()+
  scale_fill_brewer(palette = "Dark2")+
  labs(x = "Rest Intolerance", y = "Objective social media use")+theme_cowplot()+
  theme(
    legend.position = "none",  # 移除图例
    text = element_text(size = 10,face = "bold"),  # 全局文本字号设置为 10
    axis.title = element_text(size = 10,face = "bold"),  # 轴标题字号
    axis.text = element_text(size = 8,face = "bold"),  # 轴刻度标签字号
    strip.text = element_text(size = 10,face = "bold")  # 分面文本字号（如果有分面）
  )+ #+guides(fill = guide_legend(title = "Relationship Depth"),color = guide_legend(title = "Relationship Depth"))
  stat_compare_means(aes(label = format(..p.format.., digits = 3)), method = "t.test", 
                     comparisons = list(c("High", "Low")),size = 3.5)  

##Rest Intolerance ~ Objective social media activation
p3<-ggplot(ris_group, aes(x = RIS, y = num_start, fill = RIS)) +
  geom_flat_violin(aes(fill = RIS),position = position_nudge(x = .1, y = 0),
                   adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(RIS)-.15, y = num_start, colour = RIS),position =
               position_jitter(width = .05), size = 1, shape = 20)+
  geom_boxplot(aes(x = RIS, y = num_start, fill = RIS),outlier.shape = NA, alpha
               = .5, width = .1, colour = "black")+
  scale_colour_brewer(palette = "Dark2")+theme_cowplot()+
  scale_fill_brewer(palette = "Dark2")+
  labs(x = "Rest Intolerance", y = "Objective social media activation")+theme_cowplot()+
  theme(
    legend.position = "none",  # 移除图例
    text = element_text(size = 10,face = "bold"),  # 全局文本字号设置为 10
    axis.title = element_text(size = 10,face = "bold"),  # 轴标题字号
    axis.text = element_text(size = 8,face = "bold"),  # 轴刻度标签字号
    strip.text = element_text(size = 10,face = "bold")  # 分面文本字号（如果有分面）
  )+ #+guides(fill = guide_legend(title = "Relationship Depth"),color = guide_legend(title = "Relationship Depth"))
  stat_compare_means(aes(label = format(..p.format.., digits = 3)), method = "t.test", 
                     comparisons = list(c("High", "Low")),size = 3.5)  



#Build Stress group
low_level <- quantile(combine_study2$PSSS, probs=c(.27))  # 4
high_level <- quantile(combine_study2$PSSS, probs=c(.73))  # 7
low_group <- combine_study2[combine_study2$PSSS<=low_level,]
high_group <- combine_study2[combine_study2$PSSS>=high_level,]
low_group$PSSS[low_group$PSSS<=max(low_group$PSSS)]<-"Low"
high_group$PSSS[high_group$PSSS>=min(high_group$PSSS)]<-"High"
ris_group<-rbind(low_group,high_group)
ris_group$PSSS<-as.factor(ris_group$PSSS)

# Cloud and Rain Map of PSSS
##Stress ~ Subjective social media use
p4<-ggplot(ris_group, aes(x = PSSS, y = ssmu, fill = PSSS)) +
  geom_flat_violin(aes(fill = PSSS),position = position_nudge(x = .1, y = 0),
                   adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(PSSS)-.15, y = ssmu, colour = PSSS),position =
               position_jitter(width = .05), size = 1, shape = 20)+
  geom_boxplot(aes(x = PSSS, y = ssmu, fill = PSSS),outlier.shape = NA, alpha
               = .5, width = .1, colour = "black")+
  scale_colour_brewer(palette = "Dark2")+theme_cowplot()+
  scale_fill_brewer(palette = "Dark2")+
  labs(x = "Stress", y = "Subjective social media use")+theme_cowplot()+
  theme(
    legend.position = "none",  # 移除图例
    text = element_text(size = 10,face = "bold"),  # 全局文本字号设置为 10
    axis.title = element_text(size = 10,face = "bold"),  # 轴标题字号
    axis.text = element_text(size = 8,face = "bold"),  # 轴刻度标签字号
    strip.text = element_text(size = 10,face = "bold")  # 分面文本字号（如果有分面）
  )+ #+guides(fill = guide_legend(title = "Relationship Depth"),color = guide_legend(title = "Relationship Depth"))
  stat_compare_means(aes(label = ..p.format..), method = "t.test", 
                     comparisons = list(c("High", "Low")),size = 3.5)  

##Stress ~ Objective social media use
p5<-ggplot(ris_group, aes(x = PSSS, y = per_use, fill = PSSS)) +
  geom_flat_violin(aes(fill = PSSS),position = position_nudge(x = .1, y = 0),
                   adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(PSSS)-.15, y = per_use, colour = PSSS),position =
               position_jitter(width = .05), size = 1, shape = 20)+
  geom_boxplot(aes(x = PSSS, y = per_use, fill = PSSS),outlier.shape = NA, alpha
               = .5, width = .1, colour = "black")+
  scale_colour_brewer(palette = "Dark2")+theme_cowplot()+
  scale_fill_brewer(palette = "Dark2")+
  labs(x = "Stress", y = "Objective social media use")+theme_cowplot()+
  theme(
    legend.position = "none",  # 移除图例
    text = element_text(size = 10,face = "bold"),  # 全局文本字号设置为 10
    axis.title = element_text(size = 10,face = "bold"),  # 轴标题字号
    axis.text = element_text(size = 8,face = "bold"),  # 轴刻度标签字号
    strip.text = element_text(size = 10,face = "bold")  # 分面文本字号（如果有分面）
  )+ #+guides(fill = guide_legend(title = "Relationship Depth"),color = guide_legend(title = "Relationship Depth"))
  stat_compare_means(aes(label = ..p.format..), method = "t.test", 
                     comparisons = list(c("High", "Low")),size = 3.5)  

##Stress ~ Objective social media activation
p6<-ggplot(ris_group, aes(x = PSSS, y = num_start, fill = PSSS)) +
  geom_flat_violin(aes(fill = PSSS),position = position_nudge(x = .1, y = 0),
                   adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(PSSS)-.15, y = num_start, colour = PSSS),position =
               position_jitter(width = .05), size = 1, shape = 20)+
  geom_boxplot(aes(x = PSSS, y = num_start, fill = PSSS),outlier.shape = NA, alpha
               = .5, width = .1, colour = "black")+
  scale_colour_brewer(palette = "Dark2")+theme_cowplot()+
  scale_fill_brewer(palette = "Dark2")+
  labs(x = "Stress", y = "Objective social media activation")+theme_cowplot()+
  theme(
    legend.position = "none",  # 移除图例
    text = element_text(size = 10,face = "bold"),  # 全局文本字号设置为 10
    axis.title = element_text(size = 10,face = "bold"),  # 轴标题字号
    axis.text = element_text(size = 8,face = "bold"),  # 轴刻度标签字号
    strip.text = element_text(size = 10,face = "bold")  # 分面文本字号（如果有分面）
  )+ #+guides(fill = guide_legend(title = "Relationship Depth"),color = guide_legend(title = "Relationship Depth"))
  stat_compare_means(aes(label = ..p.format..), method = "t.test", 
                     comparisons = list(c("High", "Low")),size = 3.5) 


#Build Depression group
low_level <- quantile(combine_study2$PHQ, probs=c(.27))  # 1
high_level <- quantile(combine_study2$PHQ, probs=c(.73))  # 3
low_group <- combine_study2[combine_study2$PHQ<=low_level,]
high_group <- combine_study2[combine_study2$PHQ>=high_level,]
low_group$PHQ[low_group$PHQ<=max(low_group$PHQ)]<-"Low"
high_group$PHQ[high_group$PHQ>=min(high_group$PHQ)]<-"High"
ris_group<-rbind(low_group,high_group)
ris_group$PHQ<-as.factor(ris_group$PHQ)

# Cloud and Rain Map of PHQ
##Depression ~ Subjective social media use
p7<-ggplot(ris_group, aes(x = PHQ, y = ssmu, fill = PHQ)) +
  geom_flat_violin(aes(fill = PHQ),position = position_nudge(x = .1, y = 0),
                   adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(PHQ)-.15, y = ssmu, colour = PHQ),position =
               position_jitter(width = .05), size = 1, shape = 20)+
  geom_boxplot(aes(x = PHQ, y = ssmu, fill = PHQ),outlier.shape = NA, alpha
               = .5, width = .1, colour = "black")+
  scale_colour_brewer(palette = "Dark2")+theme_cowplot()+
  scale_fill_brewer(palette = "Dark2")+
  labs(x = "Depression", y = "Subjective social media use")+theme_cowplot()+
  theme(
    legend.position = "none",  # 移除图例
    text = element_text(size = 10,face = "bold"),  # 全局文本字号设置为 10
    axis.title = element_text(size = 10,face = "bold"),  # 轴标题字号
    axis.text = element_text(size = 8,face = "bold"),  # 轴刻度标签字号
    strip.text = element_text(size = 10,face = "bold")  # 分面文本字号（如果有分面）
  )+ #+guides(fill = guide_legend(title = "Relationship Depth"),color = guide_legend(title = "Relationship Depth"))
  stat_compare_means(aes(label = ..p.format..), method = "t.test", 
                     comparisons = list(c("High", "Low")),size = 3.5)  

##Depression ~ Objective social media use
p8<-ggplot(ris_group, aes(x = PHQ, y = per_use, fill = PHQ)) +
  geom_flat_violin(aes(fill = PHQ),position = position_nudge(x = .1, y = 0),
                   adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(PHQ)-.15, y = per_use, colour = PHQ),position =
               position_jitter(width = .05), size = 1, shape = 20)+
  geom_boxplot(aes(x = PHQ, y = per_use, fill = PHQ),outlier.shape = NA, alpha
               = .5, width = .1, colour = "black")+
  scale_colour_brewer(palette = "Dark2")+theme_cowplot()+
  scale_fill_brewer(palette = "Dark2")+
  labs(x = "Depression", y = "Objective social media use")+theme_cowplot()+
  theme(
    legend.position = "none",  # 移除图例
    text = element_text(size = 10,face = "bold"),  # 全局文本字号设置为 10
    axis.title = element_text(size = 10,face = "bold"),  # 轴标题字号
    axis.text = element_text(size = 8,face = "bold"),  # 轴刻度标签字号
    strip.text = element_text(size = 10,face = "bold")  # 分面文本字号（如果有分面）
  )+ #+guides(fill = guide_legend(title = "Relationship Depth"),color = guide_legend(title = "Relationship Depth"))
  stat_compare_means(aes(label = ..p.format..), method = "t.test", 
                     comparisons = list(c("High", "Low")),size = 3.5)  

##Depression ~ Objective social media activation
p9<-ggplot(ris_group, aes(x = PHQ, y = num_start, fill = PHQ)) +
  geom_flat_violin(aes(fill = PHQ),position = position_nudge(x = .1, y = 0),
                   adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(PHQ)-.15, y = num_start, colour = PHQ),position =
               position_jitter(width = .05), size = 1, shape = 20)+
  geom_boxplot(aes(x = PHQ, y = num_start, fill = PHQ),outlier.shape = NA, alpha
               = .5, width = .1, colour = "black")+
  scale_colour_brewer(palette = "Dark2")+theme_cowplot()+
  scale_fill_brewer(palette = "Dark2")+
  labs(x = "Depression", y = "Objective social media activation")+theme_cowplot()+
  theme(
    legend.position = "none",  # 移除图例
    text = element_text(size = 10,face = "bold"),  # 全局文本字号设置为 10
    axis.title = element_text(size = 10,face = "bold"),  # 轴标题字号
    axis.text = element_text(size = 8,face = "bold"),  # 轴刻度标签字号
    strip.text = element_text(size = 10,face = "bold")  # 分面文本字号（如果有分面）
  )+ #+guides(fill = guide_legend(title = "Relationship Depth"),color = guide_legend(title = "Relationship Depth"))
  stat_compare_means(aes(label = ..p.format..), method = "t.test", 
                     comparisons = list(c("High", "Low")),size = 3.5) 

# 按行排列
combined_plot2 <- (p1 + p2 + p3) / (p4+p5+p6) / (p7+p8+p9)
# 按列排列
#combined_plot <- p1 / p2 / p3

# 显示合并后的图形
print(combined_plot2)


##### 2.3 The relationship between rest intolerance and problematic social media use #####
library(readxl)
bsma_data <- read_excel("study2/社交媒体成瘾_问卷汇总_20250111.xlsx")
bsma_data <-bsma_data[,c("No","BSMAS")]
colnames(bsma_data)[1]<-"no"
com_data<-combine_study2[combine_study2$study==2,]
com_data<-merge(com_data,bsma_data,by="no")

#addict_person<-com_data[com_data$BSMAS>=24,]
addict_person<-combine_study2[combine_study2$num_start>40,]
addict_person<-addict_person[addict_person$social_media_time>14400,]
#addict_person<-combine_study2[combine_study2$social_media_time>14400,]#14400
#addict_person<-com_data[com_data$social_media_time>14400,]

#t.test(low_group$num_start, high_group$num_start)
cor.test(addict_person$RIS, addict_person$num_start)
cor.test(addict_person$RIS, addict_person$per_use)
cor.test(addict_person$RIS, addict_person$ssmu)

hist(addict_person$num_start)
plot(addict_person$RIS, addict_person$num_start)
plot(addict_person$RIS, addict_person$per_use)
plot(combine_study2$RIS, combine_study2$num_start)
plot(combine_study2$RIS, combine_study2$per_use)

add_low_level <- quantile(addict_person$RIS, probs=c(.27))  # 2.42857
add_high_level <- quantile(addict_person$RIS, probs=c(.73))  # 3.57
add_low_level <- quantile(combine_study2$RIS, probs=c(.27))  # 2.42857
add_high_level <- quantile(combine_study2$RIS, probs=c(.73))  # 3.57
add_low_group <- addict_person[addict_person$RIS<=add_low_level,]
add_high_group <- addict_person[addict_person$RIS>=add_high_level,]
#low_group$RIS[low_group$RIS<=max(low_group$RIS)]<-"Low"
#high_group$RIS[high_group$RIS>=min(high_group$RIS)]<-"High"
#ris_group<-rbind(low_group,high_group)
#ris_group$RIS<-as.factor(ris_group$RIS)

# 进行 Mann-Whitney U 检验
result <- wilcox.test(add_low_group$num_start, add_high_group$num_start, exact = FALSE)  # exact = FALSE 用于大样本
print(result)
result <- wilcox.test(add_low_group$per_use, add_high_group$per_use, exact = FALSE)  # exact = FALSE 用于大样本
print(result)
t.test(add_low_group$num_start, add_high_group$num_start)
t.test(add_low_group$per_use, add_high_group$per_use)
t.test(add_low_group$ssmu, add_high_group$ssmu)





t.test(low_group$num_start, high_group$num_start)
cor.test(combine_study2$RIS, combine_study2$num_start)
plot(combine_study2$RIS, combine_study2$num_start)


t.test(low_group$per_use, high_group$per_use)
cor.test(combine_study2$RIS, combine_study2$per_use)
plot(combine_study2$RIS, combine_study2$per_use)



















library(ppcor)    
combine_study3<-combine_study2
combine_study3$Education[combine_study3$Education==1]<-"2"
combine_study3$Education<-as.numeric(combine_study3$Education)

partial_corr_1 <- pcor.test(combine_study3$RIS, combine_study3$num_start, combine_study3[, c("Age", "Gender", "Education")])
print(partial_corr_1)

partial_corr_2 <- pcor.test(combine_study3$RIS, combine_study3$per_use, combine_study3[, c("Age", "Gender", "Education")])
print(partial_corr_2)

combine_study3<-combine_study2
#combine_study3<-combine_study3[combine_study3$ssmu2<=60,]
#combine_study3<-combine_study3[combine_study3$ssmu1<=60,]
combine_study3<-combine_study3[combine_study3$ssmu<=120,]

combine_study3_1<-combine_study3[ combine_study3$study==2 &combine_study3$SMU<24 & combine_study3$SMU>0,]
combine_study3_2<-combine_study3[combine_study3$study==1,]
combine_study3<-rbind(combine_study3_1,combine_study3_2)
#combine_study3<-combine_study3[combine_study3$ssmu>0 ,]
#combine_study3<-combine_study3[combine_study3$RIS!=5 ,]
combine_study3<-combine_study3[combine_study3$no!="H02",]#y
combine_study3<-combine_study3[combine_study3$no!="H08",]#y
combine_study3<-combine_study3[combine_study3$no!="K81",]#y
combine_study3<-combine_study3[combine_study3$no!="P50",]#y
combine_study3<-combine_study3[combine_study3$no!="B06",]#y
combine_study3<-combine_study3[combine_study3$no!="J72",]#y
combine_study3<-combine_study3[combine_study3$no!="E42",]#y
combine_study3<-combine_study3[combine_study3$no!="D7",]#m
#combine_study3<-combine_study3[combine_study3$no!="E01",]#m

write.csv(combine_study3,"study2_usedata.csv")


partial_corr_3 <- pcor.test(combine_study3$RIS, combine_study3$ssmu, combine_study3[, c("Age","Gender","Education")])
print(partial_corr_3)
cor.test(combine_study3$RIS, combine_study3$ssmu)
plot(combine_study3$RIS, combine_study3$ssmu)


combine_study2<-combine_study3
## 给被试分高低两组
low_level <- quantile(combine_study2$RIS, probs=c(.27))  # 24
high_level <- quantile(combine_study2$RIS, probs=c(.73))  #24
low_group <- combine_study2[combine_study2$RIS<low_level,]
high_group <- combine_study2[combine_study2$RIS>high_level,]

t.test(low_group$num_start, high_group$num_start)
cor.test(combine_study2$RIS, combine_study2$num_start)
plot(combine_study2$RIS, combine_study2$num_start)


t.test(low_group$per_use, high_group$per_use)
cor.test(combine_study2$RIS, combine_study2$per_use)
plot(combine_study2$RIS, combine_study2$per_use)

cor.test(combine_study2$ssmu, combine_study2$per_use)
cor.test(combine_study2$ssmu, combine_study2$num_start)
plot(combine_study2$ssmu, combine_study2$num_start)
cor.test(combine_study2$RIS, combine_study2$SMU)
#cor.test(combine_study2$per_use, combine_study2$SMU)

cs3<-combine_study2[combine_study2$SMU<21 & combine_study2$SMU>0,]
cs3<-cs3[cs3$no!="NY280",]
cs3<-cs3[cs3$no!="NY70",]
cs3<-cs3[!is.na(cs3$SMU),]
partial_corr_4 <- pcor.test(cs3$RIS, cs3$SMU, cs3[, c("Age", "Gender", "Education")])
print(partial_corr_4)
cor.test(cs3$RIS, cs3$SMU)
plot(cs3$RIS, cs3$SMU)
cor.test(cs3$ssmu, cs3$per_use)

partial_corr_3 <- pcor.test(combine_study3$RIS, combine_study3$ssmu, combine_study3[, c("Age", "Gender", "Education")])
print(partial_corr_3)


cs3<-combine_study2[combine_study2$per_use>0.2,]




cor.test(all_data2$PSS, all_data2$per_use)
cor.test(all_data2$PSQ, all_data2$per_use)
cor.test(all_data$PHQ_mean, all_data$PSS_mean)
cor.test(all_data$PSS_mean, all_data$RIS_mean)
cor.test(all_data$PHQ_mean, all_data$RIS_mean)
cor.test(all_data$RIS_mean, all_data$num_start)
cor.test(all_data$PHQ_mean, all_data$num_start)
cor.test(all_data$PSS_mean, all_data$num_start)
cor.test(all_data$per_use, all_data$num_start)