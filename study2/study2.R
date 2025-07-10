###### 1. Install and Load Required Packages ######
source("geom_flat_violin.R")
library(readxl)
library(psych)
library(cowplot) 
library(readr)
library(tidyverse)
library(ggplot2)
library(ggsci)  
library(gglayer)
library(ggpubr)
library(patchwork)
library(WeightIt)
library(survey)
library(cobalt)
library(ppcor)


###### 2 Correlation among variables ######
combine_study2 <- read_excel("20241118/Nature Human Behaviour/final/code/study2/data/study2_usedata.xlsx")
combine_study2$RIS<-as.numeric(combine_study2$RIS)
combine_study2$per_use<-as.numeric(combine_study2$per_use)
combine_study2<-combine_study2[!is.na(combine_study2$Education),]

corr_result <- corr.test(combine_study2[, c("RIS", "PSSS", "PHQ","ssmu","per_use","num_start")])
print(corr_result$r)
print(corr_result$p)
###plot

g1<-ggplot(combine_study2, aes(x = RIS, y = ssmu)) +
  geom_point(aes(color = "Data Points"), size = 3, alpha = 0.3) + 
  geom_smooth(method = "lm", aes(color = "Linear Fit"), se = TRUE, linewidth = 2) + 
  scale_color_npg() + 
  labs(
    #title = "Scatter Plot with Linear Fit",
    x = "Rest Intolerance",
    y = "Subjective social media use",
    color = "Legend"  
  ) +
  theme_minimal() +  
  theme(
    legend.position = "none",  
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
    axis.title = element_text(size = 10, face = "bold"),  
    axis.text = element_text(size = 10, face = "bold"),  
    legend.title = element_text(size = 8, face = "bold"),  
    legend.text = element_text(size = 10)  
  )

g2<-ggplot(combine_study2, aes(x = RIS, y = per_use)) +
  geom_point(aes(color = "Data Points"), size = 3, alpha = 0.3) +  
  geom_smooth(method = "lm", aes(color = "Linear Fit"), se = TRUE, linewidth = 2) + 
  scale_color_npg() + 
  labs(
    #title = "Scatter Plot with Linear Fit",
    x = "Rest Intolerance",
    y = "Objective social media use",
    color = "Legend"  
  ) +
  theme_minimal() +  
  theme(
    legend.position = "none",  
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  
    axis.title = element_text(size = 10, face = "bold"), 
    axis.text = element_text(size = 10, face = "bold"),  
    legend.title = element_text(size = 8, face = "bold"),  
    legend.text = element_text(size = 10) 
  )

g3<-ggplot(combine_study2, aes(x = RIS, y = num_start)) +
  geom_point(aes(color = "Data Points"), size = 3, alpha = 0.3) +  
  geom_smooth(method = "lm", aes(color = "Linear Fit"), se = TRUE, linewidth = 2) + 
  scale_color_npg() +  
  labs(
    #title = "Scatter Plot with Linear Fit",
    x = "Rest Intolerance",
    y = "Objective social media activation",
    color = "Legend"  
  ) +
  theme_minimal() +  
  theme(
    legend.position = "none",  
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  
    axis.title = element_text(size = 10, face = "bold"), 
    axis.text = element_text(size = 10, face = "bold"),  
    legend.title = element_text(size = 8, face = "bold"), 
    legend.text = element_text(size = 10) 
  )

combined_plot <- g1 + g2 + g3
print(combined_plot)



###### 3.Social media use differences based on subgroups of high and low rest intolerance, stress, and depression scores ######
###Build RIS group
low_level <- quantile(combine_study2$RIS, probs=c(.27)) 
high_level <- quantile(combine_study2$RIS, probs=c(.73)) 
low_group <- combine_study2[combine_study2$RIS<=low_level,]
high_group <- combine_study2[combine_study2$RIS>=high_level,]
low_group$RIS[low_group$RIS<=max(low_group$RIS)]<-"Low"
high_group$RIS[high_group$RIS>=min(high_group$RIS)]<-"High"
ris_group<-rbind(low_group,high_group)
ris_group$RIS<-as.factor(ris_group$RIS)

###age, gender, education t-test/chisq-test
t.test(low_group$Age, high_group$Age)
chisq.test(table(ris_group$RIS, ris_group$Gender))
table(ris_group$RIS, ris_group$Gender)
t.test(low_group$Education, high_group$Education)

###Test the inter group differences of variables in high and low RIS
t.test(low_group$ssmu, high_group$ssmu)
t.test(low_group$per_use, high_group$per_use)
t.test(low_group$num_start, high_group$num_start)


###Inverse Probability Weighting, IPW
# Perform inverse probability weighting
weighted_data <- weightit(
  RIS ~ Age + Gender + Education,  
  data = ris_group,                    
  method = "ps",                   # Propensity Score
  estimand = "ATE"                 # Average Treatment Effect
)
# Add weights to the data
ris_group$weights <- weighted_data$weights

# Create a weighted dataset
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
# Draw a balance chart
# Draw SMD diagram
love.plot(weighted_data, threshold = 0.1) +
  labs(
    title = "Standardized Mean Differences After Weighting",
    x = "Standardized Mean Difference (SMD)",
    y = "Variable"
  )

svglm1 <- svyglm(ssmu ~ RIS, design = design)
summary(svglm1)
svglm2 <- svyglm(per_use ~ RIS, design = design)
summary(svglm2)
svglm3 <- svyglm(num_start ~ RIS, design = design)
summary(svglm3)


###To examine the partial correlation between RIS and SMU in the entire population after controlling for sociodemographic factors
partial_corr_1 <- pcor.test(combine_study2$RIS, combine_study2$ssmu, combine_study2[, c("Age","Gender","Education")])
print(partial_corr_1)
partial_corr_2 <- pcor.test(combine_study2$RIS, combine_study2$per_use, combine_study2[, c("Age","Gender","Education")])
print(partial_corr_2)
partial_corr_3 <- pcor.test(combine_study2$RIS, combine_study2$num_start, combine_study2[, c("Age","Gender","Education")])
print(partial_corr_3)


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
  geom_point(aes(color = "Data Points"), size = 3, alpha = 0.3) + 
  geom_smooth(method = "lm", aes(color = "Linear Fit"), se = TRUE, linewidth = 2) + 
  scale_color_npg() +  
  labs(
    #title = "Scatter Plot with Linear Fit",
    x = "Rest Intolerance",
    y = "Subjective social media use",
    color = "Legend"  
  ) +
  theme_minimal() + 
  theme(
    legend.position = "none", 
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  
    axis.title = element_text(size = 10, face = "bold"), 
    axis.text = element_text(size = 10, face = "bold"),  
    legend.title = element_text(size = 8, face = "bold"),  
    legend.text = element_text(size = 10) 
  )+
  annotate("text", x = max(combine_study3$RIS) * 0.6, y = max(combine_study3$y) * 0.9, 
           label = cor_text, size = 5, color = "black", fontface = "bold")  

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
  geom_point(aes(color = "Data Points"), size = 3, alpha = 0.3) +  
  geom_smooth(method = "lm", aes(color = "Linear Fit"), se = TRUE, linewidth = 2) + 
  scale_color_npg() +  
  labs(
    #title = "Scatter Plot with Linear Fit",
    x = "Rest Intolerance",
    y = "Objective social media use",
    color = "Legend" 
  ) +
  theme_minimal() +  
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
    axis.title = element_text(size = 10, face = "bold"), 
    axis.text = element_text(size = 10, face = "bold"), 
    legend.title = element_text(size = 8, face = "bold"),  
    legend.text = element_text(size = 10)  
  )+
  annotate("text", x = max(combine_study3$RIS) * 0.6, y = max(combine_study3$y) * 0.9, 
           label = cor_text, size = 5, color = "black", fontface = "bold")  

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
  geom_point(aes(color = "Data Points"), size = 3, alpha = 0.3) +  
  geom_smooth(method = "lm", aes(color = "Linear Fit"), se = TRUE, linewidth = 2) + 
  scale_color_npg() +  
  labs(
    #title = "Scatter Plot with Linear Fit",
    x = "Rest Intolerance",
    y = "Objective social media activation",
    color = "Legend"
  ) +
  theme_minimal() + 
  theme(
    legend.position = "none", 
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
    axis.title = element_text(size = 10, face = "bold"),  
    axis.text = element_text(size = 10, face = "bold"),  
    legend.title = element_text(size = 8, face = "bold"),  
    legend.text = element_text(size = 10)  
  )+
  annotate("text", x = max(combine_study3$RIS) * 0.6, y = max(combine_study3$y) * 0.9, 
           label = cor_text, size = 5, color = "black", fontface = "bold")  


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
    legend.position = "none",  
    text = element_text(size = 10,face = "bold"), 
    axis.title = element_text(size = 10,face = "bold"),  
    axis.text = element_text(size = 8,face = "bold"),  
    strip.text = element_text(size = 10,face = "bold")  
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
    legend.position = "none",  
    text = element_text(size = 10,face = "bold"),  
    axis.title = element_text(size = 10,face = "bold"),  
    axis.text = element_text(size = 8,face = "bold"), 
    strip.text = element_text(size = 10,face = "bold") 
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
    legend.position = "none", 
    text = element_text(size = 10,face = "bold"),  
    axis.title = element_text(size = 10,face = "bold"),  
    axis.text = element_text(size = 8,face = "bold"),  
    strip.text = element_text(size = 10,face = "bold")  
  )+ #+guides(fill = guide_legend(title = "Relationship Depth"),color = guide_legend(title = "Relationship Depth"))
  stat_compare_means(aes(label = format(..p.format.., digits = 3)), method = "t.test", 
                     comparisons = list(c("High", "Low")),size = 3.5)  



#Build Stress group
low_level <- quantile(combine_study2$PSSS, probs=c(.27)) 
high_level <- quantile(combine_study2$PSSS, probs=c(.73)) 
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
    legend.position = "none",  
    text = element_text(size = 10,face = "bold"),  
    axis.title = element_text(size = 10,face = "bold"), 
    axis.text = element_text(size = 8,face = "bold"),  
    strip.text = element_text(size = 10,face = "bold")  
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
    legend.position = "none",  
    text = element_text(size = 10,face = "bold"), 
    axis.title = element_text(size = 10,face = "bold"),  
    axis.text = element_text(size = 8,face = "bold"),  
    strip.text = element_text(size = 10,face = "bold") 
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
    legend.position = "none",  
    text = element_text(size = 10,face = "bold"),  
    axis.title = element_text(size = 10,face = "bold"), 
    axis.text = element_text(size = 8,face = "bold"), 
    strip.text = element_text(size = 10,face = "bold") 
  )+ #+guides(fill = guide_legend(title = "Relationship Depth"),color = guide_legend(title = "Relationship Depth"))
  stat_compare_means(aes(label = ..p.format..), method = "t.test", 
                     comparisons = list(c("High", "Low")),size = 3.5) 


#Build Depression group
low_level <- quantile(combine_study2$PHQ, probs=c(.27))  
high_level <- quantile(combine_study2$PHQ, probs=c(.73))  
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
    legend.position = "none", 
    text = element_text(size = 10,face = "bold"),  
    axis.title = element_text(size = 10,face = "bold"),  
    axis.text = element_text(size = 8,face = "bold"),  
    strip.text = element_text(size = 10,face = "bold") 
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
    legend.position = "none",  
    text = element_text(size = 10,face = "bold"),  
    axis.title = element_text(size = 10,face = "bold"), 
    axis.text = element_text(size = 8,face = "bold"), 
    strip.text = element_text(size = 10,face = "bold") 
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
    legend.position = "none",  
    text = element_text(size = 10,face = "bold"), 
    axis.title = element_text(size = 10,face = "bold"),  
    axis.text = element_text(size = 8,face = "bold"),  
    strip.text = element_text(size = 10,face = "bold") 
  )+ #+guides(fill = guide_legend(title = "Relationship Depth"),color = guide_legend(title = "Relationship Depth"))
  stat_compare_means(aes(label = ..p.format..), method = "t.test", 
                     comparisons = list(c("High", "Low")),size = 3.5) 

# 按行排列
combined_plot2 <- (p1 + p2 + p3) / (p4+p5+p6) / (p7+p8+p9)
# 按列排列
#combined_plot <- p1 / p2 / p3
# 显示合并后的图形
print(combined_plot2)
