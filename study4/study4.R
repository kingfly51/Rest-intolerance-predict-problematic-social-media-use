library(readxl)
study4_data1 <- read_excel("study4/study4_data1.xlsx")
study4_data2 <- read_excel("study4/study4_data2.xlsx")
study4_data3 <- read_excel("study4/study4_data3.xlsx")
study4 <- merge(study4_data1,study4_data2,by="number")
study4 <- merge(study4,study4_data3,by="number")
study4$age <- as.numeric(study4$age)
#library(writexl)
#write_xlsx(study4,"study4.xlsx")


#study4 <- study4[study4$atten_check==4,]

study4$T1_RIS <- study4$RIS1.x+study4$RIS2.x+study4$RIS3.x+study4$RIS4.x+study4$RIS5.x+
  study4$RIS6.x+study4$RIS7.x+study4$RIS8.x
study4$T2_RIS <- study4$RIS1.y+study4$RIS2.y+study4$RIS3.y+study4$RIS4.y+study4$RIS5.y+
  study4$RIS6.y+study4$RIS7.y+study4$RIS8.y

study4$T1_BSMAS <-study4$BSMAS1.x+study4$BSMAS2.x+study4$BSMAS3.x+
  study4$BSMAS4.x+study4$BSMAS5.x+study4$BSMAS6.x
study4$T2_BSMAS <-study4$BSMAS1.y+study4$BSMAS2.y+study4$BSMAS3.y+
  study4$BSMAS4.y+study4$BSMAS5.y+study4$BSMAS6.y

study4$pss <-study4$PSS1.x+study4$PSS2.x+study4$PSS3.x+study4$PSS4.x
study4$phq <-study4$PHQ1.x+study4$PHQ2.x
#####5.1 Descriptive statistics and correlation analysis#####
desc_stats <- data.frame(
  Variable = c("T1_RIS", "T2_RIS", "T3_RIS", "T1_BSMAS", "T2_BSMAS", "T3_BSMAS"),
  Mean = c(mean(study4$T1_RIS, na.rm = TRUE),
           mean(study4$T2_RIS, na.rm = TRUE),
           mean(study4$T3_RIS, na.rm = TRUE),
           mean(study4$T1_BSMAS, na.rm = TRUE),
           mean(study4$T2_BSMAS, na.rm = TRUE),
           mean(study4$T3_BSMAS, na.rm = TRUE)),
  SD = c(sd(study4$T1_RIS, na.rm = TRUE),
         sd(study4$T2_RIS, na.rm = TRUE),
         sd(study4$T3_RIS, na.rm = TRUE),
         sd(study4$T1_BSMAS, na.rm = TRUE),
         sd(study4$T2_BSMAS, na.rm = TRUE),
         sd(study4$T3_BSMAS, na.rm = TRUE))
)
print(desc_stats)


library(psych)
cor_vars <- study4[,c("T1_RIS", "T2_RIS", "T3_RIS", "T1_BSMAS", "T2_BSMAS", "T3_BSMAS")]
cor_matrix <- corr.test(cor_vars, use = "pairwise", method = "pearson")
print(cor_matrix$r) 
print(cor_matrix$p)  

#cronbach.alpha
library(ltm)
items1 <- study4[, c(16:19, 21:24)]
cronbach.alpha(items1, CI = TRUE)#0.92
items2 <- study4[, c(40:47)]
cronbach.alpha(items2, CI = TRUE)#0.94
items3 <- study4[, c(64:71)]
cronbach.alpha(items3, CI = TRUE)#0.94

items4 <- study4[, c(10:15)]
cronbach.alpha(items4, CI = TRUE)#0.82
items5 <- study4[, c(34:39)]
cronbach.alpha(items5, CI = TRUE)#0.87
items6 <- study4[, c(57:62)]
cronbach.alpha(items6, CI = TRUE)#0.89

#####5.2 Cross-lagged panel model#####
library(lavaan)
library(semTools)
# 标准化变量
study4$z_T1_RIS <- scale(study4$T1_RIS)
study4$z_T2_RIS <- scale(study4$T2_RIS)
study4$z_T3_RIS <- scale(study4$T3_RIS)
study4$z_T1_BSMAS <- scale(study4$T1_BSMAS)
study4$z_T2_BSMAS <- scale(study4$T2_BSMAS)
study4$z_T3_BSMAS <- scale(study4$T3_BSMAS)


# 三波次完整交叉滞后模型(包含控制变量)
clpm_model_controlled <- '
  # 自回归路径 (稳定性路径)
  z_T2_RIS ~ a1*z_T1_RIS
  z_T3_RIS ~ a2*z_T2_RIS
  z_T2_BSMAS ~ b1*z_T1_BSMAS
  z_T3_BSMAS ~ b2*z_T2_BSMAS
  
  # 交叉滞后路径
  z_T2_RIS ~ cl1*z_T1_BSMAS
  z_T3_RIS ~ cl2*z_T2_BSMAS
  z_T2_BSMAS ~ cl3*z_T1_RIS
  z_T3_BSMAS ~ cl4*z_T2_RIS
  
  # 控制变量路径 (影响所有结果变量)
  z_T2_RIS ~ gender + age + education + nation + religion
  z_T3_RIS ~ gender + age + education + nation + religion
  z_T2_BSMAS ~ gender + age + education + nation + religion
  z_T3_BSMAS ~ gender + age + education + nation + religion
  
  # 同一时间点的协方差
  z_T1_RIS ~~ cov1*z_T1_BSMAS
  z_T2_RIS ~~ cov2*z_T2_BSMAS
  z_T3_RIS ~~ cov3*z_T3_BSMAS
  
  # 误差项协方差 
  # z_T2_RIS ~~ z_T2_BSMAS
  # z_T3_RIS ~~ z_T3_BSMAS
  
  # 控制变量与T1变量的协方差
  z_T1_RIS ~~ gender + age + education + nation + religion
  z_T1_BSMAS ~~ gender + age + education + nation + religion
  
  # 方差
  z_T1_RIS ~~ v1*z_T1_RIS
  z_T1_BSMAS ~~ v2*z_T1_BSMAS
  z_T2_RIS ~~ v3*z_T2_RIS
  z_T2_BSMAS ~~ v4*z_T2_BSMAS
  z_T3_RIS ~~ v5*z_T3_RIS
  z_T3_BSMAS ~~ v6*z_T3_BSMAS
'

# 拟合模型
clpm_fit_controlled <- sem(clpm_model_controlled, 
                           data = study4, 
                           missing = "fiml",
                           estimator = "MLR")

# 查看结果
summary(clpm_fit_controlled, 
        standardized = TRUE, 
        fit.measures = TRUE,
        rsquare = TRUE)

constrained_clpm_model <- '
  # 自回归路径 (稳定性路径) - 限制相等
  z_T2_RIS ~ a*z_T1_RIS
  z_T3_RIS ~ a*z_T2_RIS  # 与T1->T2的a相同
  
  z_T2_BSMAS ~ b*z_T1_BSMAS
  z_T3_BSMAS ~ b*z_T2_BSMAS  # 与T1->T2的b相同
  
  # 交叉滞后路径 - 限制相等
  z_T2_RIS ~ cl_ris* z_T1_BSMAS    # RIS受BSMAS影响
  z_T3_RIS ~ cl_ris* z_T2_BSMAS    # 同上系数
  
  z_T2_BSMAS ~ cl_bsmas* z_T1_RIS  # BSMAS受RIS影响
  z_T3_BSMAS ~ cl_bsmas* z_T2_RIS  # 同上系数
  
  # 控制变量路径 (允许不同时间点不同)
  z_T2_RIS ~ c1*gender + c2*age + c3*education + c4*nation + c5*religion
  z_T3_RIS ~ c6*gender + c7*age + c8*education + c9*nation + c10*religion
  
  z_T2_BSMAS ~ d1*gender + d2*age + d3*education + d4*nation + d5*religion
  z_T3_BSMAS ~ d6*gender + d7*age + d8*education + d9*nation + d10*religion
  
  # 同一时间点的协方差 - 限制相等
  z_T1_RIS ~~ cov_t1*z_T1_BSMAS
  z_T2_RIS ~~ cov_t2*z_T2_BSMAS
  z_T3_RIS ~~ cov_t3*z_T3_BSMAS
  
  # 控制变量与T1变量的协方差 (自由估计)
  z_T1_RIS ~~ gender + age + education + nation + religion
  z_T1_BSMAS ~~ gender + age + education + nation + religion
  
  # 方差 - 可自由估计或限制相等
  z_T1_RIS ~~ v1*z_T1_RIS
  z_T1_BSMAS ~~ v2*z_T1_BSMAS
  z_T2_RIS ~~ v3*z_T2_RIS
  z_T2_BSMAS ~~ v4*z_T2_BSMAS
  z_T3_RIS ~~ v5*z_T3_RIS
  z_T3_BSMAS ~~ v6*z_T3_BSMAS
'

# 拟合模型
constrained_fit <- sem(constrained_clpm_model, 
                       data = study4, 
                       missing = "fiml",
                       estimator = "MLR")

# 查看结果
summary(constrained_fit, 
        standardized = TRUE, 
        fit.measures = TRUE,
        rsquare = TRUE)

# 模型比较 (与无约束模型)
anova(clpm_fit_controlled, constrained_fit)


######5.3 RI-CLPM#######
library(lavaan)

hist(study4$T1_RIS)
hist(study4$T2_RIS)
hist(study4$T3_RIS)
hist(study4$T1_BSMAS)
hist(study4$T2_BSMAS)
hist(study4$T3_BSMAS)

shapiro.test(study4$T1_RIS)
shapiro.test(study4$T1_BSMAS)
shapiro.test(study4$T2_BSMAS)
shapiro.test(study4$T3_BSMAS)


analyze_normality <- function(data, var_name) {
  x <- na.omit(data[[var_name]])
  skewness <- moments::skewness(x)
  shapiro <- shapiro.test(x)
  ad <- nortest::ad.test(x)
  cat("=== 变量:", var_name, "===\n")
  cat("样本量:", length(x), "\n")
  cat("偏度:", round(skewness, 3), "\n")
  cat("  - 偏度解释:", 
      ifelse(skewness > 0, "正偏态(右偏)", 
             ifelse(skewness < 0, "负偏态(左偏)", "对称")), "\n")
  cat("Shapiro-Wilk检验(p值):", format.pval(shapiro$p.value, digits = 3), "\n")
  cat("Anderson-Darling检验(p值):", format.pval(ad$p.value, digits = 3), "\n\n")
  
  invisible(list(
    variable = var_name,
    n = length(x),
    skewness = skewness,
    shapiro_p = shapiro$p.value,
    ad_p = ad$p.value
  ))
}

# 安装必要包(如果尚未安装)
# install.packages(c("moments", "nortest"))
library(moments)
library(nortest)

# 分析所有变量
variables <- c("T1_RIS", "T2_RIS", "T3_RIS", "T1_BSMAS", "T2_BSMAS", "T3_BSMAS")
results <- lapply(variables, function(v) analyze_normality(study4, v))

study4$T1_RIS_log <- log(study4$T1_RIS +0.001)
study4$T2_RIS_log <- log(study4$T2_RIS +0.001)
study4$T3_RIS_log <- log(study4$T3_RIS +0.001)
study4$T1_BSMAS_log <- log(study4$T1_BSMAS +0.001)
study4$T2_BSMAS_log <- log(study4$T2_BSMAS +0.001)
study4$T3_BSMAS_log <- log(study4$T3_BSMAS +0.001)

study4$z_T1_RIS <- scale(study4$T1_RIS_log)
study4$z_T2_RIS <- scale(study4$T2_RIS_log)
study4$z_T3_RIS <- scale(study4$T3_RIS_log)
study4$z_T1_BSMAS <- scale(study4$T1_BSMAS_log)
study4$z_T2_BSMAS <- scale(study4$T2_BSMAS_log)
study4$z_T3_BSMAS <- scale(study4$T3_BSMAS_log)


ri_clpm_final <- '
  # ===== 随机截距部分 =====
  RI_RIS =~ 1*z_T1_RIS + 1*z_T2_RIS + 1*z_T3_RIS
  RI_BSMAS =~ 1*z_T1_BSMAS + 1*z_T2_BSMAS + 1*z_T3_BSMAS
  
  # ===== 个体内部分 ===== 
  # 更清晰的残差方差定义（添加"res_"前缀避免混淆）
  z_T1_RIS ~~ res_T1_RIS*z_T1_RIS
  z_T2_RIS ~~ res_T2_RIS*z_T2_RIS
  z_T3_RIS ~~ res_T3_RIS*z_T3_RIS
  z_T1_BSMAS ~~ res_T1_BSMAS*z_T1_BSMAS
  z_T2_BSMAS ~~ res_T2_BSMAS*z_T2_BSMAS
  z_T3_BSMAS ~~ res_T3_BSMAS*z_T3_BSMAS
  
  # ===== 动态关系 =====
  # 自回归路径
  z_T2_RIS ~ ar_RIS*z_T1_RIS    # RIS自回归
  z_T3_RIS ~ ar_RIS*z_T2_RIS    # 等值约束
  z_T2_BSMAS ~ ar_BSMAS*z_T1_BSMAS  # BSMAS自回归
  z_T3_BSMAS ~ ar_BSMAS*z_T2_BSMAS  # 等值约束
  
  # 交叉滞后路径
  z_T2_RIS ~ cl_RI2BS*z_T1_BSMAS  # BSMAS→RIS
  z_T3_RIS ~ cl_RI2BS*z_T2_BSMAS
  z_T2_BSMAS ~ cl_BS2RI*z_T1_RIS  # RIS→BSMAS
  z_T3_BSMAS ~ cl_BS2RI*z_T2_RIS

  
  # ===== 控制变量 =====
  RI_RIS ~ gender + age + education + nation + religion 
  RI_BSMAS ~ gender + age + education + nation + religion
  
  # ===== 协方差结构 =====
  # 随机截距相关
  RI_RIS ~~ cor_RI*RI_BSMAS
  
  # 同一时间点残差相关
  z_T1_RIS ~~ cor_T1*z_T1_BSMAS
  z_T2_RIS ~~ cor_T2*z_T2_BSMAS
  z_T3_RIS ~~ cor_T3*z_T3_BSMAS
  
  # ===== 关键约束 =====
  # 确保个体内部分与随机截距正交（可选但推荐）
  RI_RIS ~~ 0*z_T1_RIS + 0*z_T2_RIS + 0*z_T3_RIS
  RI_BSMAS ~~ 0*z_T1_BSMAS + 0*z_T2_BSMAS + 0*z_T3_BSMAS
'



# 拟合模型
fit <- sem(ri_clpm_final, 
           data = study4,
           missing = "fiml",
           estimator = "MLR",
           optim.force.converged = TRUE)

# 查看结果
summary(fit, standardized = TRUE, fit.measures = TRUE)


ri_clpm_final_free <- '
  # ===== 随机截距部分 =====
  RI_RIS =~ 1*z_T1_RIS + 1*z_T2_RIS + 1*z_T3_RIS
  RI_BSMAS =~ 1*z_T1_BSMAS + 1*z_T2_BSMAS + 1*z_T3_BSMAS
  
  # ===== 个体内部分 ===== 
  # 残差方差定义
  z_T1_RIS ~~ res_T1_RIS*z_T1_RIS
  z_T2_RIS ~~ res_T2_RIS*z_T2_RIS
  z_T3_RIS ~~ res_T3_RIS*z_T3_RIS
  z_T1_BSMAS ~~ res_T1_BSMAS*z_T1_BSMAS
  z_T2_BSMAS ~~ res_T2_BSMAS*z_T2_BSMAS
  z_T3_BSMAS ~~ res_T3_BSMAS*z_T3_BSMAS
  
  # ===== 动态关系 =====
  # 自回归路径（自由估计，不加等值约束）
  z_T2_RIS ~ ar_RIS21*z_T1_RIS     # T1→T2自回归
  z_T3_RIS ~ ar_RIS32*z_T2_RIS     # T2→T3自回归（自由估计）
  z_T2_BSMAS ~ ar_BSMAS21*z_T1_BSMAS  # T1→T2自回归
  z_T3_BSMAS ~ ar_BSMAS32*z_T2_BSMAS  # T2→T3自回归（自由估计）
  
  # 交叉滞后路径（自由估计）
  z_T2_RIS ~ cl_RI2BS21*z_T1_BSMAS  # T1 BSMAS→T2 RIS
  z_T3_RIS ~ cl_RI2BS32*z_T2_BSMAS  # T2 BSMAS→T3 RIS
  z_T2_BSMAS ~ cl_BS2RI21*z_T1_RIS  # T1 RIS→T2 BSMAS
  z_T3_BSMAS ~ cl_BS2RI32*z_T2_RIS  # T2 RIS→T3 BSMAS
  
  # ===== 控制变量 =====
  RI_RIS ~ gender + age + education + nation + religion
  RI_BSMAS ~ gender + age + education + nation + religion
  
  # ===== 协方差结构 =====
  # 随机截距相关
  RI_RIS ~~ cor_RI*RI_BSMAS
  
  # 同一时间点残差相关
  z_T1_RIS ~~ cor_T1*z_T1_BSMAS
  z_T2_RIS ~~ cor_T2*z_T2_BSMAS
  z_T3_RIS ~~ cor_T3*z_T3_BSMAS
  
  # ===== 关键约束 =====
  # 确保个体内部分与随机截距正交（可选但推荐）
  RI_RIS ~~ 0*z_T1_RIS + 0*z_T2_RIS + 0*z_T3_RIS
  RI_BSMAS ~~ 0*z_T1_BSMAS + 0*z_T2_BSMAS + 0*z_T3_BSMAS
'

# 拟合模型
fit_free <- sem(ri_clpm_final_free, 
                data = study4,
                missing = "fiml",
                estimator = "MLR",
                optim.force.converged = TRUE)
summary(fit_free, standardized = TRUE, fit.measures = TRUE)


anova(fit_free,fit)




