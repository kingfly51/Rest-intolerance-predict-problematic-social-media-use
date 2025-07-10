###### 1. Install and Load Required Packages ######
library(readxl)
library(psych)
library(ltm)
library(lavaan)
library(semTools)
library(moments)
library(nortest)
library(semTools)


###### 2.Descriptive statistics and correlation analysis######
study4 <- read_excel("20241118/Nature Human Behaviour/final/code/study4/data/three_wave_data.xlsx")
study4$age <- as.numeric(study4$age)

study4$T1_RIS <- study4$RIS1.x+study4$RIS2.x+study4$RIS3.x+study4$RIS4.x+study4$RIS5.x+
  study4$RIS6.x+study4$RIS7.x+study4$RIS8.x
study4$T2_RIS <- study4$RIS1.y+study4$RIS2.y+study4$RIS3.y+study4$RIS4.y+study4$RIS5.y+
  study4$RIS6.y+study4$RIS7.y+study4$RIS8.y

study4$T1_BSMAS <-study4$BSMAS1.x+study4$BSMAS2.x+study4$BSMAS3.x+
  study4$BSMAS4.x+study4$BSMAS5.x+study4$BSMAS6.x
study4$T2_BSMAS <-study4$BSMAS1.y+study4$BSMAS2.y+study4$BSMAS3.y+
  study4$BSMAS4.y+study4$BSMAS5.y+study4$BSMAS6.y

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


cor_vars <- study4[,c("T1_RIS", "T2_RIS", "T3_RIS", "T1_BSMAS", "T2_BSMAS", "T3_BSMAS")]
cor_matrix <- corr.test(cor_vars, use = "pairwise", method = "pearson")
print(cor_matrix$r) 
print(cor_matrix$p)  

#cronbach.alpha;ris
items1 <- study4[, c(13:20)]
cronbach.alpha(items1, CI = TRUE)#0.92
items2 <- study4[, c(27:34)]
cronbach.alpha(items2, CI = TRUE)#0.94
items3 <- study4[, c(42:49)]
cronbach.alpha(items3, CI = TRUE)#0.94
#cronbach.alpha;bsmas
items4 <- study4[, c(7:12)]
cronbach.alpha(items4, CI = TRUE)#0.82
items5 <- study4[, c(21:26)]
cronbach.alpha(items5, CI = TRUE)#0.87
items6 <- study4[, c(35:40)]
cronbach.alpha(items6, CI = TRUE)#0.89


##### 3. Cross-lagged panel model#####
# Standardization
study4$z_T1_RIS <- scale(study4$T1_RIS)
study4$z_T2_RIS <- scale(study4$T2_RIS)
study4$z_T3_RIS <- scale(study4$T3_RIS)
study4$z_T1_BSMAS <- scale(study4$T1_BSMAS)
study4$z_T2_BSMAS <- scale(study4$T2_BSMAS)
study4$z_T3_BSMAS <- scale(study4$T3_BSMAS)

# Three wave complete cross lag model (including control variables)
clpm_model_controlled <- '
  # Autoregressive path (stability path)
  z_T2_RIS ~ a1*z_T1_RIS
  z_T3_RIS ~ a2*z_T2_RIS
  z_T2_BSMAS ~ b1*z_T1_BSMAS
  z_T3_BSMAS ~ b2*z_T2_BSMAS
  
  # Cross lagged path
  z_T2_RIS ~ cl1*z_T1_BSMAS
  z_T3_RIS ~ cl2*z_T2_BSMAS
  z_T2_BSMAS ~ cl3*z_T1_RIS
  z_T3_BSMAS ~ cl4*z_T2_RIS
  
  # Control variable path (affecting all outcome variables)
  z_T2_RIS ~ gender + age + education + nation + religion
  z_T3_RIS ~ gender + age + education + nation + religion
  z_T2_BSMAS ~ gender + age + education + nation + religion
  z_T3_BSMAS ~ gender + age + education + nation + religion
  
  # Covariance at the same time point
  z_T1_RIS ~~ cov1*z_T1_BSMAS
  z_T2_RIS ~~ cov2*z_T2_BSMAS
  z_T3_RIS ~~ cov3*z_T3_BSMAS
  
  # Covariance of error term
  # z_T2_RIS ~~ z_T2_BSMAS
  # z_T3_RIS ~~ z_T3_BSMAS
  
  # Covariance between control variables and T1 variables
  z_T1_RIS ~~ gender + age + education + nation + religion
  z_T1_BSMAS ~~ gender + age + education + nation + religion
  
  # variance
  z_T1_RIS ~~ v1*z_T1_RIS
  z_T1_BSMAS ~~ v2*z_T1_BSMAS
  z_T2_RIS ~~ v3*z_T2_RIS
  z_T2_BSMAS ~~ v4*z_T2_BSMAS
  z_T3_RIS ~~ v5*z_T3_RIS
  z_T3_BSMAS ~~ v6*z_T3_BSMAS
'

# fit model
clpm_fit_controlled <- sem(clpm_model_controlled, 
                           data = study4, 
                           missing = "fiml",
                           estimator = "MLR")

# results
summary(clpm_fit_controlled, 
        standardized = TRUE, 
        fit.measures = TRUE,
        rsquare = TRUE)


constrained_clpm_model <- '
  # Autoregressive Path (Stability Path) - Constrained Equal
  z_T2_RIS ~ a*z_T1_RIS
  z_T3_RIS ~ a*z_T2_RIS  # Same as a in T1->T2
  
  z_T2_BSMAS ~ b*z_T1_BSMAS
  z_T3_BSMAS ~ b*z_T2_BSMAS  # Same as b in T1->T2
  
  # Cross lagged path - restricted equality
  z_T2_RIS ~ cl_ris* z_T1_BSMAS    
  z_T3_RIS ~ cl_ris* z_T2_BSMAS    
  
  z_T2_BSMAS ~ cl_bsmas* z_T1_RIS 
  z_T3_BSMAS ~ cl_bsmas* z_T2_RIS  
  
  # Control variable path (allowing different time points to be different)
  z_T2_RIS ~ c1*gender + c2*age + c3*education + c4*nation + c5*religion
  z_T3_RIS ~ c6*gender + c7*age + c8*education + c9*nation + c10*religion
  
  z_T2_BSMAS ~ d1*gender + d2*age + d3*education + d4*nation + d5*religion
  z_T3_BSMAS ~ d6*gender + d7*age + d8*education + d9*nation + d10*religion
  
  # Covariance with equal constraints at the same time point
  z_T1_RIS ~~ cov_t1*z_T1_BSMAS
  z_T2_RIS ~~ cov_t2*z_T2_BSMAS
  z_T3_RIS ~~ cov_t3*z_T3_BSMAS
  
  # Covariance between control variables and T1 variables (free estimation)
  z_T1_RIS ~~ gender + age + education + nation + religion
  z_T1_BSMAS ~~ gender + age + education + nation + religion
  
  #  variance
  z_T1_RIS ~~ v1*z_T1_RIS
  z_T1_BSMAS ~~ v2*z_T1_BSMAS
  z_T2_RIS ~~ v3*z_T2_RIS
  z_T2_BSMAS ~~ v4*z_T2_BSMAS
  z_T3_RIS ~~ v5*z_T3_RIS
  z_T3_BSMAS ~~ v6*z_T3_BSMAS
'

# fit model
constrained_fit <- sem(constrained_clpm_model, 
                       data = study4, 
                       missing = "fiml",
                       estimator = "MLR")

# results
summary(constrained_fit, 
        standardized = TRUE, 
        fit.measures = TRUE,
        rsquare = TRUE)

# Model comparison (with unconstrained model)
anova(clpm_fit_controlled, constrained_fit)


###### 4. RI-CLPM #######
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
  cat("=== Variable:", var_name, "===\n")
  cat("Sample:", length(x), "\n")
  cat("skewness:", round(skewness, 3), "\n")
  cat("  - Skewness Explanation:", 
      ifelse(skewness > 0, "Positive skewness (right skewed)", 
             ifelse(skewness < 0, "Negative Skewness (Left Skewness)", "symmetry")), "\n")
  cat("Shapiro-Wilk test(p):", format.pval(shapiro$p.value, digits = 3), "\n")
  cat("Anderson-Darling test(p):", format.pval(ad$p.value, digits = 3), "\n\n")
  
  invisible(list(
    variable = var_name,
    n = length(x),
    skewness = skewness,
    shapiro_p = shapiro$p.value,
    ad_p = ad$p.value
  ))
}

# Analyze all variables
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
  # ===== Random intercept section =====
  RI_RIS =~ 1*z_T1_RIS + 1*z_T2_RIS + 1*z_T3_RIS
  RI_BSMAS =~ 1*z_T1_BSMAS + 1*z_T2_BSMAS + 1*z_T3_BSMAS
  
  # ===== Within-Individual parts ===== 
  # A clearer definition of residual variance
  z_T1_RIS ~~ res_T1_RIS*z_T1_RIS
  z_T2_RIS ~~ res_T2_RIS*z_T2_RIS
  z_T3_RIS ~~ res_T3_RIS*z_T3_RIS
  z_T1_BSMAS ~~ res_T1_BSMAS*z_T1_BSMAS
  z_T2_BSMAS ~~ res_T2_BSMAS*z_T2_BSMAS
  z_T3_BSMAS ~~ res_T3_BSMAS*z_T3_BSMAS
  
  # ===== dynamics =====
  # Autoregressive path
  z_T2_RIS ~ ar_RIS*z_T1_RIS    # RIS autoregression
  z_T3_RIS ~ ar_RIS*z_T2_RIS    # Equivalent constraint
  z_T2_BSMAS ~ ar_BSMAS*z_T1_BSMAS  # BSMAS autoregression
  z_T3_BSMAS ~ ar_BSMAS*z_T2_BSMAS  # Equivalent constraint
  
  # Cross lagged path
  z_T2_RIS ~ cl_RI2BS*z_T1_BSMAS  # BSMAS→RIS
  z_T3_RIS ~ cl_RI2BS*z_T2_BSMAS
  z_T2_BSMAS ~ cl_BS2RI*z_T1_RIS  # RIS→BSMAS
  z_T3_BSMAS ~ cl_BS2RI*z_T2_RIS

  
  # ===== control variable =====
  RI_RIS ~ gender + age + education + nation + religion 
  RI_BSMAS ~ gender + age + education + nation + religion
  
  # ===== covariance structure =====
  # Random intercept correlation
  RI_RIS ~~ cor_RI*RI_BSMAS
  
  # Residual correlation at the same time point
  z_T1_RIS ~~ cor_T1*z_T1_BSMAS
  z_T2_RIS ~~ cor_T2*z_T2_BSMAS
  z_T3_RIS ~~ cor_T3*z_T3_BSMAS
  
  # ===== key constraints =====
  # Ensure that the internal part of the individual is orthogonal to the random intercept
  RI_RIS ~~ 0*z_T1_RIS + 0*z_T2_RIS + 0*z_T3_RIS
  RI_BSMAS ~~ 0*z_T1_BSMAS + 0*z_T2_BSMAS + 0*z_T3_BSMAS
'


# fit model
fit <- sem(ri_clpm_final, 
           data = study4,
           missing = "fiml",
           estimator = "MLR",
           optim.force.converged = TRUE)

# results
summary(fit, standardized = TRUE, fit.measures = TRUE)


ri_clpm_final_free <- '
  # ===== Random intercept section =====
  RI_RIS =~ 1*z_T1_RIS + 1*z_T2_RIS + 1*z_T3_RIS
  RI_BSMAS =~ 1*z_T1_BSMAS + 1*z_T2_BSMAS + 1*z_T3_BSMAS
  
  # ===== Within Individual parts ===== 
  # Definition of Residual Variance
  z_T1_RIS ~~ res_T1_RIS*z_T1_RIS
  z_T2_RIS ~~ res_T2_RIS*z_T2_RIS
  z_T3_RIS ~~ res_T3_RIS*z_T3_RIS
  z_T1_BSMAS ~~ res_T1_BSMAS*z_T1_BSMAS
  z_T2_BSMAS ~~ res_T2_BSMAS*z_T2_BSMAS
  z_T3_BSMAS ~~ res_T3_BSMAS*z_T3_BSMAS
  
  # ===== dynamics =====
  # Autoregressive path (free estimation, without equivalence constraints)
  z_T2_RIS ~ ar_RIS21*z_T1_RIS     
  z_T3_RIS ~ ar_RIS32*z_T2_RIS     
  z_T2_BSMAS ~ ar_BSMAS21*z_T1_BSMAS  
  z_T3_BSMAS ~ ar_BSMAS32*z_T2_BSMAS 
  
  # Cross lagged path (free estimation)
  z_T2_RIS ~ cl_RI2BS21*z_T1_BSMAS  # T1 BSMAS→T2 RIS
  z_T3_RIS ~ cl_RI2BS32*z_T2_BSMAS  # T2 BSMAS→T3 RIS
  z_T2_BSMAS ~ cl_BS2RI21*z_T1_RIS  # T1 RIS→T2 BSMAS
  z_T3_BSMAS ~ cl_BS2RI32*z_T2_RIS  # T2 RIS→T3 BSMAS
  
  # ===== control variable =====
  RI_RIS ~ gender + age + education + nation + religion
  RI_BSMAS ~ gender + age + educcovariance structureation + nation + religion
  
  # ===== covariance structure =====
  # Random intercept correlation
  RI_RIS ~~ cor_RI*RI_BSMAS
  
  # Residual correlation at the same time point
  z_T1_RIS ~~ cor_T1*z_T1_BSMAS
  z_T2_RIS ~~ cor_T2*z_T2_BSMAS
  z_T3_RIS ~~ cor_T3*z_T3_BSMAS
  
  # ===== key constraints =====
  # Ensure that the internal part of the individual is orthogonal to the random intercept
  RI_RIS ~~ 0*z_T1_RIS + 0*z_T2_RIS + 0*z_T3_RIS
  RI_BSMAS ~~ 0*z_T1_BSMAS + 0*z_T2_BSMAS + 0*z_T3_BSMAS
'

#fit model
fit_free <- sem(ri_clpm_final_free, 
                data = study4,
                missing = "fiml",
                estimator = "MLR",
                optim.force.converged = TRUE)
summary(fit_free, standardized = TRUE, fit.measures = TRUE)

anova(fit_free,fit)
