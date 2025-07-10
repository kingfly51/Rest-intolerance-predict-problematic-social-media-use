###### 1. Install and Load Required Packages ######
# List of packages to install (if not already installed)
packages_to_install <- c(
  "readxl",      # For reading Excel files
  "dplyr",       # Data manipulation
  "tidyverse",   # Collection of R packages for data science
  "corrplot",    # Visualization of correlation matrices
  "psych",       # Psychological and psychometric analysis
  "SHAPforxgboost", # SHAP values for XGBoost
  "ggplot2",     # Data visualization
  "xgboost",     # XGBoost algorithm
  "data.table",  # Fast data manipulation
  "here",        # Easy file path management
  "caret",       # Classification and regression training
  "Rmisc",       # Collection of useful R functions
  "cowplot"      # Enhanced plotting with ggplot2
)

# Install missing packages (skip if already installed)
install.packages(
  packages_to_install[!packages_to_install %in% installed.packages()[, "Package"]],
  dependencies = TRUE
)

# Load all packages
library(readxl)
library(dplyr)
library(tidyverse)
library(corrplot)
library(psych)
library(SHAPforxgboost)
library(ggplot2)
library(xgboost)
library(data.table)
library(here)
library(caret)
library(Rmisc)
library(cowplot)



###### 2. Correlation Matrix #####
#select data
data<- read_excel("20241118/Nature Human Behaviour/final/code/study1/data/data_for_correlation.xlsx")
#Create correlation matrix
cor_matrix <- cor(data, method="pearson")
#Heat map drawing
my_palette <- colorRampPalette(c('#11427C','white','#C31E1F'))(50)
# Create color vector
label_colors <- rep(NA, 34)
#Assign colors to each set of variables
label_colors[1:9] <- "#CC6600"   
label_colors[10:19] <- "#0000FF"   
label_colors[20:22] <- "#FF00FF"
label_colors[23:33] <- "#FF0000"
label_colors[34] <- "black"
#Create heat map
corrplot(cor_matrix, 
         method = "color",
         tl.cex = 0.5, 
         tl.col = label_colors, 
         col = my_palette,
         number.cex = 0.4,  # Annotated size
         addCoef.col = "black",  # Add color to correlation coefficient
         number.col = "black",   # numeral color
         is.corr = TRUE,         # The matrix represents correlation
         type = "lower",         # Only display the lower triangular matrix
         tl.srt = 45,           # Label rotation angle
         tl.font = 2,           # Label font format (italic)
         cl.lim = c(-1, 1)      # color limit
)



###### 3.XGBoost model ######

#import and select data
dataXY_df <- read_excel("20241118/Nature Human Behaviour/final/code/study1/data/data_for_XGBoost.xlsx")
dataXY_df$label<- as.factor(dataXY_df$label)

# set seed
set.seed(1234)
# Randomly select 70% as the index for the test set
train_indices <- sample(1:nrow(dataXY_df), size = 0.7 * nrow(dataXY_df))
# Extract test and validation sets
train_set <- dataXY_df[train_indices, ]
test_set <- dataXY_df[-train_indices, ]

#write.csv(train_set,"train_set.csv") # see data train_set.csv
#write.csv(test_set,"test_set.csv") # see data test_set.csv

#using one hot encoding
labels <- train_set$label
ts_label <- test_set$label
new_tr <- model.matrix(~.+0, data = train_set[, !names(train_set) %in% "label"])
new_ts <- model.matrix(~.+0, data = test_set[, !names(test_set) %in% "label"])

#convert factor to numeric
labels <- as.numeric(labels)-1
ts_label <- as.numeric(ts_label)-1

#preparing matrix
dtrain <- xgb.DMatrix(data = new_tr,label = labels)
dtest <- xgb.DMatrix(data = new_ts,label=ts_label)

#default parameters
params <- list(
  booster = "gbtree",
  objective = "binary:logistic",#  "binary:logistic",reg:squarederror
  eta=0.3,#Learning rate 
  gamma=0,#minimum loss reduction
  max_depth=6,#maximum depth of a tree
  min_child_weight=1,#minimum sum of instance weight (hessian)
  subsample=1,#Sub-sample
  colsample_bytree=1,#Col-sample-by-tree
  lambda=1,
  alpha=0
)

xgbcv <- xgb.cv(params = params
                ,data = dtrain
                ,nrounds = 100
                ,nfold = 10
                ,showsd = T
                ,stratified = T
                ,print_every_n = 10
                ,early_stop_round = 20
                ,maximize = F
)
# View evaluation logs
eval_log <- xgbcv$evaluation_log
# print evaluation logs
print(eval_log)
# Extract the optimal number of iterations
eval_log$iter[which.min(eval_log$test_logloss_mean)]
#14

#first default - model training
xgb1 <- xgb.train(
  params = params
  ,data = dtrain
  ,nrounds = 14 #optimal number of iterations
  ,watchlist = list(val=dtest,train=dtrain)
  ,print_every_n = 10
  ,early_stop_round = 10
  ,maximize = F
  ,eval_metric = "error"
)

#model prediction
xgbpred <- predict(xgb1,dtest)
xgbpred <- ifelse(xgbpred > 0.5,1,0)

#confusion matrix
confusionMatrix(as.factor(xgbpred), as.factor(ts_label))
#Calculate confusion matrix
cm <- confusionMatrix(as.factor(xgbpred), as.factor(ts_label))
# Extracting indicators from the confusion matrix
accuracy <- cm$overall['Accuracy']
precision <- cm$byClass['Precision']
recall <- cm$byClass['Recall']
f1_score <- cm$byClass['F1']
# print results
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")


####SHAP
# To return the SHAP values and ranked features by mean|SHAP|
shap_values <- shap.values(xgb_model = xgb1, X_train = new_tr)
# The ranked features by mean |SHAP|
shap_values$mean_shap_score
# To prepare the long-format data:
shap_long <- shap.prep(xgb_model = xgb1, X_train = new_tr)
# is the same as: using given shap_contrib
shap_long <- shap.prep(shap_contrib = shap_values$shap_score, X_train = new_tr)
# **SHAP summary plot**
# Take the average of the absolute SHAP values of each feature as the importance of that feature
shap.plot.summary(shap_long, kind="bar")


###SHAP value of social media addiction features in XGBoost model
##rest_intol
shap_rest_intol<- shap_long[shap_long$variable=="Rest_Intolerance",]
# Create scatter plots and Loess fitting lines
p <- ggplot(shap_rest_intol, aes(x = rfvalue, y = value)) + 
  geom_point(size = 2, color = "#99FF99") +                    
  geom_smooth(method = "loess", se = FALSE, color = "red", size = 1) + 
  labs(x = "Rest Intolerance", y = "SHAP Value") +          
  theme_minimal() +                                     
  theme(
    axis.text.x = element_text(size = 14),                
    axis.text.y = element_text(size = 14),               
    axis.title.x = element_text(size = 16),                
    axis.title.y = element_text(size = 16)             
  )                                           
# Calculate and fit the intersection point of the line
loess_fit <- loess(value ~ rfvalue, data = shap_rest_intol)
predicted_values <- predict(loess_fit, newdata = data.frame(rfvalue = seq(min(shap_rest_intol$rfvalue), max(shap_rest_intol$rfvalue), length.out = 100)))
# Find the intersection point
zero_crossing_index <- which.min(abs(predicted_values)) 
intersection_x <- seq(min(shap_rest_intol$rfvalue), max(shap_rest_intol$rfvalue), length.out = 100)[zero_crossing_index]
# Draw graphics
p + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") + 
  geom_vline(xintercept = intersection_x, linetype = "dashed", color = "blue") +
  geom_text(aes(x = intersection_x, y = 0, label = round(intersection_x, 2)), 
            vjust = -1, color = "purple",size=6) +
  theme(legend.position = "none")  

##depre
shap_depre<- shap_long[shap_long$variable=="Depression",]
p <- ggplot(shap_depre, aes(x = rfvalue, y = value)) + 
  geom_point(size = 2, color = "#99FF99") +                     
  geom_smooth(method = "loess", se = FALSE, color = "red", size = 1) + 
  labs(x = "Depression", y = "SHAP Value") +         
  theme_minimal() +                                   
  theme(
    axis.text.x = element_text(size = 14),               
    axis.text.y = element_text(size = 14),         
    axis.title.x = element_text(size = 16),         
    axis.title.y = element_text(size = 16)            
  )                                            

loess_fit <- loess(value ~ rfvalue, data = shap_depre)
predicted_values <- predict(loess_fit, newdata = data.frame(rfvalue = seq(min(shap_depre$rfvalue), max(shap_depre$rfvalue), length.out = 100)))

zero_crossing_index <- which.min(abs(predicted_values)) 
intersection_x <- seq(min(shap_depre$rfvalue), max(shap_depre$rfvalue), length.out = 100)[zero_crossing_index] 

p + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = intersection_x, linetype = "dashed", color = "blue") + 
  geom_text(aes(x = intersection_x, y = 0, label = round(intersection_x, 2)), 
            vjust = -1, color = "purple",size=6) +
  theme(legend.position = "none")

##Stress
shap_depre<- shap_long[shap_long$variable=="Stress",]
p <- ggplot(shap_depre, aes(x = rfvalue, y = value)) + 
  geom_point(size = 2, color = "#99FF99") +                    
  geom_smooth(method = "loess", se = FALSE, color = "red", size = 1) + 
  labs(x = "Stress", y = "SHAP Value") +         
  theme_minimal() +                                        
  theme(
    axis.text.x = element_text(size = 14),              
    axis.text.y = element_text(size = 14),                
    axis.title.x = element_text(size = 16),             
    axis.title.y = element_text(size = 16)           
  )                                         

loess_fit <- loess(value ~ rfvalue, data = shap_depre)
predicted_values <- predict(loess_fit, newdata = data.frame(rfvalue = seq(min(shap_depre$rfvalue), max(shap_depre$rfvalue), length.out = 100)))

zero_crossing_index <- which.min(abs(predicted_values)) 
intersection_x <- seq(min(shap_depre$rfvalue), max(shap_depre$rfvalue), length.out = 100)[zero_crossing_index] 

p + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = intersection_x, linetype = "dashed", color = "blue") + 
  geom_text(aes(x = intersection_x, y = 0, label = round(intersection_x, 2)), 
            vjust = -1, color = "purple",size= 6) +  
  theme(legend.position = "none")



###### 4.Draw a graph of LPA based on Mplus ######
#import data
MASLPA4 <- read_excel("20241118/Nature Human Behaviour/final/code/study1/data/MASLPA4.xlsx")
MASLPA4$class<-as.factor(MASLPA4$class)
#Convert wide format data to long format
data_long <- pivot_longer(MASLPA4, 
                          cols = c(mas1, mas2,mas3,mas4,mas5,mas6), 
                          names_to = "item", 
                          values_to = "score")
# draw graph
tgc1 <- summarySE(data_long, measurevar="score", groupvars=c("class","item"))
ggplot(tgc1, aes(x=item, y=score, colour=class,group=class,shape=class))+
  geom_point(size=4)+
  geom_line(position = position_dodge(0.1),cex=1.3)+
  theme_bw()+
  scale_color_manual(values = c('#BABABA','#0001A1','#037F77','#C5272D'))+ 
  scale_shape_manual(values = c(15,16,17,18))+
  scale_y_continuous(limits = c(1,5),expand = c(0,0))+
  labs(x='Social Media Addition',y='Score')+
  theme_test(base_size = 20)+
  theme(legend.title = element_blank(),
        legend.text = element_text(family = 'serif'), 
        legend.position = c(.3,.92),
        legend.direction = "horizontal",
        axis.text = element_text(color = 'black',family = 'serif'),
        axis.title = element_text(family = 'serif',size = 18,color = 'black'))




