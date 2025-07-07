rm(list=ls())

library(readxl)
RIS_data <- read_excel("RIS_data.xlsx")
dataXY_df <- as.data.frame(RIS_data[,c(10,15,19,29,30,35,36,44,45,46,47,58,59,64,70,74,94,106,109:110,112:113,  
                                       131,133,136,141,145,148,153,159,162,165,170,
                                       178)])#81,89,100,107,111,124:128,108,101
colnames(dataXY_df)[ncol(dataXY_df)]<-"label"
dataXY_df <- na.omit(dataXY_df)
dataXY_df <- dataXY_df[dataXY_df$Gender!=3,]
dataXY_df$label<- as.factor(dataXY_df$label)
write.csv(dataXY_df,"data_shap.csv")

##scale
library(dplyr)
# 标准化数据框中除了 label 列之外的所有变量
data_normalized <- dataXY_df %>%
  select(-label) %>%                     # 去掉 label 列
  scale() %>%                            # 标准化
  as.data.frame()                       # 转换为数据框格式
# 将标准化后的数据与 label 列合并
dataXY_df <- cbind(data_normalized, label = dataXY_df$label)
# 使用 model.matrix 进行独热编码
#dummy_vars_gender <- model.matrix(~ Gender -1, data = dataXY_df)
#dummy_vars <- dummy_vars[,-1]#去除第一列截距项
# 将虚拟变量与原始数据合并
#dataXY_df <- cbind(dataXY_df, dummy_vars_gender)
#colnames(dataXY_df)[43:44]<- c("Male","Female")
#dataXY_df<-dataXY_df[,c(-25)]

#绘制所有变量彼此之间的相关性矩阵热图
library(corrplot)
library(psych)
library(grDevices)
#data<-as.data.frame(RIS_data[,c(10,15,19,29,30,35,36,44,45,46,47,58,59,64,70,74,94,106,109:110,112:113,  
#                                131,133,136,141,145,148,153,159,162,165,170,
#                                43)])
data<-as.data.frame(RIS_data[,c(10,15,19,29,35,36,44,59,94,
                                30,46,47,58,64,70,74,106,109,110,
                                45,112:113,  
                                131,133,136,141,145,148,153,159,162,165,170,
                                43)])

colnames(data)[34]<-"Social_Media_Addition"
data <- na.omit(data)
data <- data[data$Gender!=3,]

# 创建相关性矩阵
cor_matrix <- cor(data, method="pearson")#
#cor_test <- corr.test(data,method="spearman")# ,adjust="fdr"
#使用spearman方法计算相关性矩阵
#pvalues <- cor_test$p  # 提取p值矩阵
#cor_matrix[pvalues > 0.05] <-0  # 将未通过多重比较校正的相关性设置为NA
# 热图绘制
# 选择更好看的配色方案
my_palette <- colorRampPalette(c('#11427C','white','#C31E1F'))(50)
# 创建颜色向量
label_colors <- rep(NA, 34)  # 初始化颜色向量
# 为每组变量分配颜色
label_colors[1:9] <- "#CC6600"   # 第一组（10个变量）为黄色
label_colors[10:19] <- "#0000FF"    # 第二组（15个变量）为绿色
label_colors[20:22] <- "#FF00FF"      # 第三组（5个变量）为灰色
label_colors[23:33] <- "#FF0000"       # 第四组（5个变量）为红色
label_colors[34] <- "black"         # 第五组（1个变量）为黑色
# 创建热图
corrplot(cor_matrix, 
         method = "color",
         tl.cex = 0.5, 
         tl.col = label_colors, 
         col = my_palette,
         number.cex = 0.4,  # 标注的大小
         addCoef.col = "black",  # 添加相关系数的颜色
         number.col = "black",   # 数字颜色
         is.corr = TRUE,         # 表示矩阵是相关性
         type = "lower",         # 仅显示下三角矩阵
         tl.srt = 45,           # 标签旋转角度
         tl.font = 2,           # 标签字体格式（斜体）
         cl.lim = c(-1, 1)      # color limit
)




# 设置随机数种子，确保结果可重复
set.seed(1234)
# 随机抽取70%作为测试集的索引
train_indices <- sample(1:nrow(dataXY_df), size = 0.7 * nrow(dataXY_df))
# 提取测试集和验证集
train_set <- dataXY_df[train_indices, ]
test_set <- dataXY_df[-train_indices, ]
#write.csv(train_set,"train_set.csv")
#write.csv(test_set,"test_set.csv")
# run the model with built-in data
suppressPackageStartupMessages({
  library("SHAPforxgboost"); library("ggplot2"); library("xgboost")
  library("data.table"); library("here")
})

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
# 查看评估日志
eval_log <- xgbcv$evaluation_log
# 打印评估日志
print(eval_log)
# 提取最佳迭代次数
eval_log$iter[which.min(eval_log$test_logloss_mean)]
#14

#first default - model training
xgb1 <- xgb.train(
  params = params
  ,data = dtrain
  ,nrounds = 14 #最佳迭代次数
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
library(caret)
confusionMatrix(as.factor(xgbpred), as.factor(ts_label))
#Accuracy - 89.81%
# 计算混淆矩阵
cm <- confusionMatrix(as.factor(xgbpred), as.factor(ts_label))
# 从混淆矩阵中提取指标
accuracy <- cm$overall['Accuracy']
precision <- cm$byClass['Precision']
recall <- cm$byClass['Recall']
f1_score <- cm$byClass['F1']
# 打印结果
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")


#view variable importance plot
mat <- xgb.importance(feature_names = colnames(new_tr),model = xgb1)
xgb.plot.importance(importance_matrix = mat[1:46]) #first 20 variables


######SHAP#######
# To return the SHAP values and ranked features by mean|SHAP|
shap_values <- shap.values(xgb_model = xgb1, X_train = new_tr)
# The ranked features by mean |SHAP|
shap_values$mean_shap_score
# To prepare the long-format data:
shap_long <- shap.prep(xgb_model = xgb1, X_train = new_tr)
# is the same as: using given shap_contrib
shap_long <- shap.prep(shap_contrib = shap_values$shap_score, X_train = new_tr)
# **SHAP summary plot**
shap.plot.summary(shap_long)
# 取每个特征的SHAP值的绝对值的平均值作为该特征的重要性
shap.plot.summary(shap_long, kind="bar")

###XGBoost模型中社交媒体成瘾特征的SHAP值

##rest_intol
shap_rest_intol<- shap_long[shap_long$variable=="Rest_Intolerance",]
# 创建散点图及 loess 拟合线
p <- ggplot(shap_rest_intol, aes(x = rfvalue, y = value)) + 
  geom_point(size = 2, color = "#99FF99") +                     # 样本点放大并颜色统一
  geom_smooth(method = "loess", se = FALSE, color = "red", size = 1) + # 拟合线为红色
  labs(x = "Rest Intolerance", y = "SHAP Value") +           # 标签
  theme_minimal() +                               # 简约主题           
  theme(
    axis.text.x = element_text(size = 14),                 # X轴数字大小
    axis.text.y = element_text(size = 14),                 # Y轴数字大小
    axis.title.x = element_text(size = 16),                # X轴标签大小
    axis.title.y = element_text(size = 16)                 # Y轴标签大小
  )                                             # 简约主题
# 计算与拟合线的交点，假设使用 loess 方法
loess_fit <- loess(value ~ rfvalue, data = shap_rest_intol)
predicted_values <- predict(loess_fit, newdata = data.frame(rfvalue = seq(min(shap_rest_intol$rfvalue), max(shap_rest_intol$rfvalue), length.out = 100)))
# 找到交点
zero_crossing_index <- which.min(abs(predicted_values)) # 找到最接近0的预测值的索引
intersection_x <- seq(min(shap_rest_intol$rfvalue), max(shap_rest_intol$rfvalue), length.out = 100)[zero_crossing_index] # 交点的x坐标
# 绘制图形
p + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") + # y=0 的水平虚线
  geom_vline(xintercept = intersection_x, linetype = "dashed", color = "blue") + # 与拟合线的交点
  geom_text(aes(x = intersection_x, y = 0, label = round(intersection_x, 2)), 
            vjust = -1, color = "purple",size=6) +  # 在交点上方标记 intersection_x 值
  theme(legend.position = "none")  # 去除图例

##depre
shap_depre<- shap_long[shap_long$variable=="Depression",]
p <- ggplot(shap_depre, aes(x = rfvalue, y = value)) + 
  geom_point(size = 2, color = "#99FF99") +                     # 样本点放大并颜色统一
  geom_smooth(method = "loess", se = FALSE, color = "red", size = 1) + # 拟合线为红色
  labs(x = "Depression", y = "SHAP Value") +           # 标签
  theme_minimal() +                               # 简约主题           
  theme(
    axis.text.x = element_text(size = 14),                 # X轴数字大小
    axis.text.y = element_text(size = 14),                 # Y轴数字大小
    axis.title.x = element_text(size = 16),                # X轴标签大小
    axis.title.y = element_text(size = 16)                 # Y轴标签大小
  )                                               # 简约主题
# 计算与拟合线的交点，假设使用 loess 方法
loess_fit <- loess(value ~ rfvalue, data = shap_depre)
predicted_values <- predict(loess_fit, newdata = data.frame(rfvalue = seq(min(shap_depre$rfvalue), max(shap_depre$rfvalue), length.out = 100)))
# 找到交点
zero_crossing_index <- which.min(abs(predicted_values)) # 找到最接近0的预测值的索引
intersection_x <- seq(min(shap_depre$rfvalue), max(shap_depre$rfvalue), length.out = 100)[zero_crossing_index] # 交点的x坐标
# 绘制图形
p + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") + # y=0 的水平虚线
  geom_vline(xintercept = intersection_x, linetype = "dashed", color = "blue") + # 与拟合线的交点
  geom_text(aes(x = intersection_x, y = 0, label = round(intersection_x, 2)), 
            vjust = -1, color = "purple",size=6) +  # 在交点上方标记 intersection_x 值
  theme(legend.position = "none")  # 去除图例

##Stress
shap_depre<- shap_long[shap_long$variable=="Stress",]
p <- ggplot(shap_depre, aes(x = rfvalue, y = value)) + 
  geom_point(size = 2, color = "#99FF99") +                     # 样本点放大并颜色统一
  geom_smooth(method = "loess", se = FALSE, color = "red", size = 1) + # 拟合线为红色
  labs(x = "Stress", y = "SHAP Value") +           # 标签
  theme_minimal() +                                          # 简约主题
  theme(
    axis.text.x = element_text(size = 14),                 # X轴数字大小
    axis.text.y = element_text(size = 14),                 # Y轴数字大小
    axis.title.x = element_text(size = 16),                # X轴标签大小
    axis.title.y = element_text(size = 16)                 # Y轴标签大小
  )                                            # 简约主题
# 计算与拟合线的交点，假设使用 loess 方法
loess_fit <- loess(value ~ rfvalue, data = shap_depre)
predicted_values <- predict(loess_fit, newdata = data.frame(rfvalue = seq(min(shap_depre$rfvalue), max(shap_depre$rfvalue), length.out = 100)))
# 找到交点
zero_crossing_index <- which.min(abs(predicted_values)) # 找到最接近0的预测值的索引
intersection_x <- seq(min(shap_depre$rfvalue), max(shap_depre$rfvalue), length.out = 100)[zero_crossing_index] # 交点的x坐标
# 绘制图形
p + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") + # y=0 的水平虚线
  geom_vline(xintercept = intersection_x, linetype = "dashed", color = "blue") + # 与拟合线的交点
  geom_text(aes(x = intersection_x, y = 0, label = round(intersection_x, 2)), 
            vjust = -1, color = "purple",size= 6) +  # 在交点上方标记 intersection_x 值
  theme(legend.position = "none")  # 去除图例


#######模型参数调整########
#convert characters to factors
#fact_col <- colnames(train_set)[sapply(train_set,is.character)]
#for(i in fact_col)
#  set(train_set,j=i,value = factor(train_set[[i]]))
#for(i in fact_col)
#  set(test_set,j=i,value = factor(test_set[[i]]))
#load libraries
library(mlr)

#create tasks
traintask <- makeClassifTask(data = train_set,target = "label")
testtask <- makeClassifTask(data = test_set,target = "label")

#do one hot encoding
#traintask <- createDummyFeatures(obj = traintask,target = "label")
#testtask <- createDummyFeatures(obj = testtask,target = "label")

#create learner
lrn <- makeLearner("classif.xgboost",predict.type = "response")
lrn$par.vals <- list(
  objective="binary:logistic",
  eval_metric="error",
  nrounds=14L,
  eta=0.3
)

#set parameter space
params <- makeParamSet(
  makeDiscreteParam("booster",values = c("gbtree","gblinear")),
  makeIntegerParam("max_depth",lower = 3L,upper = 10L),
  makeNumericParam("min_child_weight",lower = 1L,upper = 10L),
  makeNumericParam("subsample",lower = 0.5,upper = 1),
  makeNumericParam("colsample_bytree",lower = 0.5,upper = 1)
)

#set resampling strategy
rdesc <- makeResampleDesc("CV",stratify = T,iters=10L)

#search strategy
ctrl <- makeTuneControlRandom(maxit = 10L)

#set parallel backend
library(parallel)
library(parallelMap)
parallelStartSocket(cpus = detectCores())

#parameter tuning
mytune <- tuneParams(learner = lrn
                     ,task = traintask
                     ,resampling = rdesc
                     ,measures = acc
                     ,par.set = params
                     ,control = ctrl
                     ,show.info = T)

mytune$y #0.9014829
#Mapping in parallel: mode = socket; level = mlr.tuneParams; cpus = 8; elements = 10.
#[Tune]Result: booster=gbtree; max_depth=3; min_child_weight=9.58; subsample=0.793; colsample_bytree=0.796 : acc.test.mean=0.9156745

#set hyperparameters
lrn_tune <- setHyperPars(lrn,par.vals = mytune$x)

#train model
xgmodel <- train(learner = lrn_tune,task = traintask)

#predict model
xgpred <- predict(xgmodel,testtask)

confusionMatrix(xgpred$data$response,xgpred$data$truth)
#Accuracy : 0.9145

#stop parallelization
parallelStop()

########调参之后的模型#########
#second parameters
params2 <- list(
  booster = "gbtree",
  objective = "binary:logistic",#  "binary:logistic",reg:squarederror
  eta=0.3,#Learning rate 
  gamma=0,#minimum loss reduction
  max_depth=3,#maximum depth of a tree
  min_child_weight=9.58,#minimum sum of instance weight (hessian)
  subsample=0.793,#Sub-sample
  colsample_bytree=0.796,#Col-sample-by-tree
  lambda=1,
  alpha=0
)
#Second  model training
xgb2 <- xgb.train(
  params = params2
  ,data = dtrain
  ,nrounds = 17 #最佳迭代次数
  ,watchlist = list(val=dtest,train=dtrain)
  ,print_every_n = 10
  ,early_stop_round = 10
  ,maximize = F
  ,eval_metric = "error"
)

#model prediction
xgbpred2 <- predict(xgb2,dtest)
xgbpred2 <- ifelse(xgbpred2 > 0.5,1,0)

#confusion matrix
library(caret)
confusionMatrix(as.factor(xgbpred2), as.factor(ts_label))
#Accuracy - 89.81%
# 计算混淆矩阵
cm2 <- confusionMatrix(as.factor(xgbpred2), as.factor(ts_label))
# 从混淆矩阵中提取指标
accuracy2 <- cm2$overall['Accuracy']
precision2 <- cm2$byClass['Precision']
recall2 <- cm2$byClass['Recall']
f1_score2 <- cm2$byClass['F1']
# 打印结果
cat("Accuracy:", accuracy2, "\n")
cat("Precision:", precision2, "\n")
cat("Recall:", recall2, "\n")
cat("F1 Score:", f1_score2, "\n")

######SHAP#######
# To return the SHAP values and ranked features by mean|SHAP|
shap_values2 <- shap.values(xgb_model = xgb2, X_train = new_tr)
# The ranked features by mean |SHAP|
shap_values2$mean_shap_score
# To prepare the long-format data:
shap_long2 <- shap.prep(xgb_model = xgb2, X_train = new_tr)
# is the same as: using given shap_contrib
shap_long2 <- shap.prep(shap_contrib = shap_values2$shap_score, X_train = new_tr)
# **SHAP summary plot**
shap.plot.summary(shap_long)
# 取每个特征的SHAP值的绝对值的平均值作为该特征的重要性
shap.plot.summary(shap_long2, kind="bar")



#######LPA绘图########
library(readxl)
MASLPA4 <- read_excel("MASLPA4.xlsx")
MASLPA4$class<-as.factor(MASLPA4$class)
library(tidyr)
# 将宽格式数据转为长格式
data_long <- pivot_longer(MASLPA4, 
                          cols = c(mas1, mas2,mas3,mas4,mas5,mas6), 
                          names_to = "item", 
                          values_to = "score")
library(ggplot2)
library(Rmisc)
library(cowplot)
tgc1 <- summarySE(data_long, measurevar="score", groupvars=c("class","item"))

ggplot(tgc1, aes(x=item, y=score, colour=class,group=class)) +   
  geom_errorbar(aes(ymin=score-se, ymax=score+se), width=.1) +
  geom_line(size=2) +
  geom_point(size=2)+
  theme_bw()

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
        legend.text = element_text(family = 'serif'), #这里可以改字体为新罗马字体
        legend.position = c(.3,.92),
        legend.direction = "horizontal",
        axis.text = element_text(color = 'black',family = 'serif'),
        axis.title = element_text(family = 'serif',size = 18,color = 'black'))

