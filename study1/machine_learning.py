from sklearn import datasets
from sklearn.model_selection import train_test_split
from sklearn.tree import DecisionTreeClassifier
from sklearn.neighbors import KNeighborsClassifier
from sklearn.ensemble import RandomForestClassifier
from sklearn.svm import SVC
from sklearn.linear_model import LogisticRegression
from sklearn.naive_bayes import GaussianNB
from sklearn.metrics import accuracy_score, precision_score, recall_score, f1_score, roc_auc_score, confusion_matrix

# 加载Iris数据集
import pandas as pd
# 指定 csv 文件路径
file_path1 = 'D:/Rdaima/休息羞耻_网络成瘾_机器学习/train_set.csv'
file_path2 = 'D:/Rdaima/休息羞耻_网络成瘾_机器学习/test_set.csv'
# 读取 csv 文件
train_set = pd.read_csv(file_path1)  # 读取train数据
test_set = pd.read_csv(file_path2)  # 读取test数据

# 划分数据集为训练集和测试集
# 提取标签列作为训练标记
y_train = train_set['label']
# 将剩余的数据作为训练数据
X_train = train_set.drop(columns=['label'])
# 提取标签列作为验证标记
y_test = test_set['label']
# 将剩余的数据作为验证数据
X_test = test_set.drop(columns=['label'])

# 初始化并训练决策树分类器
clf_dt = DecisionTreeClassifier(random_state=42)
clf_dt.fit(X_train, y_train)
y_pred_dt = clf_dt.predict(X_test)

# 初始化并训练KNN分类器
clf_knn = KNeighborsClassifier()
clf_knn.fit(X_train, y_train)
y_pred_knn = clf_knn.predict(X_test)

# 初始化并训练随机森林分类器
clf_rf = RandomForestClassifier(random_state=42)
clf_rf.fit(X_train, y_train)
y_pred_rf = clf_rf.predict(X_test)

# 初始化并训练SVM分类器
clf_svm = SVC(probability=True, random_state=42)
clf_svm.fit(X_train, y_train)
y_pred_svm = clf_svm.predict(X_test)

# 初始化并训练逻辑斯蒂回归分类器
clf_lr = LogisticRegression(random_state=42)
clf_lr.fit(X_train, y_train)
y_pred_lr = clf_lr.predict(X_test)

# 初始化并训练朴素贝叶斯分类器
clf_nb = GaussianNB()
clf_nb.fit(X_train, y_train)
y_pred_nb = clf_nb.predict(X_test)

# 计算准确率
acc_dt = accuracy_score(y_test, y_pred_dt)
acc_knn = accuracy_score(y_test, y_pred_knn)
acc_rf = accuracy_score(y_test, y_pred_rf)
acc_svm = accuracy_score(y_test, y_pred_svm)
acc_lr = accuracy_score(y_test, y_pred_lr)
acc_nb = accuracy_score(y_test, y_pred_nb)

print('Accuracy of Decision Tree Classifier:', acc_dt)
print('Accuracy of KNN Classifier:', acc_knn)
print('Accuracy of Random Forest Classifier:', acc_rf)
print('Accuracy of SVM Classifier:', acc_svm)
print('Accuracy of Logistic Regression Classifier:', acc_lr)
print('Accuracy of Naive Bayes Classifier:', acc_nb)
#计算precision
precision_dt = precision_score(y_test, y_pred_dt, average='weighted')
precision_knn = precision_score(y_test, y_pred_knn, average='weighted')
precision_rf = precision_score(y_test, y_pred_rf, average='weighted')
precision_svm = precision_score(y_test, y_pred_svm, average='weighted')
precision_lr = precision_score(y_test, y_pred_lr, average='weighted')
precision_nb = precision_score(y_test, y_pred_nb, average='weighted')

print('precision of Decision Tree Classifier:', precision_dt)
print('precision of KNN Classifier:', precision_knn)
print('precision of Random Forest Classifier:', precision_rf)
print('precision of SVM Classifier:', precision_svm)
print('precision of Logistic Regression Classifier:', precision_lr)
print('precision of Naive Bayes Classifier:', precision_nb)
#计算recall
recall_dt = recall_score(y_test, y_pred_dt, average='weighted')
recall_knn = recall_score(y_test, y_pred_knn, average='weighted')
recall_rf = recall_score(y_test, y_pred_rf, average='weighted')
recall_svm = recall_score(y_test, y_pred_svm, average='weighted')
recall_lr = recall_score(y_test, y_pred_lr, average='weighted')
recall_nb = recall_score(y_test, y_pred_nb, average='weighted')

print('recall of Decision Tree Classifier:', recall_dt)
print('recall of KNN Classifier:', recall_knn)
print('recall of Random Forest Classifier:', recall_rf)
print('recall of SVM Classifier:', recall_svm)
print('recall of Logistic Regression Classifier:', recall_lr)
print('recall of Naive Bayes Classifier:', recall_nb)
#计算f1
f1_dt = f1_score(y_test, y_pred_dt, average='weighted')
f1_knn = f1_score(y_test, y_pred_knn, average='weighted')
f1_rf = f1_score(y_test, y_pred_rf, average='weighted')
f1_svm = f1_score(y_test, y_pred_svm, average='weighted')
f1_lr = f1_score(y_test, y_pred_lr, average='weighted')
f1_nb = f1_score(y_test, y_pred_nb, average='weighted')

print('f1 of Decision Tree Classifier:', f1_dt)
print('f1 of KNN Classifier:', f1_knn)
print('f1 of Random Forest Classifier:', f1_rf)
print('f1 of SVM Classifier:', f1_svm)
print('f1 of Logistic Regression Classifier:', f1_lr)
print('f1 of Naive Bayes Classifier:', f1_nb)

# 计算混淆矩阵
conf_mat_dt = confusion_matrix(y_test, y_pred_dt)
conf_mat_knn = confusion_matrix(y_test, y_pred_knn)
conf_mat_rf = confusion_matrix(y_test, y_pred_rf)
conf_mat_svm = confusion_matrix(y_test, y_pred_svm)
conf_mat_lr = confusion_matrix(y_test, y_pred_lr)
conf_mat_nb = confusion_matrix(y_test, y_pred_nb)

print('The confusion matrix of decision tree classifier:\n', conf_mat_dt)
print('The confusion matrix of KNN classifier:\n', conf_mat_knn)
print('The confusion matrix of Random Forest classifier:\n', conf_mat_rf)
print('The confusion matrix of svm classifier:\n', conf_mat_svm)
print('The confusion matrix of logistic regression classifie:\n', conf_mat_lr)
print('The confusion matrix of naive bayes classifier:\n', conf_mat_nb)