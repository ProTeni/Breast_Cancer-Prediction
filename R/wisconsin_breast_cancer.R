#The will be a supervised ML because there is a 'Target or label data'
#In addition to that, the target variables are either 
#'Benign' or 'Malignant'; this fits a classification problem. 
# A classification logistic regression will be ideal


#LOAD THE DATA. Already imported into the Folder.
getwd()  #confirm working directory
list.files() #to confirm the data set is in in the folder
data <- read.csv("winsconsin.csv")
                 

#PREVIEW the data
#previewing the first 5 rows of the data 

#To get a snapshot of the data statistical summary
summary(data)

#To view the rows and columns of the data
dim(data)

# To confirm the data types of each feature or column
str(data)

#find missing values
sum(is.na(data))

#there are 16 missing values. I'd need to know which feature(s)
colSums(is.na(data))
#there are 16 missing values under the Bare. Nuclei Column

#View the details in the Bare. Nuclei column
summary(data$Bare.nuclei)


#Find the mode of the values in Bare. Nuclei to better understand the frequency of the range of numbers
table(data$Bare.nuclei)

#This is a bimodal distribution with 2 dominant groups; 
#402 patients with scores of 1, and 132 with scores of 10. 
#using either 1 (the median) or the mean (3.454) either could work
#depending on what I'm optimizing my model for

#From the features interpretation medically; the higher the numbers of each feature, 
#the higher the chance of the cell being malignant.

# Hence, for the missing values, to populate them (rather than delete) would be a better option. 
# The question on what to populate with would either be the mean value of the median.

#Looking the the Bare Nuclei Data summary, the minimum value in the column is 1, the max is 10. 
#Altogether, there are 16 missing values. If  I used the mean of 
#3.545- I might inflate the normality (meaning increasing that algorithm's chance of being sensitive to malignant cases

#However, if I used the median, I might be making the algorithm more sensitive to benign cases.
#The objective of the model will be to have a high sensitivity score- simply because it is better
#the model has more false positives, than false negatives- which resultant of death.

#Due to the objective of increasing the algorithm's learning capacity to sensitivity (less false negatives),
#It'd be better if the model is exposed to more malignant cases to learn better. Hence, the mean will be used.

#and if i used the mean, I'm not sure how many of the number ones there are
#to know not to inflate (by using the mean) or ignore (by using 1)


#replace missing values in Bare.Nuclei with the mean
data$Bare.nuclei[is.na(data$Bare.nuclei)] <- round(mean(data$Bare.nuclei, na.rm=TRUE), 0)
table(data$Bare.nuclei)


#verify there are no more missing values
sum(is.na(data))

#NORMALIZE THE DATA
#"All features are measured on a uniform 1-10 scale, therefore normalization was not required."
summary(data)

#factorize the target label
?as.factor()
data$Class <- as.factor(data$Class)

table(data$Class)
summary(data$Class)


#GGPLOT
install.packages("ggplot2")
library(ggplot2)



ggplot(data, aes(x = Class, fill = Class)) +
  geom_bar()

ggplot(data, aes(x = Class, fill = Class)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..),
            vjust = -0.5, size = 4) +
  labs(title = "Class Distribution — Benign vs Malignant",
       x = "Tumour Class",
       y = "Number of Patients") +
  theme_minimal()


#The data has a class imbalance. Might favour more benighs because it has moreof that,
#except SMOTE- synthetic data is generated to balance and possibly
#surpass benigh- so the model is re exposed to malignant cases.
str(data)

#Split the Data
?sample
set.seed(42)
train_index <- sample(1:nrow(data), nrow(data) * 0.70, replace = FALSE)
nrow(data)

train_data <- data[train_index, ]
test_data <- data[-train_index, ]


dim(train_data)
dim(test_data)

?knn
#knn not available



#install class
install.packages("class")
library(class)

?knn

train_data[, -which(names(train_data) == "Class")]

train_features <- train_data[, -which(names(train_data) == "Class")]
test_features <- test_data[, -which(names(test_data) == "Class")]
train_labels <- train_data$Class

str(train_features)
str(test_features)
str(train_labels)

#Run KNN algorithm
knn_predictions <- knn(train = train_features, test =test_features, cl = train_labels, k = 21)

knn_predictions

#Compare the model's performace
table(knn_predictions, test_data$Class)

conf_matrix <- table(knn_predictions, test_data$Class)
conf_matrix

#In cancer cases, malignant are 1 (positive), while benighs are 0(negatie)
#More so the data will be such that rows are predited outcpmes, while columns are the actual


true_negative <- conf_matrix [1,1]
false_positve <- conf_matrix[2, 1]
false_negative <- conf_matrix[1,2]
true_positive <- conf_matrix[2, 2]

true_negative
true_positive
false_negative
false_positve



calculate_metrics <- function(true_positive, true_negative,
                              false_positve, false_negative)
  {
  accuracy <- (true_positive + true_negative)/(true_negative + true_positive
                                               +false_negative + false_positve)
  sensitivity <- true_positive / (true_positive + false_negative)
  specificity <- true_negative / (true_negative + false_positve)
  return(c(Accuracy = accuracy, Sensitivity = sensitivity, Specificity = specificity))
  }


calculate_metrics(true_positive, true_negative,
                  false_positve, false_negative)


#Hyper-tuning 1
knn_predcitions_1sthypertune <- knn(train = train_features,
                                    test= test_features, cl = train_labels,
                                    k = 5)
knn_predcitions_1sthypertune

#confusuon matrix
conf_matrix_1sthypertune <- table(knn_predcitions_1sthypertune,
                       test_data$Class)

conf_matrix_1sthypertune

true_negative_1h <- conf_matrix_1sthypertune [1,1]
false_positve_1h <- conf_matrix_1sthypertune[2, 1]
false_negative_1h <- conf_matrix_1sthypertune[1,2]
true_positive_1h <- conf_matrix_1sthypertune[2, 2]

true_negative_1h
true_positive_1h
false_negative_1h
false_positve_1h

#same results...no need evaluation...
#try lower clusters

knn_predcitions_1sthypertune <- knn(train = train_features,
                                    test= test_features, cl = train_labels,
                                    k = 5)
knn_predcitions_1sthypertune

#confusuon matrix
conf_matrix_1sthypertune <- table(knn_predcitions_1sthypertune,
                                  test_data$Class)

conf_matrix_1sthypertune

true_negative_1h <- conf_matrix_1sthypertune [1,1]
false_positve_1h <- conf_matrix_1sthypertune[2, 1]
false_negative_1h <- conf_matrix_1sthypertune[1,2]
true_positive_1h <- conf_matrix_1sthypertune[2, 2]

true_negative_1h
true_positive_1h
false_negative_1h
false_positve_1h



#Hyper-tuning 3
knn_predcitions_2ndhypertune <- knn(train = train_features,
                                    test= test_features, cl = train_labels, k = 3)


knn_predcitions_2ndhypertune

#confusuon matrix
conf_matrix_2ndhypertune <- table(knn_predcitions_2ndhypertune,
                                  test_data$Class)

conf_matrix_2ndhypertune

true_negative_2h <- conf_matrix_2ndhypertune [1,1]
false_positve_2h <- conf_matrix_2ndhypertune[2, 1]
false_negative_2h <- conf_matrix_2ndhypertune[1,2]
true_positive_2h <- conf_matrix_2ndhypertune[2, 2]

true_negative_2h
true_positive_2h
false_negative_2h
false_positve_2h




calculate_metrics_h2 <- function(true_positive_2h, true_negative_2h,
                                 false_positve_2h, false_negative_2h) {
  accuracy_2h <- (true_positive_2h + true_negative_2h) / (true_negative_2h + true_positive_2h + false_negative_2h + false_positve_2h)
  sensitivity_2h <- true_positive_2h / (true_positive_2h + false_negative_2h)
  specificity_2h <- true_negative_2h / (true_negative_2h + false_positve_2h)
  return(c(Accuracy_2h = accuracy_2h, Sensitivity_2h = sensitivity_2h, Specificity_2h = specificity_2h))
}


true_negative_2h <- conf_matrix_2ndhypertune[1,1]
false_positve_2h <- conf_matrix_2ndhypertune[2,1]
false_negative_2h <- conf_matrix_2ndhypertune[1,2]
true_positive_2h <- conf_matrix_2ndhypertune[2,2]

calculate_metrics_h2(true_positive_2h, true_negative_2h, false_positve_2h, false_negative_2h)
#seeing k= 3 is the best

calculate_metrics_h2(true_positive_2h,
                  true_negative_2h,
                  false_positve_2h,
                  false_negative_2h)



#futher hypertuning...K=1
#Hyper-tuning 3
knn_predcitions_3rdhypertune <- knn(train = train_features,
                                    test= test_features, cl = train_labels,
                                    k = 1)
knn_predcitions_3rdhypertune

#confusuon matrix
conf_matrix_3rdhypertune <- table(knn_predcitions_3rdhypertune,
                                  test_data$Class)

conf_matrix_3rdhypertune

true_negative_3h <- conf_matrix_3rdhypertune [1,1]
false_positve_3h <- conf_matrix_3rdhypertune[2, 1]
false_negative_3h <- conf_matrix_3rdhypertune[1,2]
true_positive_3h <- conf_matrix_3rdhypertune[2, 2]

true_negative_3h
true_positive_3h
false_negative_3h
false_positve_3h


#furher hypertuning...K=2
#Hyper-tuning 4
knn_predcitions_4thhypertune <- knn(train = train_features,
                                    test= test_features, cl = train_labels,
                                    k = 2)
knn_predcitions_4thhypertune

#confusuon matrix
conf_matrix_4thhypertune <- table(knn_predcitions_4thhypertune,
                                  test_data$Class)

conf_matrix_4thhypertune

true_negative_4h <- conf_matrix_4thhypertune [1,1]
false_positve_4h <- conf_matrix_4thhypertune[2, 1]
false_negative_4h <- conf_matrix_4thhypertune[1,2]
true_positive_4h <- conf_matrix_4thhypertune[2, 2]

true_negative_4h
true_positive_4h
false_negative_4h
false_positve_4h




calculate_metrics_4h <- function(true_positive_4h, true_negative_4h,
                                 false_positve_4h, false_negative_4h) {
  accuracy_4h <- (true_positive_4h + true_negative_4h) / (true_negative_4h + true_positive_4h + false_negative_4h + false_positve_4h)
  sensitivity_4h <- true_positive_4h / (true_positive_4h + false_negative_4h)
  specificity_4h <- true_negative_4h / (true_negative_4h + false_positve_4h)
  return(c(Accuracy_4h = accuracy_4h, Sensitivity_4h = sensitivity_4h, Specificity_4h = specificity_4h))
}

calculate_metrics_4h(true_positive_4h, true_negative_4h, false_positve_4h, false_negative_4h)
true_positive_4h
false_negative_4h
conf_matrix_4thhypertune


#Hyper-tuning 3
knn_predcitions_2ndhypertune <- knn(train = train_features,
                                    test= test_features, cl = train_labels,
                                    k = 3)
knn_predcitions_2ndhypertune

#confusuon matrix
conf_matrix_2ndhypertune <- table(knn_predcitions_2ndhypertune,
                                  test_data$Class)

conf_matrix_2ndhypertune

true_negative_2h <- conf_matrix_2ndhypertune [1,1]
false_positve_2h <- conf_matrix_2ndhypertune[2, 1]
false_negative_2h <- conf_matrix_2ndhypertune[1,2]
true_positive_2h <- conf_matrix_2ndhypertune[2, 2]

true_negative_2h
true_positive_2h
false_negative_2h
false_positve_2h




calculate_metrics_h2 <- function(true_positive_2h, true_negative_2h,
                                 false_positve_2h, false_negative_2h)
{
  accuracy_2h <- (true_positive_2h + true_negative_2h)/(true_negative_2h + true_positive_2h
                                                        +false_negative_2h + false_positve_2h)
  sensitivity_2h <- true_positive_2h / (true_positive_2h + false_negative_2h)
  specificity_2h <- true_negative_2h / (true_negative_2h + false_positve_2h)
  return(c(Accuracy_2h =accuracy_2h, Sensitivity_2h = sensitivity_2h, Specificity_2h = specificity_2h))
}

calculate_metrics_h2(true_positive_2h, true_negative_2h, false_positve_2h, false_negative_2h)


#seeing k= 2 is the best

??rpart

#Install Decsion Tree Pacage
install.packages("rpart")
library(rpart)

#decision tree because in medicine explainanbility matters.
#Decision tree is like a flow chart that answers...is x present?
#Yes, if no, the y....that help clinicians understand or explain how the algoritjm got to its answer
# without any black box....that thye amy not be able to trust
#it also put weights on the features that matter the most

dt_model <- rpart(Class  ~ ., data = train_data)

par(mar = c(1, 1, 1, 1))
plot(dt_model, uniform = TRUE, margin = 0.1)
text(dt_model, use.n = TRUE, cex = 0.7)

dt_predictions <- predict(dt_model, 
                          newdata = test_data, type = "class")

#confusuon matrix
conf_matrix_dt <- table(dt_predictions,
                                  test_data$Class)

conf_matrix_dt

true_negative_dt <- conf_matrix_dt [1,1]
false_positve_dt <- conf_matrix_dt[2, 1]
false_negative_dt <- conf_matrix_dt[1,2]
true_positive_dt <- conf_matrix_dt[2, 2]

true_negative_dt
true_positive_dt
false_negative_dt
false_positve_dt




calculate_metrics_dt <- function(tp, tn, fp, fn) {
  accuracy_dt <- (tp + tn) / (tn + tp + fn + fp)
  sensitivity_dt <- tp / (tp + fn)
  specificity_dt <- tn / (tn + fp)
  return(c(Accuracy_dt = accuracy_dt, Sensitivity_dt = sensitivity_dt, Specificity_dt = specificity_dt))
}

calculate_metrics_dt(true_positive_dt, true_negative_dt, false_positve_dt, false_negative_dt)





#Second plot on KNN
calculate_metrics(true_positive, true_negative, false_positve, false_negative)
calculate_metrics(true_positive_1h, true_negative_1h, false_positve_1h, false_negative_1h)
calculate_metrics(true_positive_2h, true_negative_2h, false_positve_2h, false_negative_2h)
calculate_metrics(true_positive_3h, true_negative_3h, false_positve_3h, false_negative_3h)
calculate_metrics(true_positive_4h, true_negative_4h, false_positve_4h, false_negative_4h)




knn_results <- data.frame(
  K = c(1, 2, 3, 5, 21),
  Accuracy = c(0.9524, 0.9571, 0.9571, 0.9524, 0.9381),
  Sensitivity = c(0.9219, 0.9375, 0.9219, 0.9219, 0.8750)
)

knn_summary <- data.frame(
  K = c(21, 5, 3, 1, 2),
  TP = c(56, 59, 59, 59, 60),
  TN = c(141, 141, 142, 141, 141),
  FP = c(5, 5, 4, 5, 5),
  FN = c(8, 5, 5, 5, 4),
  Accuracy = c(0.9381, 0.9524, 0.9571, 0.9524, 0.9571),
  Sensitivity = c(0.8750, 0.9219, 0.9219, 0.9219, 0.9375),
  Specificity = c(0.9658, 0.9658, 0.9726, 0.9658, 0.9658)
)

print(knn_summary)


#KNN across all K-Values and metrics
knn_results <- data.frame(
  K = c(1, 2, 3, 5, 21),
  Accuracy = c(0.9524, 0.9571, 0.9571, 0.9524, 0.9381),
  Sensitivity = c(0.9219, 0.9375, 0.9219, 0.9219, 0.8750),
  Specificity = c(0.9658, 0.9658, 0.9726, 0.9658, 0.9658)
)

knn_long <- pivot_longer(knn_results,
                         cols = c(Accuracy, Sensitivity, Specificity),
                         names_to = "Metric",
                         values_to = "Value")

ggplot(knn_long, aes(x = factor(K), y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(Value * 100, 1), "%")),
            position = position_dodge(width = 0.9),
            vjust = 0.5,
            hjust = 1.1,
            angle = 90,
            size = 2.5) +
  labs(title = "KNN Accuracy, Sensitivity and Specificity Across K Values",
       x = "K Value",
       y = "Score") +
  coord_cartesian(ylim = c(0.85, 1.02)) +
  scale_fill_manual(values = c("Accuracy" = "coral",
                               "Sensitivity" = "steelblue",
                               "Specificity" = "seagreen")) +
  theme_minimal()


#knn vs Dt
# Corrected knn_results
knn_results <- data.frame(
  K = c(1, 2, 3, 5, 21),
  Accuracy = c(0.9524, 0.9571, 0.9571, 0.9524, 0.9381),
  Sensitivity = c(0.9219, 0.9375, 0.9219, 0.9219, 0.8750),
  Specificity = c(0.9658, 0.9658, 0.9726, 0.9658, 0.9658)
)

# KNN K=2 vs Decision Tree comparison
model_comparison <- data.frame(
  Model = c("KNN K=2", "Decision Tree"),
  Accuracy = c(0.9571, 0.9333),
  Sensitivity = c(0.9375, 0.8906),
  Specificity = c(0.9658, 0.9521)
)

library(tidyr)
model_long <- pivot_longer(model_comparison,
                           cols = c(Accuracy, Sensitivity, Specificity),
                           names_to = "Metric",
                           values_to = "Value")

ggplot(model_long, aes(x = Metric, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(Value * 100, 1), "%")),
            position = position_dodge(width = 0.9),
            vjust = 0.5,
            hjust = 1.1,
            angle = 90,
            size = 3) +
  labs(title = "KNN K=2 vs Decision Tree — Performance Comparison",
       x = "Metric", y = "Score") +
  coord_cartesian(ylim = c(0.80, 1.02)) +
  scale_fill_manual(values = c("KNN K=2" = "steelblue",
                               "Decision Tree" = "coral")) +
  theme_minimal()
