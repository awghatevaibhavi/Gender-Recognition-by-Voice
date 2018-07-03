setwd("C:/Users/Vaibhavi/Documents/Spring 17/BDA/Project")
getwd()
input_data <- read.csv("voice.csv")

data_to_be_processed <- input_data[,-21]

install.packages("caret")
library(caret)

pre_processed_data <- preProcess(data_to_be_processed, method = "pca")
pca_prediction <- predict(pre_processed_data, data_to_be_processed)

data_to_be_modeled <- cbind(pca_prediction, label=as.factor(input_data[,21]))
data_size <- floor(0.70 *nrow(data_to_be_modeled))
set.seed(123)
train_index = sample(seq_len(nrow(data_to_be_modeled)), size = data_size)

training_data = data_to_be_modeled[train_index, ]
testing_data = data_to_be_modeled[-train_index, ]

#applying one R classifier
install.packages("RWeka")
library(RWeka)

one_r_model <- OneR(label ~., training_data)
summary(one_r_model)

one_r_predict <- predict(one_r_model, newdata = testing_data)
(one_r_predict)

#applying J48 algorithm
j48_model <- J48(label~., training_data)
j48_predict <- predict(j48_model, newdata = testing_data)

j48_cf <- confusionMatrix(j48_predict, testing_data$label)
j48_cf

#applying CART algorithm
install.packages("rpart")
library(rpart)

cart_model = rpart(label ~ . , data = training_data, method = "class")
summary(cart_model)
install.packages("rpart.plot")
library(rpart.plot)
prp(cart_model)

cart_predict <- matrix(predict(cart_model, newdata = testing_data, type = "class"))
cart_cf <- confusionMatrix(cart_predict, (testing_data$label))
cart_cf

#applying naive bayes algorithm
install.packages("e1071")
library(e1071)

naive_model <- naiveBayes(label ~., data = training_data)
naive_predict <- predict(naive_model, newdata = testing_data)
naive_cf <- confusionMatrix(naive_predict, testing_data$label)
naive_cf

#applying random forest algorithm
install.packages("randomForest")
library(randomForest)
random_forest_model <- randomForest(label~.,training_data, ntrees = 50, importance = TRUE)
plot(random_forest_model)

rf_predict = predict(random_forest_model, newdata = testing_data)

rf_cf <- confusionMatrix(rf_predict, (testing_data$label))
rf_cf

