setwd("C:/Users/Vaibhavi/Documents/Spring 17/BDA/Project")
getwd()
input_data <- read.csv("voice.csv")

#dividing data into training and testing data
data_size <- floor(0.70 *nrow(input_data))
set.seed(123)
train_index = sample(seq_len(nrow(input_data)), size = data_size)

training_data = input_data[train_index, ]
testing_data = input_data[-train_index, ]

install.packages("caret")
library(caret)

#applying one R classifier
install.packages("RWeka")
library(RWeka)

one_r_classifier <- OneR(label ~., training_data)
summary(one_r_classifier)

one_r_prediction = predict(one_r_classifier, newdata = testing_data, type = 'prob')
summary(one_r_prediction)

install.packages("ROCR")
library(ROCR)
p <- prediction(one_r_prediction[,2], testing_data$label)
prf <- performance(p, measure = "tpr", x.measure = "fpr")
plot(prf, col="blue")

#applying J48 classifier
j48_classifier <- J48(label~., training_data)
j48_predict <- predict(j48_classifier, newdata = testing_data)
j48_cf <- confusionMatrix(j48_predict, testing_data$label)
j48_cf

#applying cart algorithm
install.packages("rpart")
library(rpart)

cart_classifier = rpart(label ~ . , data = training_data, method = "class")
summary(cart_classifier)
install.packages("rpart.plot")
library(rpart.plot)
prp(cart_classifier)

cart_prediction = matrix(predict(cart_classifier, newdata = testing_data, type = "class"))
summary(cart_prediction)

cart_cf <- confusionMatrix(cart_prediction, (testing_data$label))
cart_cf

plot_predictor<- predict(cart_classifier, newdata = testing_data, type = 'prob')
pred <- prediction(plot_predictor[,2], (testing_data$label))
prf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(prf, col="red")

#applying naive bayes algorithm
install.packages("e1071")
library(e1071)

naive_model <- naiveBayes(label ~., data = training_data)
naive_predict <- predict(naive_model, newdata = testing_data)
naive_cf <- confusionMatrix(naive_predict, testing_data$label)
naive_cf


#applying random forest
install.packages("randomForest")
library(randomForest)
random_forest_classifier <- randomForest(label~.,training_data, ntrees = 50, importance = TRUE)
plot(random_forest_classifier)

cf_predictor = predict(random_forest_classifier, newdata = testing_data)

cf <- confusionMatrix(cf_predictor, (testing_data$label))
cf

plot_predictor<- predict(random_forest_classifier, newdata = testing_data, type = 'prob')
pred <- prediction(plot_predictor[,2], (testing_data$label))
prf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(prf, col="magenta")

varImpPlot(random_forest_classifier,sort = T, n.var = 10, type = 1, col="red")
