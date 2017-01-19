library(Matrix)
library(Metrics)
library(xgboost)
require(Ckmeans.1d.dp)

#sparce_matrix_train <- sparse.model.matrix(Demanda_uni_equil ~ .-1, data = week789dataLess2000)
#model <- xgb.cv(data = sparce_matrix_train, label = week789dataLess2000[, "Demanda_uni_equil"], nfold = 4, max.depth = 23, eta = 0.06, nthread = 2, nround = 300, objective = "reg:linear", early.stop.round = 10, maximize = FALSE)
#model <- xgboost(data = sparce_matrix_train, label = week789dataLess2000[, "Demanda_uni_equil"], nfold = 4, max.depth = 23, eta = 0.05, nthread = 2, nround = 63, objective = "reg:linear", maximize = FALSE)

# XGBoost for yearly data
results$best.a = results$best.a*10
test$best.a = test$best.a*10

results$best.a = results$best.a/10000
test$best.a = test$best.a/10000

#Normalize training data
maxs <- apply(results, 2, max) 
mins <- apply(results, 2, min)
scaledResults <- as.data.frame(scale(results, center = mins, scale = maxs - mins))
scaledResults$best.a <- results$best.a

#Normalize Test data
testMaxs <- apply(test, 2, max) 
testMins <- apply(test, 2, min)
scaledTest <- as.data.frame(scale(test, center = testMins, scale = testMaxs - testMins))
scaledTest$best.a <- test$best.a

## Assign normalized data
toPred = 1
prev = toPred - 1
data = scaledResults[1:(nrow(scaledResults)-toPred),]
testData = scaledTest
label = scaledResults$best.a[1:(nrow(scaledResults)-toPred)]
actual = test$best.a
days = test$day

testData = results[(nrow(results)-prev):nrow(results),]
actual = results$best.a[(nrow(results)-prev):nrow(results)]

# XGBoost for yearly data
results$best.a = results$best.a/rounder
test$best.a = test$best.a/rounder

toPred = 0
prev = toPred - 0
data = results[1:(nrow(results)-toPred),]
data = data.frame(data[1:6], data[9:length(data)])
data = data.frame(data[1:6])
data = data.matrix(data)
data <- data[,-6]
#data <- data[,-7]
testData = test
testData = data.frame(test[1:6], test[9:length(test)])
testData = data.frame(test[1:6])
testData = data.matrix(testData)
testData <- testData[,-6]
#testData <- testData[,-7]
label = results$best.a[1:(nrow(results)-toPred)]
actual = test$best.a
days = test$day

sparce_matrix_train <- sparse.model.matrix(best.a ~ .-1, data = data)
eta = 0.01
nround = 100000
max.depth = 10
model <- xgb.cv(data = sparce_matrix_train, label = label, nfold = 3, max.depth = max.depth, eta = eta, nthread = 4, nround = nround, objective = "reg:linear", early.stop.round = 10, maximize = FALSE)

nround = 2000
model <- xgboost(data = sparce_matrix_train, label = label, nfold = 3, max.depth = max.depth, eta = eta, nthread = 5, nround = nround, objective = "reg:linear", maximize = FALSE)
model <- xgboost(data = data, label = label, nfold = 3, max.depth = max.depth, eta = eta, nthread = 5, nround = nround, objective = "reg:linear", maximize = FALSE)

area = 'Maharagama'
testData = data.frame(test[test$moh_name==area,][1:6], test[test$moh_name==area,][9:length(test)])
testData = data.frame(test[test$moh_name==area,][1:6])
actual = testData$best.a
days = testData$day
testData = data.matrix(testData)
testData <- testData[,-6]
sparce_matrix_test <- sparse.model.matrix(best.a ~ .-1, data = testData)
pred = predict(model, sparce_matrix_test)
pred = predict(model, testData)
mse(predicted = pred, actual = actual)
rmsle(predicted = pred, actual = actual)

## Denormalize
pred <- (pred)*(max(actual)-min(actual))+min(actual)

act = data.frame(actual = actual, pred = pred)

importance_matrix <- xgb.importance(sparce_matrix_train@Dimnames[[2]], model = model)
importance_matrix <- xgb.importance(feature_names = colnames(inputs), model = model)
xgb.plot.importance(importance_matrix)

days = c(0:51)
plot(days, actual, xlab = "Week", ylab = "a", type = "o", pch = 1, col = "1", main=paste("Predicted parameter 'a': For ", area," 2014 from 2012 and 2013: RMSL=", round(rmsle(predicted = pred, actual = actual), 6)), 
     sub = "Training is combined of many MOH area/s and all the dependencies")
lines(days, pred, xlab = "Day", ylab = "a", type = "o", pch = 2, col = "2")
legend("topright",col=c(2,1),lty=1,legend=c("Predicted","Actual"))

ggplot.data = data.frame(week = days, predicted = pred, actual = actual)
dmelt = melt(ggplot.data, id = "week")
test.rmsle = rmsle(predicted = pred, actual = actual)
title = paste("Parameter 'a' for 2014_Prediction by XGBoost - ", area, " RMSLE = ", round(test.rmsle, digits = 5))

ggplot(data = dmelt, 
       aes(x = week, y = value, color = variable, shape=variable)) +
  xlab("Week") +
  ylab("Incidences") +
  #theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  geom_line() +
  geom_point()+
  ggtitle(title, subtitle = "Model Trained by 2012 data and 4th, 5th, 6th lag of cases with 12 temperature lags and 7 mobility lags") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))



days = c(0:51)
daysRange = 1:52
daysRange = 53:104
daysRange = 105:156
daysRange = 157:208
plot(days, actual[daysRange], xlab = "Week", ylab = "a", type = "o", col = "1", main=paste("Predicted parameter 'a': For 2014 from 2012 and 2013: RMSL=", rmsle(predicted = pred[daysRange], actual = actual[daysRange]), " :  ", dengue2014[area2013_2014,][2]))
lines(days, pred[daysRange], xlab = "Day", ylab = "a", type = "o", col = "2")




area = 'Dehiwala'
area = 'Piliyandala'
area = 'Homagama'
area = 'Boralesgamuwa'
area = 'Nugegoda'
area = 'Boralesgamuwa'
area = 'Boralesgamuwa'
area = 'Kaduwela'
area = 'Kollonnawa'
area = 'Maharagama'
area = 'MC - Colombo'
area = 'Moratuwa'
area = 'MC-Galle'
area = 'Panadura'

test = test[order(test$day),]
inputs = results[1:(nrow(results)-toPred),]
inputs = data.frame(inputs[1:6], inputs[9:length(inputs)])
inputs = data.matrix(inputs)
inputs <- inputs[,-6]
label = results$best.a[1:(nrow(results)-toPred)]

nround = 2000
model <- xgb.cv(data = inputs, label = label, nfold = 2, max.depth = max.depth, eta = eta, nthread = 5, nround = nround, objective = "reg:linear", early.stop.round = 10, maximize = FALSE, verbose = 0)
nround = 1500
model <- xgboost(data = inputs, label = label, nfold = 1, max.depth = max.depth, eta = eta, nthread = 5, nround = nround, objective = "reg:linear", maximize = FALSE)

testData = data.frame(test[test$moh_name==area,][1:6], test[test$moh_name==area,][9:length(test)])
actual = testData$best.a
days = testData$day
testData = data.matrix(testData)
testData <- testData[,-6]
pred = predict(model, testData)
mse(predicted = pred, actual = actual)
rmsle(predicted = pred, actual = actual)

# R Squared
R2 <- 1 - (sum((actual-pred )^2)/sum((actual-mean(actual))^2))
R2
