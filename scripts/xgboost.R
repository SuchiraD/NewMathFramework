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
data = data.matrix(data)
data <- data[,-6]
#data <- data[,-7]
testData = test
testData = data.frame(test[1:6], test[9:length(test)])
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

sparce_matrix_test <- sparse.model.matrix(best.a ~ .-1, data = testData)
pred = predict(model, sparce_matrix_test)
pred = predict(model, testData)
mse(predicted = pred, actual = actual)
rmsle(predicted = pred, actual = actual)

## Denormalize
pred <- (pred)*(max(actual)-min(actual))+min(actual)

act = data.frame(actual = actual, pred = pred)

importance_matrix <- xgb.importance(sparce_matrix_train@Dimnames[[2]], model = model)
xgb.plot.importance(importance_matrix)

days = c(0:51)
plot(days, actual[1:52], xlab = "Week", ylab = "a", type = "l", col = "1", main=paste("Predicted parameter 'a': For 2014 from 2012 and 2013: RMSL=", rmsle(predicted = pred, actual = actual), " :  ", dengue2014[area2013_2014,][2]))
lines(days, pred[1:52], xlab = "Day", ylab = "a", type = "l", col = "2")
legend("topright",col=c(2,1),lty=1,legend=c("Predicted","Actual"))


actual = as.data.frame(actual)
maxs <- apply(actual, 2, max) 
mins <- apply(actual, 2, min)
actual <- as.data.frame(scale(actual, center = mins, scale = maxs - mins))

pred = as.data.frame(pred)
maxs <- apply(pred, 2, max) 
mins <- apply(pred, 2, min)
pred <- as.data.frame(scale(pred, center = mins, scale = maxs - mins))

plot(days, actual$actual, xlab = "Day", ylab = "a", type = "l", col = "1")
lines(days, pred$pred, xlab = "", ylab = "Day", type = "l", col = "2")



# XGBoost or two years data
sparce_matrix_train <- sparse.model.matrix(cases ~ .-1, data = as.data.frame(currentMOH[1:78, ]))
eta = 0.01
nround = 100000
max.depth = 7
model <- xgb.cv(data = sparce_matrix_train, label = currentMOH[, "cases"][1:78], nfold = 4, max.depth = max.depth, eta = eta, nthread = 4, nround = nround, objective = "reg:linear", early.stop.round = 50, maximize = FALSE)

nround = 490
model <- xgboost(data = sparce_matrix_train, label = currentMOH[, "cases"][1:78], nfold = 4, max.depth = max.depth, eta = eta, nthread = 4, nround = nround, objective = "reg:linear", maximize = FALSE)

sparce_matrix_test <- sparse.model.matrix(cases ~ .-1, data = as.data.frame(currentMOH[79:104, ]))
pred = predict(model, sparce_matrix_test)
mse(predicted = pred, actual = currentMOH[, "cases"][79:104])

importance_matrix <- xgb.importance(sparce_matrix_train@Dimnames[[2]], model = model)
xgb.plot.importance(importance_matrix)

data = data.frame(week = c(79:104), predicted = as.numeric(pred), cleanedCases = as.numeric(currentMOH[79:104,]$cases))
dmelt = melt(data, id = "week")
title = paste("Dengue Incidences 2013_2014 Prediction By XGBoost - Dehiwala MOH  MSE = ", round(mse(predicted = pred, actual = currentMOH[, "cases"][79:104])
, digits = 2))

ggplot(data = dmelt, 
       aes(x = week, y = value, color = variable)) +
  xlab("Week") +
  ylab("Incidences") +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  geom_line() +
  ggtitle(title)

# Model Fit
pred = predict(model, sparce_matrix_train)
data = data.frame(week = c(1:78), predicted = as.numeric(pred), cleanedCases = as.numeric(currentMOH[1:78,]$cases))
dmelt = melt(data, id = "week")
title = paste("Dengue Incidences 2013_2014 XGBoost fit Dehiwala MOH  MSE = ", round(mse(predicted = pred, actual = currentMOH[, "cases"][1:78])
                                                                                            , digits = 2))

ggplot(data = dmelt, 
       aes(x = week, y = value, color = variable)) +
  xlab("Week") +
  ylab("Incidences") +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  geom_line() +
  ggtitle(title)
