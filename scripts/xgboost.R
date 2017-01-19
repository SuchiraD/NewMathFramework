library(Matrix)
library(Metrics)
library(xgboost)
require(Ckmeans.1d.dp)

## Plot importance matrix graph
importance_matrix <- xgb.importance(sparce_matrix_train@Dimnames[[2]], model = model)
importance_matrix <- xgb.importance(feature_names = colnames(inputs), model = model)
xgb.plot.importance(importance_matrix)

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
