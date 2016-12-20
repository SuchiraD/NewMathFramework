require(data.table)
require("sfsmisc")

result1 = fread("data/result 1 .csv", data.table = F, header = T)
result2 = fread("data/result 2 .csv", data.table = F, header = T)
result3 = fread("data/result 3 .csv", data.table = F, header = T)

#Read JAVA results
results1 = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/SEIRAnalysisthread1.csv", data.table = F, header = T)
results2 = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/SEIRAnalysisthread2.csv", data.table = F, header = T)
results3 = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/SEIRAnalysisthread3.csv", data.table = F, header = T)
results4 = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/SEIRAnalysisthread4.csv", data.table = F, header = T)
results5 = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/SEIRAnalysisthread5.csv", data.table = F, header = T)
results6 = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/SEIRAnalysisthread6.csv", data.table = F, header = T)
results1 <- as.data.frame(sapply(results1, as.numeric)) 
results2 <- as.data.frame(sapply(results2, as.numeric)) 
results3 <- as.data.frame(sapply(results3, as.numeric)) 
results4 <- as.data.frame(sapply(results4, as.numeric)) 
results5 <- as.data.frame(sapply(results5, as.numeric)) 
results6 <- as.data.frame(sapply(results6, as.numeric)) 

plot(results5$best.sh,results6$best.sh)

best.as = data.frame(day = c(1:46))
best.as$a1 = results1$best.a
best.as$a2 = results2$best.a
best.as$a3 = results3$best.a
best.as$a4 = results4$best.a
best.as$a5 = results5$best.a
best.as$a6 = results6$best.a

correlation = data.frame(cor(best.as))

results = results2
gammah = 0.5

sqError = function(dataframe) {
  error.sum = 0
  for(week in c(1:10)) {
    error.sum = error.sum + (currentMOH$cases[week] - gammah * (sum(results$best.eh[((week-1)*7+1):(week*7)])))^2
    if(is.na(error.sum)) {
      cat("Week is ", week)
    }
  }
  
  return (error.sum)
}

predictionForWeeks = function(results, maxWeeks) {
  prediction = data.frame(week = 1:maxWeeks)
  for(week in c(1:maxWeeks)) {
    prediction$cases[week] = gammah * (sum(results$best.eh[((week-1)*7+1):(week*7)]))
  }
  prediction
  return(prediction)
}

predicted = 0
calculateDengueDynamicsWeekly = function(day, sh0, eh0, ih0, rh0, index) {
  testData = data.frame(day = day, 
                        best.sh = sh0, 
                        best.eh = eh0, 
                        best.ih = ih0, 
                        best.rh = rh0, 
                        best.a = 0,
                        test[index,][7:ncol(test)])
  sparce_matrix_test <- sparse.model.matrix(best.a ~ .-1, data = testData)
  pred[index] <<- predict(model, sparce_matrix_test)
  a = pred[index]/10000

  sh0 = testData$best.sh
  eh0 = testData$best.eh
  ih0 = testData$best.ih
  rh0 = testData$best.rh

  dSh = -a*sh0
  dEh = a*sh0 - gammah*eh0
  dIh = gammah*eh0 - sigmah*ih0
  dRh = sigmah*ih0
  
  ShEhIhRh = as.integer(array(data = c((sh0+dSh), (eh0+dEh), (ih0+dIh), (rh0+dRh))))
  ShEhIhRh[ShEhIhRh<0] = mean(SEIR$eh)
  
  return(ShEhIhRh)
}


calculateDengueDynamicsWeeklyWithPredVals = function(index) {
  a = pred[index]/10000
  
  sh0 = test$best.sh[index]
  eh0 = test$best.eh[index]
  ih0 = test$best.ih[index]
  rh0 = test$best.rh[index]
  
  dSh = -a*sh0
  dEh = a*sh0 - gammah*eh0
  dIh = gammah*eh0 - sigmah*ih0
  dRh = sigmah*ih0
  
  ShEhIhRh = as.integer(array(data = c((sh0+dSh), (eh0+dEh), (ih0+dIh), (rh0+dRh))))
  ShEhIhRh[ShEhIhRh<0] = mean(SEIR$eh)
  
  return(ShEhIhRh)
}

calculateDengueDynamicsWeekly10 = function(day, sh0, eh0, ih0, rh0, index) {
  if(index%%4==0) {
    sh0 = test$best.sh[index]
    eh0 = test$best.eh[index]
    ih0 = test$best.ih[index]
    rh0 = test$best.rh[index]
  }
  
  testData = data.frame(day = day, 
                        best.sh = sh0, 
                        best.eh = eh0, 
                        best.ih = ih0, 
                        best.rh = rh0, 
                        best.a = 0,
                        test[index,][7:ncol(test)])
  sparce_matrix_test <- sparse.model.matrix(best.a ~ .-1, data = testData)
  pred[index] <<- predict(model, sparce_matrix_test)
  a = pred[index]/10000
  
  sh0 = testData$best.sh
  eh0 = testData$best.eh
  ih0 = testData$best.ih
  rh0 = testData$best.rh
  
  dSh = -a*sh0
  dEh = a*sh0 - gammah*eh0
  dIh = gammah*eh0 - sigmah*ih0
  dRh = sigmah*ih0
  
  ShEhIhRh = as.integer(array(data = c((sh0+dSh), (eh0+dEh), (ih0+dIh), (rh0+dRh))))
  ShEhIhRh[ShEhIhRh<0] = mean(SEIR$eh)
  
  return(ShEhIhRh)
}
########################################
resultLocation = "/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/Thu Dec 15 13:43:39 IST 2016"
resultLocation = "/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/Thu Dec 15 14:18:53 IST 2016"
testLocation = "/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/Thu Dec 15 14:18:53 IST 2016"

resultLocation = "/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/Sun Dec 11 18:23:33 IST 2016"
resultLocation = "/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/Sun Dec 11 18:24:26 IST 2016"
testLocation = "/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/Sun Dec 11 18:24:34 IST 2016"

results1 = fread(paste(resultLocation,"SEIRAnalysis2012.csv",sep = "/"), data.table = F, header = T, skip = 2)
results2 = fread(paste(resultLocation,"SEIRAnalysis2013.csv",sep = "/"), data.table = F, header = T)
results3 = fread(paste(resultLocation,"SEIRAnalysisthread3.csv",sep = "/"), data.table = F, header = T)
results4 = fread(paste(resultLocation,"SEIRAnalysisthread4.csv",sep = "/"), data.table = F, header = T)
results5 = fread(paste(resultLocation,"SEIRAnalysisthread5.csv",sep = "/"), data.table = F, header = T)
results6 = fread(paste(resultLocation,"SEIRAnalysisthread6.csv",sep = "/"), data.table = F, header = T)
results7 = fread(paste(resultLocation,"SEIRAnalysisthread7.csv",sep = "/"), data.table = F, header = T)
results8 = fread(paste(resultLocation,"SEIRAnalysisthread8.csv",sep = "/"), data.table = F, header = T)
results9 = fread(paste(resultLocation,"SEIRAnalysisthread9.csv",sep = "/"), data.table = F, header = T)
results10 = fread(paste(resultLocation,"SEIRAnalysisthread10.csv",sep = "/"), data.table = F, header = T)
test = fread(paste(testLocation,"SEIRAnalysisthread1.csv",sep = "/"), data.table = F, header = T)
test = fread(paste(testLocation,"SEIRAnalysis2014.csv",sep = "/"), data.table = F, header = T)

results1 <- as.data.frame(sapply(results1, as.numeric))
results2 <- as.data.frame(sapply(results2, as.numeric))
results3 <- as.data.frame(sapply(results3, as.numeric))
results4 <- as.data.frame(sapply(results4, as.numeric))
results5 <- as.data.frame(sapply(results5, as.numeric))
results6 <- as.data.frame(sapply(results6, as.numeric))
results7 <- as.data.frame(sapply(results7, as.numeric))
results8 <- as.data.frame(sapply(results8, as.numeric))
results9 <- as.data.frame(sapply(results9, as.numeric))
results10 <- as.data.frame(sapply(results10, as.numeric))
test <- as.data.frame(sapply(test, as.numeric))

convert = function(results1) {
  for(index in 1:nrow(results1)) {
    if(results1$day[index]%%7 == 0) {
      tempShEhIhRh = results1[index,][2:5]
    }
    if(results1$day[index]%%7 == 1) {
      results1[index,][2:5] = tempShEhIhRh;
    }
    else if(results1$day[index]%%7 >= 0 && index != 1) {
      results1[index,][2:5] = results1[index-1,][2:5];
    }
  }
  
  return(results1)
}

circularIndex = function(index, lag=1, array.size) {
  newIndex = index-lag
  
  return(ifelse(newIndex<=0, (array.size-abs(newIndex)), newIndex))
}

replaceInitValues = function(results1) {
#  tempResults = results1
#  for(index in 2:nrow(results1)) {
#    results1[index,][2:5] = tempResults[(index-1),][2:5];
#  }
#  results1 = results1[2:nrow(results1),]
  
  #results1$temperature[3:NROW(results1)] = temperature[results1$day[2:length(results1)]-1]
  
  averageCases = as.integer(mean(currentMOH$cases))
  results1$lastWeekCases = c(averageCases, averageCases, currentMOH$cases[1:50])
  #results1$lastWeekCases[1:2] = as.integer(mean(results1$lastWeekCases[3:NROW(results1)]))
  
  #results1$temperature[1:2] = as.integer(mean(results1$temperature[3:NROW(results1)]))
  #results1$rainfall[2:NROW(results1)] = rainFall[1:(NROW(results1)-1)]
  #results1$partialDiff = (gammah-1)*results1$best.eh/results1$best.sh
  
  results1$tempLag1 = c(temperature[51:52], temperature[1:50])
  results1$tempLag2 = c(temperature[50:52],temperature[1:49])
  results1$tempLag3 = c(temperature[49:52],temperature[1:48])
  results1$tempLag4 = c(temperature[48:52],temperature[1:47])
  results1$tempLag5 = c(temperature[47:52],temperature[1:46])
  results1$tempLag6 = c(temperature[46:52],temperature[1:45])
  results1$tempLag7 = c(temperature[45:52],temperature[1:44])
  results1$tempLag8 = c(temperature[44:52],temperature[1:43])
  results1$tempLag9 = c(temperature[43:52],temperature[1:42])
  results1$tempLag10 = c(temperature[42:52],temperature[1:41])
  results1$tempLag11 = c(temperature[41:52],temperature[1:40])
  results1$tempLag12 = c(temperature[40:52],temperature[1:39])
  
  results1$mobilityLag1 = c(mobility$mobility[51:52], mobility$mobility[1:50])
  results1$mobilityLag2 = c(mobility$mobility[50:52], mobility$mobility[1:49])
  results1$mobilityLag3 = c(mobility$mobility[49:52], mobility$mobility[1:48])
  results1$mobilityLag4 = c(mobility$mobility[48:52], mobility$mobility[1:47])
  results1$mobilityLag5 = c(mobility$mobility[47:52], mobility$mobility[1:46])
  results1$mobilityLag6 = c(mobility$mobility[46:52], mobility$mobility[1:45])
  results1$mobilityLag7 = c(mobility$mobility[45:52], mobility$mobility[1:44])
  
  return(results1)
}

currentMOH$cases= melt(dengue2012[8,][3:54])$value
currentMOH$cases= melt(dengue2013[181,][3:54])$value
currentMOH$cases = currentMOH$cases/0.02
results1 = replaceInitValues(results1)
results2 = replaceInitValues(results2)
results3 = replaceInitValues(results3)
results4 = replaceInitValues(results4)
results5 = replaceInitValues(results5)
results6 = replaceInitValues(results6)
results7 = replaceInitValues(results7)
results8 = replaceInitValues(results8)
results9 = replaceInitValues(results9)
results10 = replaceInitValues(results10)

currentMOH$cases= melt(dengue2014[181,][3:54])$value
currentMOH$cases = currentMOH$cases/0.02
test$temperature[1:NROW(test)] = temperature[test$day[2:length(test)]-1]
results1$lastWeekCases[2:NROW(results1)] = currentMOH$cases[results1$day]
results1$temperature[1] = as.integer(mean(results1$temperature[2:NROW(results1)]))
results1$lastWeekCases[1] = as.integer(mean(results1$lastWeekCases[2:NROW(results1)]))
test = replaceInitValues(test)

results = merge(results1, results2, all = T)
results = merge(results, results3, all = T)
results = merge(results, results4, all = T)
results = merge(results, results5, all = T)
results = merge(results, results6, all = T)
results = merge(results, results7, all = T)
results = merge(results, results8, all = T)
results = merge(results, results9, all = T)
results = merge(results, results10, all = T)

results = merge(results, results1, all = T)
results = merge(results, results2, all = T)
results = merge(results, results3, all = T)
results = merge(results, results4, all = T)
results = merge(results, results5, all = T)
results = merge(results, results6, all = T)
results = merge(results, results7, all = T)
results = merge(results, results8, all = T)
results = merge(results, results9, all = T)
results = merge(results, results10, all = T)

## Remove sh column
results = data.frame(results[1], results[3:ncol(results)])
test = data.frame(test[1],test[2:ncol(test)])

predA = pred/10000
#345835,885,10026,210526
#266931,885,61452,238004
#257248,885,32645,276494
#145212,2600,6089,413305
#303962,885,75185,187240
tempSEIR = array(test[1,][2:5])
SEIR = data.frame(sh = tempSEIR[1], eh = tempSEIR[2], ih = tempSEIR[3], rh = tempSEIR[4])
for(week in 0:(length(predA)-1)) {
  #tempSEIR = calculateDengueDynamicsWeekly(week,sh0 = tempSEIR[1], eh0 = tempSEIR[2], ih0 = tempSEIR[3], rh0 = tempSEIR[4], index = (week+1))
  tempSEIR = calculateDengueDynamicsWeekly10(week,sh0 = tempSEIR[1], eh0 = tempSEIR[2], ih0 = tempSEIR[3], rh0 = tempSEIR[4], index = (week+1))
  #tempSEIR = calculateDengueDynamicsWeeklyWithPredVals(index = (week+1))
  #SEIR = data.frame(sh = tempSEIR[1], eh = tempSEIR[2], ih = tempSEIR[3], rh = tempSEIR[4])  
  SEIR[week+2, ] = tempSEIR
}
RMSLE = rmsle(predicted = pred, actual = actual)
RMSLE

maxs <- apply(SEIR, 2, max) 
mins <- apply(SEIR, 2, min)
SEIR <- as.data.frame(scale(SEIR, center = mins, scale = maxs - mins))

maxs <- apply(test, 2, max) 
mins <- apply(test, 2, min)
test <- as.data.frame(scale(test, center = mins, scale = maxs - mins))

plot(test$day, SEIR$eh[test$day+1], xlab = "Week", ylab = "Eh",type = "l", col = 2, main = paste("Dengue 2014 from Dengue 2012 and Dengue 2013 : ", dengue2014[area2013_2014,][2]))
lines(test$day, test$best.eh, type = "l", col = 1)
legend("topright",col=c(2,1),lty=1,legend=c("Predicted","Actual"))


#All in 3 files
resultLocation = "/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/Dehiwala-Tue Dec 20 06:53:12 IST 2016"
testLocation = resultLocation

results1 = fread(paste(resultLocation,"SEIRAnalysis2012.csv",sep = "/"), data.table = F, header = T)
results2 = fread(paste(resultLocation,"SEIRAnalysis2013.csv",sep = "/"), data.table = F, header = T)
results3 = fread(paste(resultLocation,"SEIRAnalysis2014.csv",sep = "/"), data.table = F, header = T)
test = fread(paste(testLocation,"SEIRAnalysis2014TEST.csv",sep = "/"), data.table = F, header = T)

results1 <- as.data.frame(sapply(results1, as.numeric))
results2 <- as.data.frame(sapply(results2, as.numeric))
results3 <- as.data.frame(sapply(results3, as.numeric))
test <- as.data.frame(sapply(test, as.numeric))

dehiwala2012 = 1
dehiwala2013_2014 = 55

colombo2012 = 8
colombo2013_2014 = 181

area2012 = dehiwala2012
area2013_2014 = dehiwala2013_2014

currentMOH$cases= melt(dengue2012[area2012,][3:54])$value
currentMOH$cases = currentMOH$cases/0.02
results1 = replaceInitValues(results1)
currentMOH$cases= melt(dengue2013[area2013_2014,][3:54])$value
currentMOH$cases = currentMOH$cases/0.02
results2 = replaceInitValues(results2)
currentMOH$cases= melt(dengue2014[area2013_2014,][3:54])$value
currentMOH$cases = currentMOH$cases/0.02
results3 = replaceInitValues(results3)
test = replaceInitValues(test)

results = merge(results1, results2, all = T)
results = merge(results, results3, all = T)
