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
                        #test[index,][7],
                        tempTest[index,][9:ncol(tempTest)])
  sparce_matrix_test <- sparse.model.matrix(best.a ~ .-1, data = testData)
  pred[index] <<- predict(model, sparce_matrix_test)
  if(pred[index]<0) {
    pred[index] <<- mean(results$best.a)
  }
  a = pred[index]

  
  
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
  a = pred[index]/rounder
  
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

calculateDengueDynamicsWeekly10 = function(day, sh0, eh0, ih0, rh0, index, range) {
  if(index!=0 && index%%range==0) {
    sh0 = tempTest$best.sh[index]
    eh0 = tempTest$best.eh[index]
    ih0 = tempTest$best.ih[index]
    rh0 = tempTest$best.rh[index]
  }
  
  testData = data.frame(day = day, 
                        best.sh = sh0, 
                        best.eh = eh0, 
                        best.ih = ih0, 
                        best.rh = rh0, 
                        best.a = 0,
                        #test[index,][7],
                        tempTest[index,][9:ncol(tempTest)])
  sparce_matrix_test <- sparse.model.matrix(best.a ~ .-1, data = testData)
  pred[index] <<- predict(model, sparce_matrix_test)
  if(pred[index]<0) {
    pred[index] <<- mean(results$best.a)
  }
  a = pred[index]
  
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


currentMOH$cases= melt(dengue2012[8,][3:54])$value
currentMOH$cases= melt(dengue2013[181,][3:54])$value
currentMOH$cases = currentMOH$cases/reportingRate
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
currentMOH$cases = currentMOH$cases/reportingRate
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

#345835,885,10026,210526
#266931,885,61452,238004
#257248,885,32645,276494
#145212,2600,6089,413305
#303962,885,75185,187240
area = 'Dehiwala'
predA = pred/10000
tempTest = test[test$moh_name==area,]
tempSEIR = array(tempTest[1,][2:5])
SEIR = data.frame(sh = tempSEIR[1], eh = tempSEIR[2], ih = tempSEIR[3], rh = tempSEIR[4])
for(week in 0:(length(predA)-1)) {
  tempSEIR = calculateDengueDynamicsWeekly(week,sh0 = tempSEIR[1], eh0 = tempSEIR[2], ih0 = tempSEIR[3], rh0 = tempSEIR[4], index = (week+1))
  #tempSEIR = calculateDengueDynamicsWeekly10(week,sh0 = tempSEIR[1], eh0 = tempSEIR[2], ih0 = tempSEIR[3], rh0 = tempSEIR[4], index = (week+1), 4)
  #tempSEIR = calculateDengueDynamicsWeeklyWithPredVals(index = (week+1))
  #SEIR = data.frame(sh = tempSEIR[1], eh = tempSEIR[2], ih = tempSEIR[3], rh = tempSEIR[4])  
  SEIR[week+2, ] = tempSEIR
}
rmsle(predicted = pred, actual = actual)

plot(tempTest$day, tempTest$best.eh, xlab = "Week", ylab = "Eh",type = "o", pch = 21, col = 1, main = paste("Dengue 2014 from Dengue 2012 and Dengue 2013 : ", area))
lines(tempTest$day, SEIR$eh[tempTest$day+1-tempTest$day[1]], type = "o", pch = 22, col = 2)
legend("topright",col=c(2,1),lty=1,legend=c("Predicted","Actual"))

#maxs <- apply(SEIR, 2, max) 
#mins <- apply(SEIR, 2, min)
#SEIR <- as.data.frame(scale(SEIR, center = mins, scale = maxs - mins))

#maxs <- apply(test, 2, max) 
#mins <- apply(test, 2, min)
#test <- as.data.frame(scale(test, center = mins, scale = maxs - mins))

#Plot Dengue cases
denguePredsFor2014 = SEIR$eh*gammah*reportingRate
RMSLE = rmsle(predicted = as.numeric(denguePredsFor2014[test$day+1-test$day[1]]), actual = as.numeric(dengue2014[dengue2014$MOH_name==area,][test$day+1+2-test$day[1]]))
RMSLE

plot((test$day+1), denguePredsFor2014[1:52], xlab = "Week", ylab = "Dengue Cases",type = "l", col = 2, main = paste("Dengue 2014 from Dengue 2012, Dengue 2013, Dengue 2014 1to30 : ", dengue2014[area2013_2014,][2]))
lines((test$day+1), cases2014[1:52], type = "l", col = 1)

data = data.frame(week = (test$day+1), predicted = as.numeric(denguePredsFor2014[test$day+1-test$day[1]]), actual = as.numeric(dengue2014[dengue2014$MOH_name==area,][test$day+1+2-test$day[1]]))
dmelt = melt(data, id = "week")
title = paste("Dengue Incidences 2014 - ", area, " RMSLE = ", round(RMSLE, digits = 5))

ggplot(data = dmelt, 
       aes(x = week, y = value, color = variable, shape=variable)) +
  xlab("Week") +
  ylab("Incidences") +
  #theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  geom_line() +
  geom_point()+
  ggtitle(title, subtitle = "Model Trained by 2012 data and 4th, 5th, 6th lag of cases with 12 temperature lags and 7 mobility lags") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))


#All in 3 files
resultLocation = "/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/All"
testLocation = resultLocation

results1 = fread(paste(resultLocation,"SEIRAnalysis2012.csv",sep = "/"), data.table = F, header = T)
results2 = fread(paste(resultLocation,"SEIRAnalysis2013.csv",sep = "/"), data.table = F, header = T)
results3 = fread(paste(resultLocation,"SEIRAnalysis2014.csv",sep = "/"), data.table = F, header = T)
test = fread(paste(testLocation,"SEIRAnalysis2014TEST.csv",sep = "/"), data.table = F, header = T)

results1 <- data.frame(sapply(results1[1:7], as.numeric), results1[8])
results2 <- data.frame(sapply(results2[1:7], as.numeric), results2[8])
results3 <- data.frame(sapply(results3[1:7], as.numeric), results3[8])
test <- data.frame(sapply(test[1:7], as.numeric), test[8])

dehiwala2012 = 1
dehiwala2013_2014 = 55

colombo2012 = 8
colombo2013_2014 = 181

area2012 = colombo2012
area2013_2014 = colombo2013_2014

currentMOH = data.frame(week = 1:156, cases = 1:156)
cases2012 = melt(dengue2012[area2012,][3:54])$value
cases2013 = melt(dengue2013[area2013_2014,][3:54])$value
cases2014 = melt(dengue2014[area2013_2014,][3:54])$value
cases2011 = (cases2012+cases2013)/2
cases2015 = (cases2013+cases2014)/2

#A list to keep combined cases
#cases = list()
#cases[[2012]] = data.frame(moh_name = dengue2012[area2012,][,2], week = c(0:51), cases = c(cases2012, cases2013, cases2011)/reportingRate)
#cases[[2013]] = data.frame(moh_name = dengue2013[area2013_2014,][,2], week = c(0:51), cases = c(cases2013, cases2014, cases2012)/reportingRate)
#cases[[2014]] = data.frame(moh_name = dengue2014[area2013_2014,][,2], week = c(0:51), cases = c(cases2014, cases2015, cases2011)/reportingRate)

#cases2 = list()
#area2012 = colombo2012
#area2013_2014 = colombo2013_2014
#cases2012 = melt(dengue2012[area2012,][3:54])$value
#cases2013 = melt(dengue2013[area2013_2014,][3:54])$value
#cases2014 = melt(dengue2014[area2013_2014,][3:54])$value
#cases2011 = (cases2012+cases2013)/2
#cases2015 = (cases2013+cases2014)/2
#cases2[[2012]] = data.frame(moh_name = "MC - Colombo", week = c(0:51), cases = c(cases2012, cases2013, cases2011)/reportingRate)
#cases2[[2013]] = data.frame(moh_name = dengue2013[area2013_2014,][,2], week = c(0:51), cases = c(cases2013, cases2014, cases2012)/reportingRate)
#cases2[[2014]] = data.frame(moh_name = dengue2014[area2013_2014,][,2], week = c(0:51), cases = c(cases2014, cases2015, cases2011)/reportingRate)

#cases[[2012]] = merge(cases[[2012]], cases2[[2012]], all=T, sort=F)
#cases[[2013]] = merge(cases[[2013]], cases2[[2013]], all=T, sort=F)
#cases[[2014]] = merge(cases[[2014]], cases2[[2014]], all=T, sort=F)

currentMOH$cases= c(cases2012, cases2013, cases2011)
currentMOH$cases = currentMOH$cases/reportingRate
results1 = replaceInitValues(results1)

currentMOH$cases= c(cases2013, cases2014, cases2012)
currentMOH$cases = currentMOH$cases/reportingRate
results2 = replaceInitValues(results2)

currentMOH$cases = c(cases2014, cases2015, cases2011)
currentMOH$cases = currentMOH$cases/reportingRate
#results3 = replaceInitValues(results3)
test = replaceInitValues(test)

#results1$seasonality = 1
#results2$seasonality = 0
#results3$seasonality = 1
#test$seasonality = 1

results = merge(results1, results2, all = T)
results = merge(results, results3, all = T)


testData = data.frame(day = 6, 
                      best.sh = test$best.sh[test$day==6], 
                      best.eh = test$best.eh[test$day==6], 
                      best.ih = test$best.ih[test$day==6], 
                      best.rh = test$best.rh[test$day==6], 
                      best.a = 0,
                      test[7,][7:ncol(test)])
sparce_matrix_test <- sparse.model.matrix(best.a ~ .-1, data = testData)
predict(model, sparce_matrix_test)


testLocation = resultLocation
setTrainingAndTest = function(resultLocation, testLocation, area2012, area2013_2014, tempArea, mobilityArea) {
  results1 <<- fread(paste(resultLocation,"SEIRAnalysis2012.csv",sep = "/"), data.table = F, header = T, stringsAsFactors = F)
  results2 <<- fread(paste(resultLocation,"SEIRAnalysis2013.csv",sep = "/"), data.table = F, header = T, stringsAsFactors = F)
  #results3 = fread(paste(resultLocation,"SEIRAnalysis2014.csv",sep = "/"), data.table = F, header = T, stringsAsFactors = F)
  test1 <<- fread(paste(testLocation,"SEIRAnalysis2014TEST.csv",sep = "/"), data.table = F, header = T, stringsAsFactors = F)
  
  results1 <<- data.frame(sapply(results1[1:7], as.numeric), results1[8], sapply(results1[9], as.numeric), stringsAsFactors = F)
  results2 <<- data.frame(sapply(results2[1:7], as.numeric), results2[8], sapply(results2[9], as.numeric), stringsAsFactors = F)
  #results3 <- data.frame(sapply(results3[1:7], as.numeric), results3[8], sapply(results2[9], as.numeric), stringsAsFactors = F)
  test1 <<- data.frame(sapply(test1[1:7], as.numeric), test1[8], sapply(test1[9], as.numeric), stringsAsFactors = F)
  
  temperature = melt(temperatureData2013[tempArea,][,3:54])$value
  mobility = data.frame(week = c(mobility2013$week[mobility2013$moh.id==mobilityArea], 49:52), 
                        mobility = c(mobility2013$mobility.value[mobility2013$moh.id==mobilityArea], (mobility2013$mobility.value[mobility2013$moh.id==mobilityArea])[45:48]), 
                        stringsAsFactors = F
  )
  
  currentMOH = data.frame(week = 1:156, cases = 1:156)
  cases2012 = melt(dengue2012[area2012,][3:54])$value
  cases2013 = melt(dengue2013[area2013_2014,][3:54])$value
  cases2014 = melt(dengue2014[area2013_2014,][3:54])$value
  cases2011 = (cases2012+cases2013)/2
  cases2015 = (cases2013+cases2014)/2
  
  currentMOH$cases = c(cases2012, cases2013, cases2011)
  currentMOH$cases = currentMOH$cases/reportingRate
  results1 <<- replaceInitValues(results1)
  
  currentMOH$cases= c(cases2013, cases2014, cases2012)
  currentMOH$cases = currentMOH$cases/reportingRate
  results2 <<- replaceInitValues(results2)
  
  currentMOH$cases = c(cases2014, cases2015, cases2013)
  currentMOH$cases = currentMOH$cases/reportingRate
  #results3 = replaceInitValues(results3)
  test1 <<- replaceInitValues(test1)
  
  cat("Reporting rate: ", reportingRate)
  
  results <<- joinFrames(results, results1)
  results <<- joinFrames(results, results2)
  #results <<- merge(results, results3, all = T)
  
  test <<- joinFrames(test, test1)
}

joinFrames = function(frame1, frame2) {
  if(nrow(frame1)==0) {
    frame1 = frame2
  }
  else {
    frame1EndIndex = nrow(frame1)
    frame1[(frame1EndIndex+1):(frame1EndIndex+nrow(frame2)),] = frame2[1:nrow(frame2),]  
  }
  
  return(frame1)
}

# Dengue cases MOH area ids
dehiwala2012 = 1
dehiwala2013_2014 = 55
colombo2012 = 8
colombo2013_2014 = 181
## Kaduwela = 4, 111
## Moratuwa = 9, 200
## Maharagama = 7, 166

# Temperature area ids
colomboTempArea = 138
dehiwalaTempArea = 296
## Kaduwela = 103
## Moratuwa = 75
## Maharagama = 97

# Mobility area ids
## Colombo - 52
## Dehiwala - 57
## Kaduwela = 112
## Moratuwa = 199
## Maharagama = 170


replaceInitValues = function(results1) {
  #  tempResults = results1
  #  for(index in 2:nrow(results1)) {
  #    results1[index,][2:5] = tempResults[(index-1),][2:5];
  #  }
  #  results1 = results1[2:nrow(results1),]
  
  #results1$temperature[3:NROW(results1)] = temperature[results1$day[2:length(results1)]-1]
  
  #averageCases = as.integer(mean(currentMOH$cases))
  
  #results1$casesLag1 = cases[[results1$year[1]]][cases[[results1$year[1]]]$moh_name == results1$moh_name,]$cases[circularIndex(results1$day, 1, 156)]
  #results1$casesLag2 = cases[[results1$year[1]]][cases[[results1$year[1]]]$moh_name == results1$moh_name,]$cases[circularIndex(results1$day, 2, 156)]
  #results1$casesLag3 = cases[[results1$year[1]]][cases[[results1$year[1]]]$moh_name == results1$moh_name,]$cases[circularIndex(results1$day, 3, 156)]
  #results1$casesLag4 = cases[[results1$year[1]]][cases[[results1$year[1]]]$moh_name == results1$moh_name,]$cases[circularIndex(results1$day, 4, 156)]
  
  #results1$casesLag1 = currentMOH$cases[circularIndex(results1$day, 1, 156)]
  #results1$casesLag2 = currentMOH$cases[circularIndex(results1$day, 2, 156)]
  #results1$casesLag3 = currentMOH$cases[circularIndex(results1$day, 3, 156)]
  results1$casesLag4 = currentMOH$cases[circularIndex(results1$day, 4, 156)]
  results1$casesLag5 = currentMOH$cases[circularIndex(results1$day, 5, 156)]
  results1$casesLag6 = currentMOH$cases[circularIndex(results1$day, 6, 156)]
  
  #results1$lastWeekCases[1:2] = as.integer(mean(results1$lastWeekCases[3:NROW(results1)]))
  
  #results1$temperature[1:2] = as.integer(mean(results1$temperature[3:NROW(results1)]))
  #results1$rainfall[2:NROW(results1)] = rainFall[1:(NROW(results1)-1)]
  #results1$partialDiff = (gammah-1)*results1$best.eh/results1$best.sh
  
  results1$tempLag4 = temperature[circularIndex(results1$day, 4, 52)]
  results1$tempLag5 = temperature[circularIndex(results1$day, 5, 52)]
  results1$tempLag6 = temperature[circularIndex(results1$day, 6, 52)]
  results1$tempLag7 = temperature[circularIndex(results1$day, 7, 52)]
  results1$tempLag8 = temperature[circularIndex(results1$day, 8, 52)]
  results1$tempLag9 = temperature[circularIndex(results1$day, 9, 52)]
  results1$tempLag10 = temperature[circularIndex(results1$day, 10, 52)]
  
  #Factorized mobility values
  current.moh.name = unique(results1$moh_name)
  current.moh.year = unique(results1$year)
  
  cases2012 = mobilityTripsFactorized[mobilityTripsFactorized$MOH_NAME==current.moh.name,]$HOME_CASES_FACTOR_2012
  cases2013 = mobilityTripsFactorized[mobilityTripsFactorized$MOH_NAME==current.moh.name,]$HOME_CASES_FACTOR_2013
  cases2014 = mobilityTripsFactorized[mobilityTripsFactorized$MOH_NAME==current.moh.name,]$HOME_CASES_FACTOR_2014
  cases2011 = (cases2012+cases2013)/2
  cases2015 = (cases2013+cases2014)/2
  
  if(current.moh.year==2012) {
    mobilityMOH$cases = c(cases2012, cases2013, cases2011)
    mobilityMOH$cases = mobilityMOH$cases/reportingRate
  }
  
  if(current.moh.year==2013) {
    mobilityMOH$cases= c(cases2013, cases2014, cases2012)
    mobilityMOH$cases = mobilityMOH$cases/reportingRate
  }
  
  if(current.moh.year==2014) {
    mobilityMOH$cases = c(cases2014, cases2015, cases2013)
    mobilityMOH$cases = mobilityMOH$cases/reportingRate
  }
  results1$mobilityLag4 = mobilityMOH$cases[circularIndex(results1$day, 4, 156)]
  results1$mobilityLag5 = mobilityMOH$cases[circularIndex(results1$day, 5, 156)]
  results1$mobilityLag6 = mobilityMOH$cases[circularIndex(results1$day, 6, 156)]
  results1$mobilityLag7 = mobilityMOH$cases[circularIndex(results1$day, 7, 156)]
  results1$mobilityLag8 = mobilityMOH$cases[circularIndex(results1$day, 8, 156)]
  results1$mobilityLag9 = mobilityMOH$cases[circularIndex(results1$day, 9, 156)]
  
  ## Outbreak or not
  #results1$outbreak = 0
  #results1$outbreak[results1$best.eh > mean(results1$best.eh)] = 1
  
  ## Delete the row with a=0
  #results1 = results1[-52,];
  
  return(results1)
}  

replaceInitValues = function(results1){
  
  
  #MOH's cases
  current.moh.name = unique(results1$moh_name)
  current.moh.year = unique(results1$year)
  moh_names_array = array(unique(mobilityTrips2013$HOME[mobilityTrips2013$MOH_NAME==current.moh.name], nmax = 10))
  moh_names_array = intersect(moh_names_array, ALL_MOH_NAMES)
  
  for (index in 1:5) {
    mobilityMOH = data.frame(week = 1:156, cases = 1:156)
    cases2012 = melt(dengue2012[dengue2012$MOH_name==moh_names_array[index],][3:54])$value
    cases2013 = melt(dengue2013[dengue2012$MOH_name==moh_names_array[index],][3:54])$value
    cases2014 = melt(dengue2014[dengue2012$MOH_name==moh_names_array[index],][3:54])$value
    cases2011 = (cases2012+cases2013)/2
    cases2015 = (cases2013+cases2014)/2
    
    if(current.moh.year==2012) {
      mobilityMOH$cases = c(cases2012, cases2013, cases2011)
      mobilityMOH$cases = mobilityMOH$cases/reportingRate
    }
    
    if(current.moh.year==2013) {
      mobilityMOH$cases= c(cases2013, cases2014, cases2012)
      mobilityMOH$cases = mobilityMOH$cases/reportingRate
    }
    
    if(current.moh.year==2014) {
      mobilityMOH$cases = c(cases2014, cases2015, cases2013)
      mobilityMOH$cases = mobilityMOH$cases/reportingRate
    }
    
    results1 = data.frame(results1, 
                          moh_lag4=mobilityMOH$cases[circularIndex(results1$day, 4, 156)],
                          moh_lag5=mobilityMOH$cases[circularIndex(results1$day, 5, 156)],
                          moh_lag6=mobilityMOH$cases[circularIndex(results1$day, 6, 156)]
    )
  }
  
  
  
  #Factorized mobility values
  current.moh.name = unique(results1$moh_name)
  current.moh.year = unique(results1$year)
  
  current.mobility.trips = mobilityTrips2013[mobilityTrips2013$MOH_NAME==current.moh.name,]
  current.mobility.trips = current.mobility.trips[(current.mobility.trips$HOME %in% ALL_MOH_NAMES),]
  mobilityMOH = data.frame(week = 1:156, cases = 1:156)
  
  getCases = function(x) {
    column = (as.integer(x[3])+2)
    dengue2012[dengue2012$MOH_name==x[2],][,column]/reportingRate
  }
  current.mobility.trips$HOME_CASES_2012 = apply(current.mobility.trips, 1, getCases)
  class(current.mobility.trips$HOME_CASES_2012) = "numeric"
  
  getCases = function(x) {
    column = (as.integer(x[3])+2)
    dengue2013[dengue2013$MOH_name==x[2],][,column]/reportingRate
  }
  current.mobility.trips$HOME_CASES_2013 = apply(current.mobility.trips, 1, getCases)
  class(current.mobility.trips$HOME_CASES_2013) = "numeric"
  
  getCases = function(x) {
    column = (as.integer(x[3])+2)
    dengue2014[dengue2014$MOH_name==x[2],][,column]/reportingRate
  }
  current.mobility.trips$HOME_CASES_2014 = apply(current.mobility.trips, 1, getCases)
  class(current.mobility.trips$HOME_CASES_2014) = "numeric"
  
  current.mobility.trips[is.na(current.mobility.trips)]= 0
  current.mobility.trips$HOME_CASES_FACTOR_2012 = current.mobility.trips$MOBILITY_VALUE*current.mobility.trips$HOME_CASES_2012
  current.mobility.trips$HOME_CASES_FACTOR_2013 = current.mobility.trips$MOBILITY_VALUE*current.mobility.trips$HOME_CASES_2013
  current.mobility.trips$HOME_CASES_FACTOR_2014 = current.mobility.trips$MOBILITY_VALUE*current.mobility.trips$HOME_CASES_2014
  
  index = 1
  for (week in unique(current.mobility.trips$WEEK_NUMBER)) {
    cases2012[index] = sum(current.mobility.trips$HOME_CASES_FACTOR_2012[current.mobility.trips$WEEK_NUMBER==week])/sum(current.mobility.trips$MOBILITY_VALUE[current.mobility.trips$WEEK_NUMBER==week])
    cases2013[index] = sum(current.mobility.trips$HOME_CASES_FACTOR_2013[current.mobility.trips$WEEK_NUMBER==week])/sum(current.mobility.trips$MOBILITY_VALUE[current.mobility.trips$WEEK_NUMBER==week])
    cases2014[index] = sum(current.mobility.trips$HOME_CASES_FACTOR_2014[current.mobility.trips$WEEK_NUMBER==week])/sum(current.mobility.trips$MOBILITY_VALUE[current.mobility.trips$WEEK_NUMBER==week])
    index = index+1
  }
  
  cases2011 = (cases2012+cases2013)/2
  cases2015 = (cases2013+cases2014)/2
  
  if(current.moh.year==2012) {
    mobilityMOH$cases = c(cases2012, cases2013, cases2011)
    mobilityMOH$cases = mobilityMOH$cases/reportingRate
  }
  
  if(current.moh.year==2013) {
    mobilityMOH$cases= c(cases2013, cases2014, cases2012)
    mobilityMOH$cases = mobilityMOH$cases/reportingRate
  }
  
  if(current.moh.year==2014) {
    mobilityMOH$cases = c(cases2014, cases2015, cases2013)
    mobilityMOH$cases = mobilityMOH$cases/reportingRate
  }
  results1$mobilityLag4 = mobilityMOH$cases[circularIndex(results1$day, 4, 156)]
  results1$mobilityLag5 = mobilityMOH$cases[circularIndex(results1$day, 5, 156)]
  results1$mobilityLag6 = mobilityMOH$cases[circularIndex(results1$day, 6, 156)]
  results1$mobilityLag7 = mobilityMOH$cases[circularIndex(results1$day, 7, 156)]
  results1$mobilityLag8 = mobilityMOH$cases[circularIndex(results1$day, 8, 156)]
  results1$mobilityLag9 = mobilityMOH$cases[circularIndex(results1$day, 9, 156)]
  
  return(results1)
  
}

moh_names_array = array(unique(mobilityTrips2013$HOME[mobilityTrips2013$MOH_NAME==area], nmax = 10))
moh_names_array = array(unique(mobilityTrips2013$HOME[mobilityTrips2013$MOH_NAME=='Dehiwala'], nmax = 10))
moh_names_array = intersect(moh_names_array, ALL_MOH_NAMES)
moh_names_array

ALL_MOH_NAMES = array(unique(c(dengue2013$MOH_name, dengue2012$MOH_name)))
ALL_MOH_NAMES = array(intersect(dengue2013$MOH_name, dengue2012$MOH_name))

date = "Thu Jan 05 20:45:29 IST 2017"
date = "Thu Jan 05 06:06:49 IST 2017"
date = "Sun Jan 08 10:13:13 IST 2017"
date = "Tue Jan 10 17:02:07 IST 2017"
date = "Tue Jan 10 17:53:18 IST 2017" ##with purpose of predicting the mid peak in Maharagama.....
date = "Wed Jan 11 07:34:58 IST 2017" ##with purpose of predicting the mid peak in Dehiwala.......
date = "Mon Jan 16 13:55:01 IST 2017" ##For Malaya
date = "Wed Jan 18 18:33:11 IST 2017" ##MC -Colombo with 10 iterations ------ BAD PREDICTIONS
date = "Wed Jan 18 18:41:14 IST 2017" ##MC -Colombo with 10 iterations ------ BAD PREDICTIONS
date = "Wed Jan 18 18:52:24 IST 2017" ##MC -Colombo with 200 iterations --------- RMSE = 22.7, R2 = 0.46 ----- RMSE = 22.08, R2 = 0.49 ---- RMSE = 20.83, R2 = 0.55
date = "Wed Jan 18 21:13:32 IST 2017" ##MC -Colombo with 200 iterations ------ BAD PREDICTIONS
date = "Wed Jan 18 21:07:36 IST 2017" ##MC -Colombo with 200 7 decimal places only in a iterations ------ BAD PREDICTIONS
date = "Wed Jan 18 12:01:39 IST 2017" ##MC -Colombo with 1000 iterations ------ BAD PREDICTIONS
date = "Wed Jan 18 17:49:41 IST 2017" ##MC -Colombo with 10000 iterations ------ BAD PREDICTIONS
date = "Wed Jan 18 12:15:23 IST 2017" 
date = "Thu Jan 19 07:44:39 IST 2017" ## All mohs with 200 iterations 

results = data.frame()
test = data.frame()

## Dehiwala
resultLocation = paste("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/Dehiwala-", date, sep = '')
setTrainingAndTest(resultLocation = resultLocation, testLocation = resultLocation, area2012 = 1, area2013_2014 = 55, tempArea = 296, mobilityArea = 57)

## Panadura
resultLocation = paste("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/Panadura-", date, sep = '')
setTrainingAndTest(resultLocation = resultLocation, testLocation = resultLocation, area2012 = 36, area2013_2014 = 235, tempArea = 41, mobilityArea = 239)

## Piliyandala
resultLocation = paste("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/Piliyandala-", date, sep = '')
setTrainingAndTest(resultLocation = resultLocation, testLocation = resultLocation, area2012 = 2, area2013_2014 = 242, tempArea = 29, mobilityArea = 249)

## Homagama
resultLocation = paste("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/Homagama-", date, sep = '')
setTrainingAndTest(resultLocation = resultLocation, testLocation = resultLocation, area2012 = 3, area2013_2014 = 100, tempArea = 275, mobilityArea = 201)

## Boralesgamuwa
resultLocation = paste("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/Boralesgamuwa-", date, sep = '')
setTrainingAndTest(resultLocation = resultLocation, testLocation = resultLocation, area2012 = 12, area2013_2014 = 41, tempArea = 47, mobilityArea = 43)

#Kaduwela
resultLocation = paste("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/Kaduwela-", date, sep = '')
setTrainingAndTest(resultLocation = resultLocation, testLocation = resultLocation, area2012 = 4, area2013_2014 = 111, tempArea = 103, mobilityArea = 112)

#Kollonnawa
resultLocation = paste("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/Kollonnawa-", date, sep = '')
setTrainingAndTest(resultLocation = resultLocation, testLocation = resultLocation, area2012 = 5, area2013_2014 = 142, tempArea = 168, mobilityArea = 147)

#Nugegoda
resultLocation = paste("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/Nugegoda-", date, sep = '')
setTrainingAndTest(resultLocation = resultLocation, testLocation = resultLocation, area2012 = 10, area2013_2014 = 220, tempArea = 229, mobilityArea = 220)

#Wattala
resultLocation = paste("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/Wattala-", date, sep = '')
setTrainingAndTest(resultLocation = resultLocation, testLocation = resultLocation, area2012 = 27, area2013_2014 = 308, tempArea = 146, mobilityArea = 317)

#Kelaniya
resultLocation = paste("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/Kelaniya-", date, sep = '')
setTrainingAndTest(resultLocation = resultLocation, testLocation = resultLocation, area2012 = 20, area2013_2014 = 135, tempArea = 143, mobilityArea = 140)

#Maharagama
resultLocation = paste("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/Maharagama-", date, sep = '')
setTrainingAndTest(resultLocation = resultLocation, testLocation = resultLocation,  area2012 = 7, area2013_2014 = 166, tempArea = 97, mobilityArea = 170)

#MC - Colombo
resultLocation = paste("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/MC - Colombo-", date, sep = '')
setTrainingAndTest(resultLocation = resultLocation, testLocation = resultLocation,  area2012 = 8, area2013_2014 = 181, tempArea = 138, mobilityArea = 52)

#Moratuwa
resultLocation = paste("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/Moratuwa-", date, sep = '')
setTrainingAndTest(resultLocation = resultLocation, testLocation = resultLocation,  area2012 = 9, area2013_2014 = 200, tempArea = 75, mobilityArea = 199)

## Draw incidences graph
area = 'Kaduwela'
predA = pred
tempTest = test[test$moh_name==area,]
tempSEIR = array(tempTest[1,][2:5])
SEIR = data.frame(sh = tempSEIR[1], eh = tempSEIR[2], ih = tempSEIR[3], rh = tempSEIR[4])
for(week in 0:(length(predA)-1)) {
  tempSEIR = calculateDengueDynamicsWeekly(week,sh0 = tempSEIR[1], eh0 = tempSEIR[2], ih0 = tempSEIR[3], rh0 = tempSEIR[4], index = (week+1))
  #tempSEIR = calculateDengueDynamicsWeekly10(week,sh0 = tempSEIR[1], eh0 = tempSEIR[2], ih0 = tempSEIR[3], rh0 = tempSEIR[4], index = (week+1), 4)
  #tempSEIR = calculateDengueDynamicsWeeklyWithPredVals(index = (week+1))
  #SEIR = data.frame(sh = tempSEIR[1], eh = tempSEIR[2], ih = tempSEIR[3], rh = tempSEIR[4])  
  SEIR[week+2, ] = tempSEIR
}
rmsle(predicted = pred, actual = actual)

denguePredsFor2014 = SEIR$eh*gammah*reportingRate
RMSLE = rmsle(predicted = as.numeric(denguePredsFor2014[test$day+1-test$day[1]]), actual = as.numeric(dengue2014[dengue2014$MOH_name==area,][test$day+1+2-test$day[1]]))
RMSE = rmse(predicted = as.numeric(denguePredsFor2014[test$day+1-test$day[1]]), actual = as.numeric(dengue2014[dengue2014$MOH_name==area,][test$day+1+2-test$day[1]]))
RMSE
RMSLE

data = data.frame(week = (test$day+1), predicted = as.numeric(denguePredsFor2014[test$day+1-test$day[1]]), actual = as.numeric(dengue2014[dengue2014$MOH_name==area,][test$day+1+2-test$day[1]]))
R2 <- 1 - (sum((data$actual-data$predicted )^2)/sum((data$actual-mean(data$actual))^2))
R2

dmelt = melt(data, id = "week")
title = paste("Dengue Incidences 2014 - ", area, " R-squared = ", round(R2, digits = 5), ", RMSE = ", round(RMSE, digits = 5))
mohs = paste(array(unique(results$moh_name)), collapse = ", ")
subtitle = paste("Model Trained by 2012, 2013 data with MOHs (", mohs, ") and 4th, 5th, 6th lag of cases with 12 temperature lags and 4, 5, 6 mobility lags")

ggplot(data = dmelt, 
       aes(x = week, y = value, color = variable, shape=variable)) +
  xlab("Week") +
  ylab("Incidences") +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  geom_line() +
  geom_point()+
  ggtitle(title, subtitle = subtitle) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

## Generate parameter 'a' graph
ggplot.data = data.frame(week = days, predicted = pred, actual = actual)
dmelt = melt(ggplot.data, id = "week")
test.rmsle = rmsle(predicted = pred, actual = actual)
title = paste("Parameter 'a' for 2014-Prediction by XGBoost - ", area, " RMSLE = ", round(test.rmsle, digits = 5))

ggplot(data = dmelt, 
       aes(x = week, y = value, color = variable, shape=variable)) +
  xlab("Week") +
  ylab("Incidences") +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  geom_line() +
  geom_point()+
  ggtitle(title, subtitle = subtitle) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
