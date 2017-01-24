require(data.table)

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
                        tempTest[index,][9:ncol(tempTest)], stringsAsFactors = F)
  matrix_test <- data.matrix(testData)
  matrix_test <- matrix_test[,-6]
  pred[index] <<- predict(model, matrix(matrix_test, nrow = 1))
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

calculateDengueDynamicsRecursively = function(seirDataFrame, gap = 4) {
  if(nrow(seirDataFrame) != (gap+1)) {
    for(week in 0:gap){
      seirDataFrame[week+2,] = calculateDengueDynamicsWeekly(day = week, sh0 = seirDataFrame[week+1,]$sh, eh0 = seirDataFrame[week+1,]$eh, ih0 = seirDataFrame[week+1,]$ih, rh0 = seirDataFrame[week+1,]$rh, index = week+1)
    }
  }
  
  for(week in (gap+1):51) {
    lastAnswer = calculateDengueDynamicsWeekly(day = week-gap, sh0 = seirDataFrame[week-gap+1,]$sh, eh0 = seirDataFrame[week-gap+1,]$eh, ih0 = seirDataFrame[week-gap+1,]$ih, rh0 = seirDataFrame[week-gap+1,]$rh, index = week-gap+1)
    for(offset in 1:gap) {
      lastAnswer = calculateDengueDynamicsWeekly(day = week+offset-gap, sh0 = lastAnswer[1], eh0 = lastAnswer[2], ih0 = lastAnswer[3], rh0 = lastAnswer[4], index = week+offset-gap+1)
    }
    seirDataFrame[week+1,] = lastAnswer
  }
  
  return(seirDataFrame)
}

circularIndex = function(index, lag=1, array.size) {
  newIndex = index-lag
  
  return(ifelse(newIndex<=0, (array.size-abs(newIndex)), newIndex))
}

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
  #results <<- joinFrames(results, results3)
  
  test <<- joinFrames(test, test1)
}

setTrainingAndTest = function(resultLocation, testLocation, mohName) {
  results1 <<- fread(paste(resultLocation,"SEIRAnalysis2012.csv",sep = "/"), data.table = F, header = T, stringsAsFactors = F)
  results2 <<- fread(paste(resultLocation,"SEIRAnalysis2013.csv",sep = "/"), data.table = F, header = T, stringsAsFactors = F)
  #results3 = fread(paste(resultLocation,"SEIRAnalysis2014.csv",sep = "/"), data.table = F, header = T, stringsAsFactors = F)
  test1 <<- fread(paste(testLocation,"SEIRAnalysis2014TEST.csv",sep = "/"), data.table = F, header = T, stringsAsFactors = F)
  
  results1 <<- data.frame(sapply(results1[1:7], as.numeric), results1[8], sapply(results1[9], as.numeric), stringsAsFactors = F)
  results2 <<- data.frame(sapply(results2[1:7], as.numeric), results2[8], sapply(results2[9], as.numeric), stringsAsFactors = F)
  #results3 <- data.frame(sapply(results3[1:7], as.numeric), results3[8], sapply(results2[9], as.numeric), stringsAsFactors = F)
  test1 <<- data.frame(sapply(test1[1:7], as.numeric), test1[8], sapply(test1[9], as.numeric), stringsAsFactors = F)
  
  temperature <<- melt(temperatureData2013[temperatureData2013$MOH_name==mohName,][,3:54])$value
  vegetationIndexes = vegetationIndicesWeekly[vegetationIndicesWeekly$MOH_name==mohName,]
  
  currentMOH <<- data.frame(week = 1:156, cases = 1:156, veg_index = 1:156)
  cases2012 = melt(dengue2012[dengue2012$MOH_name==mohName,][,3:54])$value
  cases2013 = melt(dengue2013[dengue2013$MOH_name==mohName,][,3:54])$value
  cases2014 = melt(dengue2014[dengue2014$MOH_name==mohName,][,3:54])$value
  cases2011 = (cases2012+cases2013)/2
  cases2015 = (cases2013+cases2014)/2
  
  vegIndexes2013 = as.numeric(vegetationIndexes[vegetationIndexes$year == 2013,][,3:54])
  vegIndexes2014 = as.numeric(vegetationIndexes[vegetationIndexes$year == 2014,][,3:54])
  vegIndexes2015 = as.numeric(vegetationIndexes[vegetationIndexes$year == 2015,][,3:54])
  vegIndexes2012 = (vegIndexes2013+vegIndexes2014)/2
  vegIndexes2011 = (vegIndexes2012+vegIndexes2013)/2
  
  currentMOH$cases <<- c(cases2012, cases2013, cases2011)
  currentMOH$cases <<- currentMOH$cases/reportingRate
  currentMOH$veg_index <<- c(vegIndexes2012, vegIndexes2013, vegIndexes2011)
  results1 <<- replaceInitValues(results1)
  
  currentMOH$cases <<- c(cases2013, cases2014, cases2012)
  currentMOH$cases <<- currentMOH$cases/reportingRate
  currentMOH$veg_index <<- c(vegIndexes2013, vegIndexes2014, vegIndexes2012)
  results2 <<- replaceInitValues(results2)
  
  currentMOH$cases <<- c(cases2014, cases2015, cases2013)
  currentMOH$cases <<- currentMOH$cases/reportingRate
  currentMOH$veg_index <<- c(vegIndexes2014, vegIndexes2015, vegIndexes2013)
  #results3 = replaceInitValues(results3)
  test1 <<- replaceInitValues(test1)
  
  cat("Reporting rate: ", reportingRate, fill = T)
  
  results <<- joinFrames(results, results1)
  results <<- joinFrames(results, results2)
  #results <<- joinFrames(results, results3)
  
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

replaceInitValues = function(results1) {
  # Previous cases
  results1$casesLag8 = currentMOH$cases[circularIndex(results1$day, 8, 156)]
  results1$casesLag9 = currentMOH$cases[circularIndex(results1$day, 9, 156)]
  results1$casesLag10 = currentMOH$cases[circularIndex(results1$day, 10, 156)]
  
  # Previous temperatures
  results1$tempLag8 = temperature[circularIndex(results1$day, 8, 52)]
  results1$tempLag9 = temperature[circularIndex(results1$day, 9, 52)]
  results1$tempLag10 = temperature[circularIndex(results1$day, 10, 52)]
  results1$tempLag11 = temperature[circularIndex(results1$day, 11, 52)]
  results1$tempLag12 = temperature[circularIndex(results1$day, 12, 52)]
  results1$tempLag13 = temperature[circularIndex(results1$day, 13, 52)]
  results1$tempLag14 = temperature[circularIndex(results1$day, 14, 52)]
  
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
  results1$mobilityLag8 = mobilityMOH$cases[circularIndex(results1$day, 8, 156)]
  results1$mobilityLag9 = mobilityMOH$cases[circularIndex(results1$day, 9, 156)]
  results1$mobilityLag10 = mobilityMOH$cases[circularIndex(results1$day, 10, 156)]
  results1$mobilityLag11 = mobilityMOH$cases[circularIndex(results1$day, 11, 156)]
  results1$mobilityLag12 = mobilityMOH$cases[circularIndex(results1$day, 12, 156)]
  results1$mobilityLag13 = mobilityMOH$cases[circularIndex(results1$day, 13, 156)]
  
  
  ##Vegetation index lags
  #results1$vegIndexLag4 = currentMOH$veg_index[circularIndex(results1$day, 4, 156)]
  #results1$vegIndexLag5 = currentMOH$veg_index[circularIndex(results1$day, 5, 156)]
  #results1$vegIndexLag6 = currentMOH$veg_index[circularIndex(results1$day, 6, 156)]
  
  return(results1)
}  

replaceInitValues = function(results1){
  #previous mobilities
  results1$mobilityLag4 = c(mobility$mobility[circularIndex(results1$day, 4, 52)])
  results1$mobilityLag5 = c(mobility$mobility[circularIndex(results1$day, 5, 52)])
  results1$mobilityLag6 = c(mobility$mobility[circularIndex(results1$day, 6, 52)])
  results1$mobilityLag7 = c(mobility$mobility[circularIndex(results1$day, 7, 52)])
  results1$mobilityLag8 = c(mobility$mobility[circularIndex(results1$day, 8, 52)])
  results1$mobilityLag9 = c(mobility$mobility[circularIndex(results1$day, 9, 52)])
  results1$mobilityLag10 = c(mobility$mobility[circularIndex(results1$day, 10, 52)])
  
  
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
  
  return(results1)
  
}

## Get top 10 moh areas according to mobility
moh_names_array = array(unique(mobilityTrips2013$HOME[mobilityTrips2013$MOH_NAME=='MC - Colombo'], nmax = 10))
moh_names_array = intersect(moh_names_array, ALL_MOH_NAMES)
moh_names_array

# Get all MOH area names which are common in both dengue 2012 and 2013_2014
ALL_MOH_NAMES = array(intersect(dengue2013$MOH_name, dengue2012$MOH_name))

date = "Thu Jan 05 20:45:29 IST 2017"
date = "Thu Jan 05 06:06:49 IST 2017" ## CMC RMSE = 21.96, R2 = 0.50
date = "Sun Jan 08 10:13:13 IST 2017"
date = "Tue Jan 10 17:02:07 IST 2017"
date = "Tue Jan 10 17:53:18 IST 2017" ##with purpose of predicting the mid peak in Maharagama.....
date = "Wed Jan 11 07:34:58 IST 2017" ##with purpose of predicting the mid peak in Dehiwala.......
date = "Mon Jan 16 13:55:01 IST 2017" ##For Malaya
date = "Wed Jan 18 18:33:11 IST 2017" ##MC -Colombo with 10 iterations ------ BAD PREDICTIONS
date = "Wed Jan 18 18:41:14 IST 2017" ##MC -Colombo with 10 iterations ------ BAD PREDICTIONS
date = "Wed Jan 18 18:52:24 IST 2017" ##MC -Colombo with 200 iterations ------ RMSE = 22.7, R2 = 0.46 ----- RMSE = 22.08, R2 = 0.49 ---- RMSE = 20.83, R2 = 0.55
date = "Wed Jan 18 21:13:32 IST 2017" ##MC -Colombo with 200 iterations ------ BAD PREDICTIONS
date = "Wed Jan 18 21:07:36 IST 2017" ##MC -Colombo with 200 7 decimal places only in a iterations ------ BAD PREDICTIONS
date = "Thu Jan 19 12:11:15 IST 2017" ##Dehiwala and CMC with 200 with reportingrate = 0.2
date = "Thu Jan 19 12:20:13 IST 2017" ##All with 200 with reportingrate = 0.025
date = "Thu Jan 19 12:49:55 IST 2017" ##All with 200 with reportingrate = 0.04
date = "Wed Jan 18 12:01:39 IST 2017" ##MC -Colombo with 1000 iterations ------ BAD PREDICTIONS
date = "Wed Jan 18 17:49:41 IST 2017" ##MC -Colombo with 10000 iterations ------ BAD PREDICTIONS
date = "Wed Jan 18 12:15:23 IST 2017" 
date = "Thu Jan 19 07:44:39 IST 2017" ## All mohs with 200 iterations 

results = data.frame()
test = data.frame()

## Dehiwala
resultLocation = paste("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/", date, "/Dehiwala", sep = '')
setTrainingAndTest(resultLocation = resultLocation, testLocation = resultLocation, area2012 = 1, area2013_2014 = 55, tempArea = 296, mobilityArea = 57)

## Panadura
resultLocation = paste("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/", date, "Panadura", sep = '')
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
resultLocation = paste("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/", date, "/Kaduwela", sep = '')
setTrainingAndTest(resultLocation = resultLocation, testLocation = resultLocation, area2012 = 4, area2013_2014 = 111, tempArea = 103, mobilityArea = 112)
setTrainingAndTest(resultLocation = resultLocation, testLocation = resultLocation, "Kaduwela")

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
resultLocation = paste("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/", date, "/Maharagama", sep = '')
setTrainingAndTest(resultLocation = resultLocation, testLocation = resultLocation,  area2012 = 7, area2013_2014 = 166, tempArea = 97, mobilityArea = 170)
setTrainingAndTest(resultLocation = resultLocation, testLocation = resultLocation, "Maharagama")

#MC - Colombo
resultLocation = paste("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/", date, "/MC - Colombo", sep = '')
setTrainingAndTest(resultLocation = resultLocation, testLocation = resultLocation,  area2012 = 8, area2013_2014 = 181, tempArea = 138, mobilityArea = 52)
setTrainingAndTest(resultLocation = resultLocation, testLocation = resultLocation,  "MC - Colombo")

#Moratuwa
resultLocation = paste("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/", date, "/Moratuwa", sep = '')
setTrainingAndTest(resultLocation = resultLocation, testLocation = resultLocation,  area2012 = 9, area2013_2014 = 200, tempArea = 75, mobilityArea = 199)
setTrainingAndTest(resultLocation = resultLocation, testLocation = resultLocation, "Moratuwa")

## Draw incidences graph
area = 'Kaduwela'
predA = pred
tempTest = test[test$moh_name==area,]
tempSEIR = array(tempTest[1,][2:5])
SEIR = data.frame(sh = tempSEIR[1], eh = tempSEIR[2], ih = tempSEIR[3], rh = tempSEIR[4])
for(week in 0:(length(predA)-1)) {
  tempSEIR = calculateDengueDynamicsWeekly(day = week,sh0 = tempSEIR[1], eh0 = tempSEIR[2], ih0 = tempSEIR[3], rh0 = tempSEIR[4], index = (week+1))
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
title = paste("Dengue Incidences ", unique(tempTest$year), " - ", area, " R-squared = ", round(R2, digits = 5), ", RMSE = ", round(RMSE, digits = 5))
mohs = paste(array(unique(results$moh_name)), collapse = ", ")
trainedYears = paste(array(unique(results$year)), collapse = ", ")
cols = colnames(results)
caseLags = paste((gsub('casesLag([0-9]+).*', "\\1", cols[grep("casesLag", cols)])), collapse = ", ")
temperatureLags = paste((gsub('tempLag([0-9]+).*', "\\1", cols[grep("tempLag", cols)])), collapse = ", ")
mobilityLags = paste((gsub('mobilityLag([0-9]+).*', "\\1", cols[grep("mobilityLag", cols)])), collapse = ", ")
subtitle = paste("Model Trained by ", trainedYears, " data with MOHs (", mohs, ") \nand case lags (", caseLags, ") , temperature lags (", temperatureLags, ") , mobility lags (", mobilityLags, ")")

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


predictSEIR = function(area) {
  tempTest <<- test[test$moh_name==area,]
  tempSEIR <<- array(tempTest[1,][2:5])
  SEIR <<- data.frame(sh = tempSEIR[1], eh = tempSEIR[2], ih = tempSEIR[3], rh = tempSEIR[4])
  #SEIR <<- calculateDengueDynamicsRecursively(seirDataFrame = SEIR)
  for(week in 0:(length(predA)-1)) {
    tempSEIR <<- calculateDengueDynamicsWeekly(week,sh0 = tempSEIR[1], eh0 = tempSEIR[2], ih0 = tempSEIR[3], rh0 = tempSEIR[4], index = (week+1))
    #tempSEIR = calculateDengueDynamicsWeekly10(week,sh0 = tempSEIR[1], eh0 = tempSEIR[2], ih0 = tempSEIR[3], rh0 = tempSEIR[4], index = (week+1), 4)
    SEIR[week+2, ] <<- tempSEIR
  }
  cat(rmsle(predicted = pred, actual = actual), fill = T)
}

plotIncidencesGraph = function(area) {
  denguePredsFor2014 <<- SEIR$eh*gammah*reportingRate
  RMSLE <<- rmsle(predicted = as.numeric(denguePredsFor2014[test$day+1-test$day[1]]), actual = as.numeric(dengue2014[dengue2014$MOH_name==area,][test$day+1+2-test$day[1]]))
  RMSE <<- rmse(predicted = as.numeric(denguePredsFor2014[test$day+1-test$day[1]]), actual = as.numeric(dengue2014[dengue2014$MOH_name==area,][test$day+1+2-test$day[1]]))
  cat(RMSE, fill = T)
  cat(RMSLE)
  
  data <<- data.frame(week = (test$day+1), predicted = as.numeric(denguePredsFor2014[test$day+1-test$day[1]]), actual = as.numeric(dengue2014[dengue2014$MOH_name==area,][test$day+1+2-test$day[1]]))
  R2 <<- 1 - (sum((data$actual-data$predicted )^2)/sum((data$actual-mean(data$actual))^2))
  R2
  
  dmelt <<- melt(data, id = "week")
  title <<- paste("Dengue Incidences ", unique(tempTest$year), " - ", area, " R-squared = ", round(R2, digits = 5), ", RMSE = ", round(RMSE, digits = 5))
  mohs <<- paste(array(unique(results$moh_name)), collapse = ", ")
  trainedYears <<- paste(array(unique(results$year)), collapse = ", ")
  cols <<- colnames(results)
  caseLags <<- paste((gsub('casesLag([0-9]+).*', "\\1", cols[grep("casesLag", cols)])), collapse = ", ")
  temperatureLags <<- paste((gsub('tempLag([0-9]+).*', "\\1", cols[grep("tempLag", cols)])), collapse = ", ")
  mobilityLags <<- paste((gsub('mobilityLag([0-9]+).*', "\\1", cols[grep("mobilityLag", cols)])), collapse = ", ")
  subtitle <<- paste("Model Trained by ", trainedYears, " data with MOHs (", mohs, ") \nand case lags (", caseLags, ") , temperature lags (", temperatureLags, ") , mobility lags (", mobilityLags, ")")
  
  incidencesPlot <- ggplot(data = dmelt, 
         aes(x = week, y = value, color = variable, shape=variable)) +
    xlab("Week") +
    ylab("Incidences") +
    theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
    geom_line() +
    geom_point()+
    ggtitle(title, subtitle = subtitle) +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  
  return(incidencesPlot)
}
