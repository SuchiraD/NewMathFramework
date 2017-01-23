require("stringr")

trainingDataFrame = data.frame()
testingDataFrame = data.frame()


###################################      Functions       ######################
setTrainingAndTestML = function(mohName) {
  
  population = populations[populations$MOH_NAME==mohName, ]$actual_POP
  temperature <<- melt(temperatureData2013[temperatureData2013$MOH_name==mohName,][,3:54])$value
  
  #currentMOH <<- data.frame(week = 1:156, cases = 1:156)
  cases2012 = melt(dengue2012[dengue2012$MOH_name==mohName,][,3:54])$value
  cases2013 = melt(dengue2013[dengue2013$MOH_name==mohName,][,3:54])$value
  cases2014 = melt(dengue2014[dengue2014$MOH_name==mohName,][,3:54])$value
  cases2011 = (cases2012+cases2013)/2
  cases2015 = (cases2013+cases2014)/2
  
  currentMOH <<- data.frame(week = 1:156, cases = 1:156)
  currentMOH$cases <<- c(cases2012, cases2013, cases2011)
  #currentMOH$cases <<- currentMOH$cases/reportingRate
  trainingDataFrame1 <<- data.frame(cases = cases2012, week = 1:52, year = 2012, moh_name = mohName, population = population, stringsAsFactors = F)
  trainingDataFrame1 <<- setColumns(trainingDataFrame1)
  
  currentMOH$cases <<- c(cases2013, cases2014, cases2012)
  #currentMOH$cases <<- currentMOH$cases/reportingRate
  trainingDataFrame2 <<- data.frame(cases = cases2013, week = 1:52, year = 2013, moh_name = mohName, population = population, stringsAsFactors = F)
  trainingDataFrame2 <<- setColumns(trainingDataFrame2)
  
  currentMOH$cases <<- c(cases2014, cases2015, cases2013)
  #currentMOH$cases <<- currentMOH$cases/reportingRate
  #results3 = setColumns(results3)
  testingDataFrame1 <<- data.frame(cases = cases2014, week = 1:52, year = 2014, moh_name = mohName, population = population, stringsAsFactors = F)
  testingDataFrame1 <<- setColumns(testingDataFrame1)
  
  cat("Reporting rate: ", reportingRate, fill = T)
  
  trainingDataFrame <<- joinFrames(trainingDataFrame, trainingDataFrame1)
  trainingDataFrame <<- joinFrames(trainingDataFrame, trainingDataFrame2)
  #trainingDataFrame <<- joinFrames(trainingDataFrame, results3)
  
  testingDataFrame <<- joinFrames(testingDataFrame, testingDataFrame1)
}

setColumns = function(train_test_dataframe) {
  # Previous cases
  train_test_dataframe$casesLag4 = currentMOH$cases[circularIndex(train_test_dataframe$week, 4, 156)]
  train_test_dataframe$casesLag5 = currentMOH$cases[circularIndex(train_test_dataframe$week, 5, 156)]
  train_test_dataframe$casesLag6 = currentMOH$cases[circularIndex(train_test_dataframe$week, 6, 156)]
  
  # Previous temperatures
  train_test_dataframe$tempLag4 = temperature[circularIndex(train_test_dataframe$week, 4, 52)]
  train_test_dataframe$tempLag5 = temperature[circularIndex(train_test_dataframe$week, 5, 52)]
  train_test_dataframe$tempLag6 = temperature[circularIndex(train_test_dataframe$week, 6, 52)]
  train_test_dataframe$tempLag7 = temperature[circularIndex(train_test_dataframe$week, 7, 52)]
  train_test_dataframe$tempLag8 = temperature[circularIndex(train_test_dataframe$week, 8, 52)]
  train_test_dataframe$tempLag9 = temperature[circularIndex(train_test_dataframe$week, 9, 52)]
  train_test_dataframe$tempLag10 = temperature[circularIndex(train_test_dataframe$week, 10, 52)]
  
  #Factorized mobility values
  current.moh.name = unique(train_test_dataframe$moh_name)
  current.moh.year = unique(train_test_dataframe$year)
  
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
  train_test_dataframe$mobilityLag4 = mobilityMOH$cases[circularIndex(train_test_dataframe$week, 4, 156)]
  train_test_dataframe$mobilityLag5 = mobilityMOH$cases[circularIndex(train_test_dataframe$week, 5, 156)]
  train_test_dataframe$mobilityLag6 = mobilityMOH$cases[circularIndex(train_test_dataframe$week, 6, 156)]
  train_test_dataframe$mobilityLag7 = mobilityMOH$cases[circularIndex(train_test_dataframe$week, 7, 156)]
  train_test_dataframe$mobilityLag8 = mobilityMOH$cases[circularIndex(train_test_dataframe$week, 8, 156)]
  train_test_dataframe$mobilityLag9 = mobilityMOH$cases[circularIndex(train_test_dataframe$week, 9, 156)]
  
  return(train_test_dataframe)
}

plotIncidencesGraphML = function(area, predictions) {
  tempTestML = testingDataFrame[testingDataFrame$moh_name==area,]
  dataML = data.frame(week = tempTestML$week, predicted = predictions, actual = tempTestML$cases)
  R2 = 1 - (sum((dataML$actual-dataML$predicted )^2)/sum((dataML$actual-mean(dataML$actual))^2))
  RMSE = rmse(predicted = dataML$predicted, actual = dataML$actual)
  
  dmelt = melt(dataML, id = "week")
  title = paste("Dengue Incidences ", unique(tempTestML$year), " - ", area, " R-squared = ", round(R2, digits = 5), ", RMSE = ", round(RMSE, digits = 5))
  mohs = paste(array(unique(trainingDataFrame$moh_name)), collapse = ", ")
  trainedYears = paste(array(unique(trainingDataFrame$year)), collapse = ", ")
  cols = colnames(trainingDataFrame)
  caseLags = paste((gsub('casesLag([0-9]+).*', "\\1", cols[grep("casesLag", cols)])), collapse = ", ")
  temperatureLags = paste((gsub('tempLag([0-9]+).*', "\\1", cols[grep("tempLag", cols)])), collapse = ", ")
  mobilityLags = paste((gsub('mobilityLag([0-9]+).*', "\\1", cols[grep("mobilityLag", cols)])), collapse = ", ")
  subtitle = paste("Model Trained by ", trainedYears, " data with MOHs (", mohs, ") \nand case lags (", caseLags, ") , temperature lags (", temperatureLags, ") , mobility lags (", mobilityLags, ")")
  
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

###################################   ML - Run all at once     #########################
trainingDataFrame = data.frame()
testingDataFrame = data.frame()
incidencesPlotsML = list()
areas = c("MC - Colombo", "Dehiwala", "Maharagama", "Panadura", "Moratuwa", "Kaduwela", "Kollonnawa", "Boralesgamuwa", "Nugegoda", "Piliyandala", "Kelaniya", "Wattala", "Homagama")
areas = c("MC - Colombo", "Dehiwala", "Maharagama", "Panadura", "Moratuwa", "Kaduwela", "Kollonnawa", "Nugegoda", "Piliyandala", "Kelaniya", "Wattala")
areas = moh_in_galle[moh_in_galle %in% mohs_dengue12]
areas = moh_in_colombo[moh_in_colombo %in% mohs_population]
areas = moh_in_kandy[(moh_in_kandy %in% mohs_temperature) & 
                       (moh_in_kandy %in% mohs_population) &
                       (moh_in_kandy %in% mohs_mobility) &
                       (moh_in_kandy %in% mohs_dengue12) &
                       (moh_in_kandy %in% mohs_dengue13)
                     ]
for (mohName in areas) {
  cat("***************** ",mohName, "   ******************", fill = T)
  setTrainingAndTestML(mohName = mohName)
}

mlModel = trainTheMLmodel(depth = 12, rounds = 1000)

for (index in 1:length(areas)) {
  area = areas[index]
  predictions = testTheMLmodel(area = area, model = mlModel)
  incidencesPlotsML[[index]] = plotIncidencesGraphML(area = area, predictions = predictions)
}

#  Save plots
folderPath = file.path("images", "ml model", date)
dir.create(folderPath)
for (moh in areas) {
  incidencesPlot = incidencesPlotsML[[grep(moh, areas)]]
  
  path = file.path(folderPath, incidencesPlot$labels$title)
  
  ggsave(filename = paste(path, ".png", sep = ""), plot = incidencesPlot, width = 14.23, height = 8, units = "in", dpi = 96)
}

###################################   ML - Run all at once for seperate MOH areas     #########################
incidencesPlotsForSeperateMOHs = list()
areas = c("MC - Colombo", "Dehiwala", "Maharagama", "Panadura", "Moratuwa", "Kaduwela", "Kollonnawa", "Boralesgamuwa", "Nugegoda", "Piliyandala", "Kelaniya", "Wattala", "Homagama")
areas = moh_in_colombo[moh_in_colombo %in% mohs_population]
index = 1
for (mohName in areas) {
  trainingDataFrame = data.frame()
  testingDataFrame = data.frame()
  cat("***************** ",moh, "   ******************")
  setTrainingAndTestML(mohName = mohName)
  
  mlModel = trainTheMLmodel(depth = 10, rounds = 1000)
  
  predictions = testTheMLmodel(area = mohName, model = mlModel)
  incidencesPlotsForSeperateMOHs[[index]] = plotIncidencesGraphML(area = mohName, predictions = predictions)
  index = index + 1
}



############################# Testings  ##################

#([0-9]{4})_([0-9])(_[0-9])?
testRegExp = paste((gsub('tempLag([0-9]+).*', "\\1", cols[grep("tempLag", cols)])), collapse = ", ")


