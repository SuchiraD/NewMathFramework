date = "Thu Jan 05 20:45:29 IST 2017"
date = "Thu Jan 05 06:06:49 IST 2017" ## CMC RMSE = 21.96, R2 = 0.50
date = "Wed Jan 18 18:52:24 IST 2017" ##MC -Colombo with 200 iterations ------ RMSE = 22.7, R2 = 0.46 ----- RMSE = 22.08, R2 = 0.49 ---- RMSE = 20.83, R2 = 0.55


#Run all areas at once
results = data.frame()
test = data.frame()
areas = c("MC - Colombo", "Dehiwala", "Maharagama", "Panadura", "Moratuwa", "Kaduwela", "Kollonnawa", "Boralesgamuwa", "Nugegoda", "Piliyandala", "Kelaniya", "Wattala", "Homagama")
areas = c("MC - Colombo", "Dehiwala", "Maharagama", "Panadura", "Moratuwa")
areas = c("Dehiwala", "MC - Colombo", "Maharagama", "Kaduwela", "Moratuwa")
areas = c("MC - Colombo")
for (moh in areas) {
  cat("***************** ",moh, "   ******************")
  resultLocation = paste("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/",moh,"-", date, sep = '')
  setTrainingAndTest(resultLocation = resultLocation, testLocation = resultLocation, mohName = moh)
}

modelForParamA = trainTheModel(depth = 10)

incidencesPlotsCM = list()
imageIndex = 1
for (moh in areas) {
  predictSEIR(area = moh)
  incidencesPlotsCM[[imageIndex]] = plotIncidencesGraph(area = moh)
  imageIndex = imageIndex + 1
}

#  Save plots
dir.create(date)
for (moh in areas) {
  incidencesPlot = incidencesPlotsCM[[grep(moh, areas)]]
  
  path = file.path(date, incidencesPlot$labels$title)
  
  ggsave(filename = paste(path, ".png", sep = ""), plot = incidencesPlot, width = 14.23, height = 8, units = "in", dpi = 96)
}

## Run area by area
incidencesPlotsForSeperateMOHsCM = list()
areas = c("MC - Colombo", "Dehiwala", "Maharagama", "Panadura", "Moratuwa", "Kaduwela", "Kollonnawa", "Boralesgamuwa", "Nugegoda", "Piliyandala", "Kelaniya", "Wattala", "Homagama")
areas = c("MC - Colombo", "Dehiwala", "Maharagama", "Panadura", "Moratuwa")
areas = c("MC - Colombo")
index = 1
for (moh in areas) {
  results = data.frame()
  test = data.frame()
  cat("***************** ",moh, "   ******************")
  resultLocation = paste("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/",moh,"-", date, sep = '')
  setTrainingAndTest(resultLocation = resultLocation, testLocation = resultLocation, mohName = moh)
  modelForParamA = trainTheModel(depth = 10)
  testTheModel(area = moh, model = modelForParamA)
  predictSEIR(area = moh)
  incidencesPlotsForSeperateMOHsCM[[index]] = plotIncidencesGraph(area = moh)
  index = index + 1
}



## Save the last graph
ggsave(filename = "CMC.png", width = 14.23, height = 8, units = "in", dpi = 96)
