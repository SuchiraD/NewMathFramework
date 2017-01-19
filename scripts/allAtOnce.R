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


#Run all areas at once
results = data.frame()
test = data.frame()
areas = c("MC - Colombo", "Dehiwala", "Maharagama", "Panadura", "Moratuwa", "Kaduwela", "Kollonnawa", "Boralesgamuwa", "Nugegoda", "Piliyandala", "Kelaniya", "Wattala", "Homagama")
areas = c("MC - Colombo", "Dehiwala", "Maharagama", "Panadura", "Moratuwa")
for (moh in areas) {
  cat("***************** ",moh, "   ******************")
  resultLocation = paste("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/DenguenatorAnalysis/results/",moh,"-", date, sep = '')
  setTrainingAndTest(resultLocation = resultLocation, testLocation = resultLocation, mohName = moh)
}

modelForParamA = trainTheModel(depth = 10)
area = areas[1]
testTheModel(area = area, model = modelForParamA)

predictSEIR(area = area)
incidencesPlot = plotIncidencesGraph(area = area)
incidencesPlot

## Save the last graph
ggsave(filename = "CMC.png", width = 14.23, height = 8, units = "in", dpi = 96)
