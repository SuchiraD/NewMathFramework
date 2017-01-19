require(data.table)
require("sfsmisc")

#Read "dengueCases2013_2014.csv"
dengue2013_2014 = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Dengue/dengueCases2013_2014.csv", data.table = F, header = T, col.names = c("id", "MOH_name", c(1:104), "Total"))

#Read "dengueCases2014.csv"
dengue2014 = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Dengue/dengueCases2014.csv", data.table = F, header = F, col.names = c("id", "MOH_name", c(1:52), "Total"))

#Read "dengueCases2013.csv"
dengue2013 = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Dengue/dengueCases2013.csv", data.table = F, header = F, col.names = c("id", "MOH_name", c(1:52), "Total"))
dengue.data = dengue2013

#Read "dengueCases2012.csv"
dengue2012 = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Dengue/dengueCases2012.csv", data.table = F, header = T, col.names = c("id", "MOH_name", c(1:52), "Total"))
dengue2012 = dengue2012[-c(239,222, 281),]
dengue.data = dengue2012

currentMOH$cases= melt(dengue2014[181,][3:54])$value
currentMOH$cases = currentMOH$cases/0.02

#Read "temp.csv"
colomboTempArea = 138
dehiwalaTempArea = 296

temperatureData2013 = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Met_data/temp/temp.csv", data.table = F, header = T)
tempArea=dehiwalaTempArea
temperature = melt(temperatureData2013[tempArea,][,3:54])$value

#Read rainFall 
rainfallData2013 = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/R/ANN/data/rainfall2013.csv", data.table = F, header = T, col.names = c("MOH_name", c(1:52)))
rainFall = melt(rainfallData2013[152,][,2:53])$value

#Read Population
populations = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Population/Estimated and Actual Populations in MOH's Srilanka2.csv", data.table = F, header = T)
populations = data.frame(sapply(populations[1:2], as.numeric), populations[3], sapply(populations[4:6], as.numeric))
  
#Read ISD
isd = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/ISD-Weather/SL-2006-2016/9009387163514dat.txt", data.table = F, header = T, sep=' ')


#Read results
mfrow = c(1,1)
par(mfrow=mfrow)                                              # divides the plot area into two up and two down
mult.fig(mfrow = mfrow,main="MetaPop model of Dengue")

#Read mobility
mobility2013 = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Mobility/weeklyMobility.csv", data.table = F, header = T)
mobility = data.frame(week = mobility2013$WEEK_NUMBER[2549:(2549+51)], mobility = mobility2013$MOBILITY_VALUE[2549:(2549+51)])

#Read mobility from Lasantha ayya
mobility2013 = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Mobility/mohMobilityWeekly2013_Lasantha.csv", data.table = F, header = T)
mobArea = 57 
## Colombo - 52
## Dehiwala - 57
mobility = data.frame(week = c(mobility2013$week[mobility2013$moh.id==mobArea], 49:52), 
                      mobility = c(mobility2013$mobility.value[mobility2013$moh.id==mobArea], (mobility2013$mobility.value[mobility2013$moh.id==mobArea])[45:48])
)

#Read mobility - trips
#mobilityTrips2013 = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Mobility/weekly_mobility_trips.csv", data.table = F, header = T)
mobilityTrips2013 = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Mobility/weekly_mobility_trips_top10.csv", data.table = F, header = T, stringsAsFactors = F)
class(mobilityTrips2013$WEEK_NUMBER) = 'numeric'
class(mobilityTrips2013$MOBILITY_VALUE) = "numeric"
mobilityTrips2013 = data.frame(MOH_NAME = sapply(mobilityTrips2013$MOH_NAME, simpleCap), HOME = sapply(mobilityTrips2013$HOME, simpleCap), mobilityTrips2013[3:4])
mobilityTrips2013$HOME_CASES = 0
getCases = function(x) {
  column = (as.integer(x[3])+2)
  dengue2013[dengue2013$MOH_name==x[2],][,column]
}
mobilityTrips2013$HOME_CASES = apply(mobilityTrips2013, 1, getCases)
class(mobilityTrips2013$HOME_CASES) = "numeric"
mobilityTrips2013[is.na(mobilityTrips2013)]= 0
mobilityTrips2013$MOBILITY_FACTOR = mobilityTrips2013$MOBILITY_VALUE*mobilityTrips2013$HOME_CASES

simpleCap = function(x) {
  if(x=="MC - Colombo" || x=="Kalutara(North)" || x=="Beruwala(North)") {
    return (x)
  }
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
        sep="", collapse=" ")
}

## Factorizing mobility
mobilityTrips2013 = mobilityTrips2013[(mobilityTrips2013$HOME %in% ALL_MOH_NAMES),]
getCases = function(x) {
  column = (as.integer(x[3])+2)
  return (dengue2012[dengue2012$MOH_name==x[2],][,column]/reportingRate)
}
mobilityTrips2013$HOME_CASES_2012 = apply(mobilityTrips2013, 1, getCases)
class(mobilityTrips2013$HOME_CASES_2012) = "numeric"

getCases = function(x) {
  column = (as.integer(x[3])+2)
  dengue2013[dengue2013$MOH_name==x[2],][,column]/reportingRate
}
mobilityTrips2013$HOME_CASES_2013 = apply(mobilityTrips2013, 1, getCases)
class(mobilityTrips2013$HOME_CASES_2013) = "numeric"

getCases = function(x) {
  column = (as.integer(x[3])+2)
  dengue2014[dengue2014$MOH_name==x[2],][,column]/reportingRate
}
mobilityTrips2013$HOME_CASES_2014 = apply(mobilityTrips2013, 1, getCases)
class(mobilityTrips2013$HOME_CASES_2014) = "numeric"

mobilityTrips2013[is.na(mobilityTrips2013)]= 0
mobilityTrips2013$HOME_CASES_FACTOR_2012 = mobilityTrips2013$MOBILITY_VALUE*mobilityTrips2013$HOME_CASES_2012
mobilityTrips2013$HOME_CASES_FACTOR_2013 = mobilityTrips2013$MOBILITY_VALUE*mobilityTrips2013$HOME_CASES_2013
mobilityTrips2013$HOME_CASES_FACTOR_2014 = mobilityTrips2013$MOBILITY_VALUE*mobilityTrips2013$HOME_CASES_2014

mobilityTripsFactorized = data.frame(MOH_NAME=character(0), WEEK_NUMBER=numeric(0), 
                                     HOME_CASES_FACTOR_2012=numeric(0),
                                     HOME_CASES_FACTOR_2013=numeric(0),
                                     HOME_CASES_FACTOR_2014=numeric(0),
                                     stringsAsFactors = F)
index = 1;
for(moh in unique(mobilityTrips2013$MOH_NAME)) {
  current.mobility.trips = mobilityTrips2013[mobilityTrips2013$MOH_NAME==moh,]
  for (week in unique(current.mobility.trips$WEEK_NUMBER)) {
    tempFrame = data.frame(MOH_NAME=moh, WEEK_NUMBER=week, 
                           HOME_CASES_FACTOR_2012=sum(current.mobility.trips$HOME_CASES_FACTOR_2012[current.mobility.trips$WEEK_NUMBER==week])/sum(current.mobility.trips$MOBILITY_VALUE[current.mobility.trips$WEEK_NUMBER==week]),
                           HOME_CASES_FACTOR_2013=sum(current.mobility.trips$HOME_CASES_FACTOR_2013[current.mobility.trips$WEEK_NUMBER==week])/sum(current.mobility.trips$MOBILITY_VALUE[current.mobility.trips$WEEK_NUMBER==week]),
                           HOME_CASES_FACTOR_2014=sum(current.mobility.trips$HOME_CASES_FACTOR_2014[current.mobility.trips$WEEK_NUMBER==week])/sum(current.mobility.trips$MOBILITY_VALUE[current.mobility.trips$WEEK_NUMBER==week]),
                           stringsAsFactors = F)
    mobilityTripsFactorized[index,] = tempFrame
    index = index+1
  }
}

## Read Factorized mobility
mobilityTripsFactorized = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Mobility/mobilityTripsFactorizedWithReportingRate0.04.csv", data.table = F, header = T, stringsAsFactors = F)



results = fread("myfile.csv", data.table = F, header = F, col.names = c(c(1:52), "Total"))
results[results < 0] = 0
plot(x = c(1:52), y = results[1,][1:52], lwd=3, xlab="Week",ylab="Total Infected", type = "l", col = "red", main = "Actual vs Predicted")
lines(x = c(1:52), y = results[2,][1:52], xlab = "", ylab = "", lwd=3, type = "l", col = "black")
axis(4)
#mtext("y2",side=4,line=3)
legend("topleft",col=c("red","black"),lty=1,legend=c("Actual","Predicted"))



#Plot diagrams
mfrow = c(2,1)
par(mfrow=mfrow)                                              # divides the plot area into two up and two down
mult.fig(mfrow = mfrow,main="SIR model of Dengue")

plot(x = c(1:52), y = currentMOH[3:54], lwd=3, xlab="Week",ylab="Total Infected", type = "l", col = "red", main = "Dengue 2013")
par(new = T)
plot(x = c(1:52), y = tempCsv[138,][3:54], xaxt="n",yaxt="n", xlab = "", ylab = "", lwd=3, type = "l", col = "blue")
axis(4)
#mtext("y2",side=4,line=3)
legend("topleft",col=c("red","blue"),lty=1,legend=c("Dengue 2013","Temperature"))

plot(c(1:52), dengue.data[323,][3:54], xlab = "Week", ylab = "Total Infected", type = "l", col = "red", lwd=3, main = paste("Dengue 2014 - ", dengue.data[323,]$MOH_name))

data <- dengue2014
for (variable in 1:NROW(data)) {
  if(max(data[variable,][3:54] > 15)) {
    plot(c(1:52), data[variable,][3:54], xlab = "Week", ylab = "Total Infected", type = "l", col = "black", lwd=3, main = paste("Dengue 2014 - ", data[variable,][2]))
  }
}
for(i in 3:54) {
  dengue.data[323, ][i] = 0
  dengue.data[323, ][i] = sum(dengue.data[i])
}




#Write CSV
write.csv(x = results1, file = "results1.csv", sep = ",", row.names = FALSE, col.names = TRUE)
write.csv(x = results2, file = "results2.csv", sep = ",", row.names = FALSE, col.names = TRUE)
write.csv(x = test, file = "results3.csv", sep = ",", row.names = FALSE, col.names = TRUE)
write.csv(x = mobilityTripsFactorized, file = "/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Mobility/mobilityTripsFactorizedWithReportingRate0.025.csv", row.names = FALSE)
