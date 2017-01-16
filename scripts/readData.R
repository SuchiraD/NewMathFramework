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
mobilityTrips2013 = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Mobility/weekly_mobility_trips_top10.csv", data.table = F, header = T)
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
