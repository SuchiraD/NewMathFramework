require(data.table)
require("sfsmisc")

#Read "dengueCases2013_2014.csv"
dengue2013_2014 = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Dengue/dengueCases2013_2014.csv", data.table = F, header = T, col.names = c("id", "MOH_name", c(1:104), "Total"))

#Read "dengueCases2014.csv"
dengue2014 = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Dengue/dengueCases2014.csv", data.table = F, header = F, col.names = c("id", "MOH_name", c(1:52), "Total"), stringsAsFactors = F)

#Read "dengueCases2013.csv"
dengue2013 = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Dengue/dengueCases2013.csv", data.table = F, header = F, col.names = c("id", "MOH_name", c(1:52), "Total"), stringsAsFactors = F)

#Read "dengueCases2012.csv"
dengue2012 = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Dengue/dengueCases2012.csv", data.table = F, header = T, col.names = c("id", "MOH_name", c(1:52), "Total"), stringsAsFactors = F)
dengue2012 = dengue2012[-c(239,222, 281),]

currentMOH$cases= melt(dengue2014[181,][3:54])$value
currentMOH$cases = currentMOH$cases/0.02

#Read "temp.csv"
colomboTempArea = 138
dehiwalaTempArea = 296
temperatureData2013 = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Met_data/temp/temp.csv", data.table = F, header = T, stringsAsFactors = F)
temperature = melt(temperatureData2013[tempArea,][,3:54])$value

#Read rainFall 
rainfallData2013 = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Application/R/ANN/data/rainfall2013.csv", data.table = F, header = T, col.names = c("MOH_name", c(1:52)))
rainFall = melt(rainfallData2013[152,][,2:53])$value

#Read Population
populations = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Population/Estimated and Actual Populations in MOH's Srilanka2.csv", data.table = F, header = T, stringsAsFactors = F)
populations = data.frame(sapply(populations[1:2], as.numeric), populations[3], sapply(populations[4:6], as.numeric))
  
## Read districts with MOHs
districtsAndMOHs = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/MOH/districts and mohs.csv", data.table = F, header = T, stringsAsFactors = F)


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

simpleCap = function(x) {
  if(x=="MC - Colombo" || x=="Kalutara(North)" || x=="Beruwala(North)" || x=="MC - Galle") {
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

## Write processed mobility trips
write.csv(x = mobilityTripsFactorized, file = "/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Mobility/mobilityTripsFactorizedWithReportingRate0.04.csv", row.names = FALSE)

## Read Factorized mobility
mobilityTripsFactorized = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Mobility/mobilityTripsFactorizedWithReportingRate0.04.csv", data.table = F, header = T, stringsAsFactors = F)
mobilityTripsFactorized = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Mobility/mobilityTripsFactorized.csv", data.table = F, header = T, stringsAsFactors = F)


## Refactoring the raw vegetation index file
vegetationIndices = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Vegetation Index/mean_negative_removed.csv", data.table = F, header = T, stringsAsFactors = F)
columns = names(vegetationIndices)
vegetationIndices = data.frame(MOH_Area = sapply(vegetationIndices$MOH_Area, simpleCap), vegetationIndices[2:ncol(vegetationIndices)], stringsAsFactors = F, row.names=NULL)
colnames(vegetationIndices) = columns
write.csv(x = vegetationIndices, file = "/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Vegetation Index/", row.names = FALSE)
#names(vegetationIndices)[2:ncol(vegetationIndices)] = str_extract(string = columns[2:length(columns)], pattern = "([0-9]{4})_([0-9])(_[0-9])?")

## Read vegetation indices
vegetationIndices = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Vegetation Index/mean_negative_removed.csv", data.table = F, header = T, stringsAsFactors = F)
## Extract columns only with an year and a month
requiredCols = c(columns[1], str_extract(string = columns[2:length(columns)], pattern = "([0-9]{4})_([0-9]{1,2})(?!_)"))
vegetationIndices = vegetationIndices[,(columns %in% requiredCols)]

## Convert monthly vegetation indices to weekly
vegetationIndicesWeekly = data.frame(MOH_name = NA, year = NA, stringsAsFactors = F)
vegetationIndicesWeekly[,3:54] = NA
names(vegetationIndicesWeekly) = c("MOH_name", "year", 1:52)
vegetationIndicesWeekly = vegetationIndicesWeekly[-1,]
names_vegindex = names(vegetationIndices)
year_month_list = strsplit(names_vegindex[2:length(names_vegindex)], "_")
names_as_dates = as.Date("2017-1-1")
for (i in 1:length(year_month_list)) {
  names_as_dates[i] = as.Date(paste(year_month_list[[i]][1], year_month_list[[i]][2], 1, sep="-"))
}
veg_temp_frame = data.frame(date = names_as_dates)
for (mohName in vegetationIndices$MOH_Area) {
  veg_temp_frame$veg_index = melt(vegetationIndices[vegetationIndices$MOH_Area==mohName,][,2:ncol(vegetationIndices)])$value
  findVegIndex = splinefun(veg_temp_frame)
  weeks = 1:52
  years = 2013:2015
  index = nrow(vegetationIndicesWeekly)+1
  for(year in years) {
    veg_index_weekly = findVegIndex(as.Date(paste(year, weeks, 1, sep="-"), "%Y-%U-%u"))
    vegetationIndicesWeekly[index,] = c(mohName, year, veg_index_weekly)
    index = index + 1
  }
}
write.csv(x = vegetationIndicesWeekly, file = "/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Vegetation Index/vegetationIndicesWeekly.csv", row.names = FALSE)

## Read weekly vegetation indices file
vegetationIndicesWeekly = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Vegetation Index/vegetationIndicesWeekly.csv", data.table = F, header = T, stringsAsFactors = F)
