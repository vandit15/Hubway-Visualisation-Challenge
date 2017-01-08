#### load data
loadData <- function() {
  missingTypes <- c(NA, '', ' ')
  trips <<- read.csv('./data//hubway_trips.csv', na.strings=missingTypes, stringsAsFactors=FALSE)
  stations <<- read.csv('./data//hubway_stations.csv', na.strings=missingTypes, stringsAsFactors=FALSE)  
}

#### load libraries
usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

loadLibraries <- function() {
  usePackage('maps')
  usePackage('ggmap')
  usePackage('ggplot2')
  usePackage('dplyr')
}

#### casting data types
casteDataTypeForTripsDF <- function(tripsDF) {
  tripsDF$seq_id <- NULL
  tripsDF$status <- NULL
  tripsDF$start_date <- strptime(tripsDF$start_date, format='%m/%d/%Y %H:%M:%S')
  # tripsDF$strt_statn <- as.factor(tripsDF$strt_statn)
  tripsDF$end_date <- strptime(tripsDF$end_date, format='%m/%d/%Y %H:%M:%S')
  # tripsDF$end_statn <- as.factor(tripsDF$end_statn)
  tripsDF$bike_nr <- as.factor(tripsDF$bike_nr)
  tripsDF$subsc_type <- as.factor(tripsDF$subsc_type)
  # tripsDF$zip_code <- gsub("'", '', tripsDF$zip_code)
  tripsDF$zip_code <- NULL
  tripsDF$birth_date <- as.factor(tripsDF$birth_date)
  tripsDF$gender <- as.factor(tripsDF$gender)
  return(tripsDF)
}

casteDataTypeForStations <- function(stationsDF) {
  stationsDF$terminal <- as.factor(stationsDF$terminal)
  stationsDF$station <- as.factor(stationsDF$station)
  stationsDF$municipal <- as.factor(stationsDF$municipal)
  stationsDF$status <- as.factor(stationsDF$status)
  return(stationsDF)
}

#### remove rows with missing stations info
removeRowsMissStns <- function(tripsDF) {
  tripsDF <- subset(tripsDF, !is.na(strt_statn) & !is.na(end_statn))
  return(tripsDF)
}
## get Boston map
getBostonMap <- function(maptype='roadmap', zoom=12, color='bw') {
  longitude <- -71.075
  latitude <- 42.36
  lonlat <- c(longitude, latitude)
  maptype <- 'roadmap'
  # boston <- get_map(location="boston", zoom=12)
  boston <- get_map(location = lonlat, 
                    zoom=zoom, maptype=maptype, color=color) 
  boston <- ggmap(boston)
  return(boston)
}

## plot stations
plotStations <- function(plot) {
  plot <- plot + 
    geom_point(data=stations, aes(x=lng, y=lat))
  return(plot)
}


## add longitude and latitude info for starting stations
addStartLocs <- function(df) {
  df$strtLng <- stations$lng[match(df$strt_statn, stations$id)]
  df$strtLat <- stations$lat[match(df$strt_statn, stations$id)]
  return(df)  
}

## add longitude and latitude info for ending stations
addEndLocs <- function(df) {
  df$endLng <- stations$lng[match(df$end_statn, stations$id)]
  df$endLat <- stations$lat[match(df$end_statn, stations$id)]
  return(df)
}


## add longitude and latitude information to start and end stations
addStartAndEndLocs <- function(trAggDF) {
  trAggDF <- addStartLocs(trAggDF)
  trAggDF <- addEndLocs(trAggDF)
  return(trAggDF)
}

## add station locations
addStnLocs <- function(df) {
  df$lng <- stations$lng[match(df$statn, stations$id)]
  df$lat <- stations$lat[match(df$statn, stations$id)]
  return(df)
}

## add total station in and out 
addStnInandOut <- function(df,df2)
{
  df$Tin <- table(df2$end_statn)
  df$Tout <- table(df2$strt_statn)
  df$Tin <- as.integer(df$Tin)
  df$Tout <- as.integer(df$Tout)
  return(df)
}

## ploting total incoming bikes
plotbyTtlIn<-function(plot)
{
  plot <- plot + geom_point(data = stations, aes(x=lng,y=lat,col=Tin))
}

## ploting total outgoing bikes
plotbyTtlOut<-function(plot)
{
  plot <- plot + geom_point(data = stations, aes(x=lng,y=lat,col=Tout))
}

## plotting incoming bikes by municipal
InMunicipal <-function()
{
  InMunicipal<-tapply(stations$Tin,stations$municipal,sum)
  barplot(InMunicipal)
}

## comparing duration for registered and casual members
Durtn <- function(df)
{
  Durtn <- tapply(trips$duration,trips$subsc_type,mean)
  barplot(Durtn)
}


## Plot Top stations for female
TopFstns <- function(df,boston)
{
  tripsF <- subset(df,df$gender == "Female")
  stationsF <- addStnInandOut(stations,tripsF)
  plot <- boston + geom_point(data = stationsF, aes(x=lng,y=lat,col=(Tout+Tin)/2))
  return(plot)
}

## Plot Top stations for male
TopMstns <- function(df)
{
  tripsM <- subset(df,df$gender == "Male")
  stationsM <- addStnInandOut(stations,tripsM)
  plot <- boston + geom_point(data = stationsM, aes(x=lng,y=lat,col=(Tout+Tin)/2))
  return(plot)
}



## function calling
loadLibraries()

