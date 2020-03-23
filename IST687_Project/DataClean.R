#Function used to clean the time-series data
dfRaw <- read.csv("time_series_19-covid-Confirmed.csv")
#Code for China for Test of Concept
#dfChina <- dfRaw[dfRaw$Country.Region == "Mainland China",]
#dfChinaTotal <- data.frame(Province.State="Total",
#                          Country.Region="Mainland China", 
#                          t(colSums(dfChina[,-c(1,2)])))

create_dfCountries <- function(){
  #List of all Countries
  allCountries <- unique(dfRaw$Country.Region)
  #Create empty dataframe to store results
  dfCountries <- dfRaw[1,]
  dfCountries <- dfCountries[-1,]
  #Iterate through unique countries and add to the dataframe
  for (x in allCountries){
    tempdf <- data.frame(Province.State="Total",
                         Country.Region=x,
                         t(colSums(dfRaw[dfRaw$Country.Region == x,][,-c(1,2)])),
                         stringsAsFactors=FALSE)
    dfCountries <- rbind(dfCountries, tempdf)
  }
  #Remove Province and Lat/Lon
  dfCountries <- dfCountries[,-c(1,3,4)]
  #Remove rows where no cases have been found as of 3/9/2020
  dfCountries <- dfCountries[dfCountries$X3.18.20 > 0,]
  colnames(dfCountries) <- gsub("X","",colnames(dfCountries))
  colnames(dfCountries) <- gsub("\\.","/",colnames(dfCountries))
  colnames(dfCountries) <- as.Date(colnames(dfCountries), format="%m/%d/%y")
  colnames(dfCountries)[1] <- "Country"
  return(dfCountries)
}

dfCountries <- create_dfCountries()
write.csv(worldCountries, "List of Map Countries")
