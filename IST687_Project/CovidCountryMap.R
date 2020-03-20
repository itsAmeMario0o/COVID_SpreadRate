#Plot the data for number of Confirmed cases by country
#Initialize the libraries and prep world map data
library(ggplot2)
library(ggmap)
world_map <- map_data("world")
#Plot of the world map to see the syntax
#ggplot(world_map, aes(x = long, y = lat, map_id=region)) +
#  coord_map(xlim=c(-180,180), ylim = c(-58,90)) +
#  geom_map(map=world_map, fill="lightgray", colour = "white")

hist(dfCountries$`2020-03-09`)
ggplot(dfCountries) + 
  geom_col(
    aes(x = reorder(Country.Region,X3.9.20), y= log(X3.9.20)),
    color="black",
    fill="lightgray") + coord_flip()

#Test scenario, clean up later.
#test1 <- world_map[1:50,]
#test2 <- dfCountries
#test3 <- merge(world_map, dfCountries, by.x="Region", by.y="Country")

#Create vector of all countries listed on the world map
worldCountries <- unique(world_map$region)

#TESTING TO CLEAN THE DATA
#Create empty dataframe to store results
#dfWorld <- world_map[1,]
#dfWorld <- world_map[-1,]
#Iterate through unique countries and add to the dataframe
#for (x in worldCountries){
 # tempdf <- data.frame(t(colMeans(world_map[world_map$region == x,][,-c(5,6)])),
  #                     "region"=x,
   #                    "subregion"="NA")
  #dfWorld <- rbind(dfWorld, tempdf)
#}
#dfWorld <- NULL
#colMeans(world_map[world_map$region == x,][,-c(5,6)])
###END OF TEST FUNCTION

#From the list of all world map countries, merge data from covid cases
worldCountries <- data.frame(worldCountries)
covid.Countries <- merge(worldCountries,dfCountries,by.x="worldCountries", by.y="Country", all.x = TRUE)

##TESTING: WANTED TO CHECK IN EXCEL THE DIFFERENCE IN NAMES BETWEEN COUNTRIES
#order(dfCountries$Country)
#write.csv(dfCountries, "D:/Data/School/Graduate School/Syracuse/Q2/IST687 - Applied Data Science/covid.csv")

#Check the difference between World Map Countries and Data listed countries
unique(covid.Countries$worldCountries)
cleanCovid <- na.omit(covid.Countries)
dfCountries$Country[!dfCountries$Country %in% unique(cleanCovid$worldCountries)]

#Clean up data, match country names between ggmaps and covid countries
dfCountries[dfCountries$Country == "Mainland China",1] <- "China"
dfCountries[dfCountries$Country == "US",1] <- "USA"
dfCountries[dfCountries$Country == "Vatican City",1] <- "Vatican"
dfCountries[dfCountries$Country == "Northern Macedonia",1] <- "Macedonia"
covid.Countries <- merge(worldCountries,dfCountries,by.x="worldCountries", by.y="Country", all.x = TRUE)

#Plot world map with confirmed cases by country
my_breaks = c(1,10,100,1000,10000,80000)
map.World <- ggplot(covid.Countries, aes(map_id=worldCountries)) + 
  geom_map(map = world_map, color="lightgrey", size=0.5, aes(fill=covid.Countries$`2020-03-09`)) +
  coord_map(xlim=c(-180,180), ylim = c(-58,90)) +
  #Changed scale fill to include log transformation and custom breaks to highlight numbers on map.
  scale_fill_gradient(name="Confirmed Cases", trans="log", breaks = my_breaks, labels = my_breaks) +
  ggtitle("Covid-19 Cases by Country", subtitle="Data from 2020-03-09")
map.World