#Plot the data for number of Confirmed cases by State (USA)

#Importing data for the US covid cases by state map
covid.US <- read.csv("data-AlBNV.csv")
covid.US$Province.State <- tolower(covid.US$Province.State)
us <- map_data("state")

#Create map of the US with confirmed COVID cases
map.covid.US <- ggplot(covid.US, aes(map_id=Province.State)) + 
  geom_map(map = us, color="black", aes(fill=Confirmed)) +
  scale_fill_gradient(name="Confirmed Cases", trans="log", 
                      breaks=c(1,10,100,1000,2495)) +
  expand_limits(x = us$long, y = us$lat) + 
  xlab("Longitude") + ylab("Latitude") +
  coord_map() +
  ggtitle("COVID-19 Cases in US by State", subtitle = "Data from 2020-03-18")
map.covid.US

#Maybe can use slip on non-logarithmic map to compare and contrast the difficulties in showing the map data so that the information can be easily observed.
#"Even in a map of the US, where the number of cases between states is not as large as the global scale, linear scaling is still hard to read"
map.lin.covid.US <- ggplot(covid.US, aes(map_id=Province.State)) + 
  geom_map(map = us, color="black", aes(fill=Confirmed)) +
  scale_fill_gradient(name="Confirmed Cases") +
  expand_limits(x = us$long, y = us$lat) + 
  xlab("Longitude") + ylab("Latitude") +
  coord_map() +
  ggtitle("COVID-19 Cases in US by State", subtitle = "Data from 2020-03-18")
map.lin.covid.US
