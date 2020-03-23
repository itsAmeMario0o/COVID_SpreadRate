#Animation Slides
install.packages("gganimate")
library(gganimate)
install.packages("gifski")
library(gifski)
library(ggplot2)

###TEST CODE
#Plot world map with confirmed cases by country
#my_breaks = c(1,10,100,1000,10000,80000)
#map.WorldAni <- ggplot(covid.Countries, aes(map_id=worldCountries)) + 
#  geom_map(map = world_map, color="lightgrey", size=0.5, aes(fill=covid.Countries$`2020-03-18`)) +
#  coord_map(xlim=c(-180,180), ylim = c(-58,90)) +
#  #Changed scale fill to include log transformation and custom breaks to highlight numbers on map.
#  scale_fill_gradient(name="Confirmed Cases", trans="log", breaks = my_breaks, labels = my_breaks) +
#  ggtitle("Covid-19 Cases by Country", subtitle="Data from 2020-03-09") +
#  #gganimate specific bits:
#  labs(title="Date: {frame_time}") +
#  transition_time(date) + 
#  ease_aes('linear')
#map.WorldAni
###END OF TESTING CODE

###GATHER() DOCUMENTATION
# get first observation for each Species in iris data -- base R
##mini_iris <- iris[c(1, 51, 101), ]
## gather Sepal.Length, Sepal.Width, Petal.Length, Petal.Width
#gather(mini_iris, key = "flower_att", value = "measurement",
#       Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
###END OF GATHER DOCUMENTATION

#Rearrange data into long form
library(tidyr)
orderedcovid.long <- gather(orderedcovid, key="Date", value="Cases", -worldCountries)
orderedcovid.long$Date <- as.Date(orderedcovid.long$Date)

#Plot bar chart timeseries
column.Country3 <- ggplot(orderedcovid.long) + 
  geom_col(aes(x = factor(reorder(orderedcovid.long$worldCountries, 
                                  orderedcovid.long$Cases)), 
               y = orderedcovid.long$Cases,
               fill = orderedcovid.long$Cases)) + 
  geom_text(aes(x = factor(reorder(orderedcovid.long$worldCountries, 
                                   orderedcovid.long$Cases)), 
                y = 1.1, 
                label = as.integer(orderedcovid.long$Cases)), 
            color = "white",
            fontface = "bold",
            size = 18,
            hjust = 0) +
  scale_y_log10(breaks=c(1,10,100,1000,10000,80000)) +
  scale_fill_gradient(trans="log", guide=FALSE) +
  coord_flip() +
  ylab("Confirmed Cases") +
  xlab(NULL) +
  labs(title = "Countries with Highest Confirmed Cases", subtitle = 'Date: {frame_time}') +
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        plot.title = element_text(face="bold", size=25),
        plot.subtitle = element_text(size=15),
        axis.title = element_text(size=20)) +
  transition_time(Date)
column.Country3

column.CountryAni <- animate(column.Country3)

animate(column.Country3, 100, fps = 10,  width = 1280, height = 720, 
        renderer = gifski_renderer("gganim.gif"))