#Histogram of 10 countries with most confirmed cases
orderedcovid <- covid.Countries[order(-covid.Countries$`2020-03-18`)[1:10],]
                                
ggplot(orderedcovid) + 
  geom_col(aes(x = factor(reorder(orderedcovid$worldCountries, 
                          orderedcovid$`2020-03-18`)), 
               y = orderedcovid$`2020-03-18`,
               fill = orderedcovid$`2020-03-18`)) + 
  geom_text(aes(x = factor(reorder(orderedcovid$worldCountries, 
                                   orderedcovid$`2020-03-18`)), 
                y = (1.7*orderedcovid$`2020-03-18`),
                label = orderedcovid$`2020-03-18`)) +
  scale_y_log10(breaks=c(1,10,100,1000,10000,80000)) +
  scale_fill_gradient(trans="log", guide=FALSE) +
  coord_flip() +
  ylab("Confirmed Cases") +
  xlab(NULL) +
  ggtitle("Countries with Highest Confirmed Cases", subtitle="Data from 2020-03-18")
