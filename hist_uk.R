library(ggplot2)
library(janitor)


## Inbound Tourist visits
inbound_tourist <- read.csv("gis_data/statistic_id287133_inbound-tourist-visits-to-the-united-kingdom--uk--2002-2021.csv")
# set up the basic histogram
ggplot(inbound_tourist, aes(x=year,y=visit)) + geom_bar(stat='identity')


## International Travel spending
international_tourist <- read.csv("gis_data/statistic_id287526_international-tourist-spending-in-the-united-kingdom--uk--2004-2021.csv")
# set up the basic histogram
ggplot(international_tourist, aes(x=year,y=spending)) + geom_bar(stat='identity')


## Domestic Tourist spending
domestic_tourist <- read.csv("gis_data/statistic_id296824_domestic-travel-spending-in-great-britain-2010-2020.csv")
# set up the basic histogram
ggplot(domestic_tourist, aes(x=year,y=spending)) + geom_bar(stat='identity')


## Daily impact on restaurant dinning
dinning <- read.csv("gis_data/statistic_id1104991_daily-impact-of-covid-19-on-restaurant-dining-in-the-uk-feb-dec-2020.csv")
# set up the basic histogram
ggplot(dinning, aes(x=date,y=change)) + 
  geom_bar(stat='identity')
  geom_histogram(aes(y = ..density..),
                 binwidth = 5) 

  
## Daily London tube use 
transport <- read.csv("gis_data/COVID-19-transport-use-statistics.csv",
                      header = TRUE, 
                      sep = ",",  
                      encoding = "latin1")
summary(transport)
# Sort the variable 
transport$Transport.for.London.Tube <- factor(transport$Transport.for.London.Tube, levels=unique(as.character(transport$Transport.for.London.Tube)))
summary(transport)
# Plot the scatter and add vertical lines
ggplot(transport, aes(x=X.ef..bb..bf.Date,
                           y=Transport.for.London.Tube)) +
  geom_point() +
  geom_vline(xintercept = "2020/3/23",
             color="red",
             linetype="dashed")+
  geom_vline(xintercept = "2020/6/1",
             color="blue",
             linetype="dashed")

