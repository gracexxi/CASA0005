library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)
library(dplyr)
library(janitor)
install.packages("viridisLite")
library(viridisLite)


#############################
# England confirmed cases by regions
## Read number of cumulative cases in England and select values on 1/7/2021
EngData_region <- read.csv("gis_data/region_2021-01-07.csv",
                           header = TRUE, 
                           sep = ",",  
                           encoding = "latin1")%>%
  dplyr::filter(date == "2021-01-07")
summary(EngData_region)

## Read the statistical gis boundary shape file 
Eng_outline_region <- st_read("gis_data/Regions__December_2017__Boundaries-shp/Regions__December_2017__Boundaries.shp")
summary(Eng_outline_region)

## Claen names
EngData_region <- clean_names(EngData_region)

## Join Confirmed data to some boundaries
EngDataMap_region <- Eng_outline_region %>% 
  clean_names() %>%
  left_join(., 
            EngData_region,
            by = c("rgn17cd" = "area_code"))
summary(EngDataMap_region)

## Add a basemap from OSM and create a termed bounding box
tmapEng <- EngDataMap_region %>%
  st_bbox(.) %>% 
  tmaptools::read_osm(., type = "osm", zoom = NULL)

## tmap mode set to plotting
tmap_mode("plot")
tm1 <- 
  tm_shape(tmapEng)+
  tm_rgb()+
  tm_shape(EngDataMap_region) + 
  tm_polygons("cum_cases_by_publish_date", 
              style="jenks",
              palette=cividis(n=5, direction=-1),
              midpoint=NA,
              title="Cumulative cases by 1/7/2021",
              alpha = 0.7) + 
  tm_compass(position = c("right", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("right", "bottom")) +
  tm_layout(title = "England Confirmed cases by regions", legend.position = c("left", "bottom"))
tm1


#############################
# UK confirmed cases by nations
## Read number of cumulative cases in the UK and select values on 1/7/2021
UKData_nation <- read.csv("gis_data/nation_2021-01-07.csv",
                          header = TRUE, 
                          sep = ",",  
                          encoding = "latin1")%>%
  dplyr::filter(date == "2021-01-07")
summary(UKData_nation)

## Read the statistical gis boundary shape file 
UK_outline_nation <- st_read("gis_data/Countries__December_2018__Boundaries_UK_BFC-shp/Countries__December_2018__Boundaries_UK_BFC.shp")
summary(UK_outline_nation)

## Claen names
UKData_nation <- clean_names(UKData_nation)

## Join Confirmed data to some boundaries
UKDataMap_nation <- UK_outline_nation %>% 
  clean_names() %>%
  left_join(., 
            UKData_nation,
            by = c("ctry18cd" = "area_code"))
summary(UKDataMap_nation)

## Add a basemap from OSM and create a termed bounding box
tmapUK <- UKDataMap_nation %>%
  st_bbox(.) %>% 
  tmaptools::read_osm(., type = "osm", zoom = NULL)

## tmap mode set to plotting
tmap_mode("plot")
tm2 <- 
  tm_shape(tmapUK)+
  tm_rgb()+
  tm_shape(UKDataMap_nation) + 
  tm_text("area_name", xmod=-1, ymod=-0.5, scale = .6)+
  tm_symbols(col = "red", scale = .5)+
  tm_polygons("cum_cases_by_publish_date", 
              style="jenks",
              palette=cividis(n=5, direction=-1),
              midpoint=NA,
              title="Cumulative cases by 1/7/2021",
              alpha = 0.7) + 
  tm_compass(position = c("right", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("right", "bottom")) +
  tm_layout(title = "UK Confirmed cases by nations", legend.position = c("left", "bottom"))
tm2


################################
# London confirmed cases by Boroughs
## Read number of cumulative cases in London and select values on 1/7/2021
Confirmed_full <- read_csv(url("https://data.london.gov.uk/download/coronavirus--covid-19--cases/151e497c-a16e-414e-9e03-9e428f555ae9/phe_cases_london_boroughs.csv"))

Confirmed_Newest <- subset(Confirmed_full, date=="2021/1/7")

## Read the statistical gis boundary shape file 
LondonData_borough <- st_read(here::here("gis_data",
                                         "statistical-gis-boundaries-london", 
                                         "ESRI", 
                                         "London_Borough_Excluding_MHW.shp"))%>%
  st_transform(., 27700)
summary(LondonData_borough)

## Create a new row of City of London
new_row1 <- subset(Confirmed_Newest,area_name == "Hackney and City of London",select = c("area_name", "area_code", "date", "new_cases", "total_cases"))
new_row1 <- replace(new_row1,1,"City of London")
new_row1 <- replace(new_row1,2,"E09000001")

## Add the new row to the confirmed cases
Confirmed_Newest <- Confirmed_Newest[-c(33),]
Confirmed_Newest <- rbind(Confirmed_Newest, new_row1)

## Load the London Borough coordinates and select the lat and long
Coor <- read_csv("gis_data/borough_coordinates.csv")%>%
  dplyr::select(`latitude`, `longitude`)

## Merge two csv and make it into the spatial data
Confirmed_Newest <- cbind(Confirmed_Newest, Coor)%>%
  st_as_sf(., coords = c("longitude", "latitude"), 
           crs = 4326) %>%
  st_transform(., 27700)

## make a function for the join
Joinfun <- function(data1, data2, data3){
  output<- data1%>%
    st_join(data2,.)%>%
    add_count(GSS_CODE, name = data3) 
  return(output)
}

## Use the function for confirmed COVID-19 cases
Confirmed_newest <- Joinfun(Confirmed_Newest, LondonData_borough, "confirmed")

LondonData <- read_csv("https://files.datapress.com/london/dataset/ward-profiles-and-atlas/2015-09-24T14:21:24/ward-profiles-excel-version.csv", 
                       locale = locale(encoding = "latin1"))
LondonData <- clean_names(LondonData)

EW <- st_read("https://opendata.arcgis.com/datasets/8edafbe3276d4b56aec60991cbddda50_2.geojson")
LondonDataMap <- EW %>%
  clean_names()%>%
  filter(str_detect(lad15cd, "^E09"))%>%
  merge(.,
        LondonData, 
        by.x="lad15cd", 
        by.y="new_code",
        no.dups = TRUE)%>%
  distinct(.,lad15cd, 
           .keep_all = TRUE)

tmap_mode("plot")
tmaplondon <- LondonDataMap %>%
  st_bbox(.) %>% 
  tmaptools::read_osm(., type = "osm", zoom = NULL)

breaks = c(0,5000,10000,15000,20000,25000)

tm3 <- 
  tm_shape(tmaplondon)+
  tm_rgb()+
  tm_shape(Confirmed_newest) + 
  tm_polygons(col = "total_cases", 
              breaks = breaks,
              style="jenks",
              midpoint=NA,
              title="Cumulative cases by 1/7/2021",
              palette=cividis(n=5, direction=-1),
              alpha = 0.6) +
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "London Confirmed cases by boroughs",legend.position = c("right", "bottom"))
tm3

