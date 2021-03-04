#download packages
if(!require(readr)) install.packages("readr")
if(!require(dplyr)) install.packages("dplyr")
if(!require(DT)) install.packages("DT")
if(!require(ggrepel)) install.packages("ggrepel")
if(!require(leaflet)) install.packages("leaflet")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(statsr)) install.packages("statsr")
if(!require(grid)) install.packages("grid")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(GGally)) install.packages("GGally")

#Load packages
library(readr)
library(DT)
library(ggrepel)
library(leaflet)
library(ggplot2)
library(dplyr)
library(statsr)
library(leaps)
library(grid)
library(gridExtra)
library(GGally)

#read csv file
path <- "C:\\Users\\Bryant\\Documents\\csc177ProjectFolder\\Police_Reports.csv"
data_file <- read_csv(path)

#Display all of the Data as a datatable
data_file_sub <- data_file[1:100,]
data_file_sub$Time <- as.character(data_file_sub$Time)
datatable(data_file_sub, options = list(pageLength = 5, scrollX='400px'))

#Tell us how many rows (incident reports) are in the data
sprintf("Number of Rows in Dataframe: %s", format(nrow(data_file),big.mark = ","))

#Preprocess Data:
#Turn the all-caps data into a more readable, proper case in the appropriate columns.
proper_case <- function(x) {
  return (gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", x, perl=TRUE))
}

data_file <- data_file %>% mutate(Category = proper_case(Category),
                    Descript = proper_case(Descript),
                    PdDistrict = proper_case(PdDistrict),
                    Resolution = proper_case(Resolution),
                    Time = as.character(Time))
data_file_sub <- data_file[1:100,]
datatable(data_file_sub, options = list(pageLength = 5, scrollX='400px'))


#Visualize Data
#Crime across space
#Display crime incident locations on the map using leaflet. Click icons on the map to show incident details.
library(leaflet)

data <- data_file[1:10000,] # display the first 10,000 rows
data$popup <- paste("<b>Incident #: </b>", data$IncidntNum, "<br>", "<b>Category: </b>", data$Category,
                    "<br>", "<b>Description: </b>", data$Descript,
                    "<br>", "<b>Day of week: </b>", data$DayOfWeek,
                    "<br>", "<b>Date: </b>", data$Date,
                    "<br>", "<b>Time: </b>", data$Time,
                    "<br>", "<b>PD district: </b>", data$PdDistrict,
                    "<br>", "<b>Resolution: </b>", data$Resolution,
                    "<br>", "<b>Address: </b>", data$Address,
                    "<br>", "<b>Longitude: </b>", data$X,
                    "<br>", "<b>Latitude: </b>", data$Y)

leaflet(data, width = "100%") %>% addTiles() %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(provider = "Esri.WorldStreetMap",group = "World StreetMap") %>%
  addProviderTiles(provider = "Esri.WorldImagery",group = "World Imagery") %>%
  # addProviderTiles(provider = "NASAGIBS.ViirsEarthAtNight2012",group = "Nighttime Imagery") %>%
  addMarkers(lng = ~X, lat = ~Y, popup = data$popup, clusterOptions = markerClusterOptions()) %>%
  addLayersControl(
    baseGroups = c("OSM (default)","World StreetMap", "World Imagery"),
    options = layersControlOptions(collapsed = FALSE)
  )


data_file_theft <- data_file %>% filter(grepl("Larceny/Theft", Category))





get_hour <- function(x) {
  return (as.numeric(strsplit(x,":")[[1]][1]))
}


dow_format <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
hour_format <- c(paste(c(12,1:11),"AM"), paste(c(12,1:11),"PM"))

#Arrest Over Time
data_file_arrest <- data_file %>% filter(grepl("Arrest", Resolution))

#correlation analysis
#factor by crime category
data_file_top_crimes <- data_file_arrest %>%
  group_by(Category) %>% 
  summarize(count = n()) %>%
  arrange(desc(count))

datatable(data_file_top_crimes, options = list(pageLength = 10,scrollX='400px'))

#assault/drug&narcotics/warrants [2:4] crimes count per hour of the day, for day of week
data_file_arrest_time_crime <- data_file_arrest %>%
  filter(Category %in% data_file_top_crimes$Category[2:4]) %>%
  mutate(Hour = sapply(Time, get_hour)) %>%
  group_by(Category, DayOfWeek, Hour) %>% 
  summarize(count = n())

data_file_arrest_time_crime$DayOfWeek <- factor(data_file_arrest_time_crime$DayOfWeek, level = rev(dow_format))
data_file_arrest_time_crime$Hour <- factor(data_file_arrest_time_crime$Hour, level = 0:23, label = hour_format)

datatable(data_file_arrest_time_crime, options = list(pageLength = 10,scrollX='400px'))

#get heatmap of above
plot <- ggplot(data_file_arrest_time_crime, aes(x = Hour, y = DayOfWeek, fill = count)) +
  geom_tile() +
  # fte_theme() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6, size = 4)) +
  labs(x = "Hour of Arrest (Local Time)", y = "Day of Week of Arrest", title = "Number of Police Arrests in San Francisco from 2007 - 2016, by Category and Time of Arrest") +
  scale_fill_gradient(low = "white", high = "#2980B9") +
  facet_wrap(~ Category, nrow = 6)
plot

#Good, but the gradients aren't helpful because they are not normalized. 
#We need to normalize the range on each facet. (unfortunately, this makes 
#the value of the gradient unhelpful)
#normalize the above 2 steps
data_file_arrest_time_crime <- data_file_arrest_time_crime %>%
  group_by(Category) %>%
  mutate(norm = count/sum(count))

datatable(data_file_arrest_time_crime, options = list(pageLength = 10,scrollX='400px'))

#plot the normalized data on heat map
plot <- ggplot(data_file_arrest_time_crime, aes(x = Hour, y = DayOfWeek, fill = norm)) +
  geom_tile() +
  # fte_theme() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6, size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Hour of Arrest (Local Time)", y = "Day of Week of Arrest", title = 
            "Police Arrests for Assualt, Drug/Narcotic, and Warrants in San 
  Francisco from 2003 - 2018 by Time of Arrest, Normalized by Type of Crime") +
  scale_fill_gradient(low = "white", high = "#215451") +
  facet_wrap(~ Category, nrow = 6)
plot






#assault/drug&narcotics/warrants [2:4] crimes count per hour of the day, for day of week
data_file_arrest_time_crime <- data_file_arrest %>%
  filter(Category %in% data_file_top_crimes$Category[2:4]) %>%
  mutate(Hour = sapply(Time, get_hour)) %>%
  group_by(Category, DayOfWeek, Hour) %>% 
  summarize(count = n())

data_file_arrest_time_crime$DayOfWeek <- factor(data_file_arrest_time_crime$DayOfWeek, level = rev(dow_format))
data_file_arrest_time_crime$Hour <- factor(data_file_arrest_time_crime$Hour, level = 0:23, label = hour_format)

datatable(data_file_arrest_time_crime, options = list(pageLength = 10,scrollX='400px'))


#Factor by Police District
#Same as above, but with a different facet.
data_file_arrest_time_district <- data_file_arrest %>%
  filter(Category %in% data_file_top_crimes$Category[2:4]) %>%
  mutate(Hour = sapply(Time, get_hour)) %>%
  group_by(PdDistrict, Category, DayOfWeek, Hour) %>% 
  summarize(count = n()) %>%
  group_by(PdDistrict, Category) %>%
  mutate(norm = count/sum(count))

data_file_arrest_time_district$DayOfWeek <- factor(data_file_arrest_time_district$DayOfWeek, level = rev(dow_format))
data_file_arrest_time_district$Hour <- factor(data_file_arrest_time_district$Hour, level = 0:23, label = hour_format)

datatable(data_file_arrest_time_district, options = list(pageLength = 10,scrollX='400px'))


#plot the police district arrests on heatmap
plot <- ggplot(data_file_arrest_time_district, aes(x = Hour, y = DayOfWeek, fill = norm)) +
  geom_tile() +
  # fte_theme() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6, size = 9,), plot.title = element_text(hjust = 0.5)) +
  labs(x = "Hour of Arrest (Local Time)", y = "Day of Week of Arrest", title = "Police Arrests for assault, 
            drug/narcotics, and warrants in San Francisco from 2003 - 2018 by Time of Arrest, Normalized by 
            Station and type of crime") +
  scale_fill_gradient(low = "white", high = "#215451") +
  facet_wrap(~ PdDistrict, nrow = 5)
plot






#download packages
if(!require(readr)) install.packages("readr")
install.packages("randomForest")

#load packages
library(readr)
Sys.setenv('R_MAX_VSIZE'=32000000000)
#read csv file
path <- "/Users/Chavo/Google Drive/School/CSC 177/Police_ReportsSmall.csv"
df <- read_csv(path)
data <- read.csv(path, header = TRUE)
str(data)
data$PdDistrict <- as.factor(data$PdDistrict)
t1 <- table(data$PdDistrict)


#sort data in decreasing order
sort(t1,decreasing = TRUE)
set.seed(5)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]


# Random Forest
library(randomForest)
rf <- randomForest(PdDistrict~.,data=train, ntree =1000)
rf
importance(rf)

