library(maps)
library(sf)
library(ggplot2)
library(mapproj)
library(dplyr)
library(rgdal)
library(stats)
library(raster)
options(scipen = 999)
install.packages("housingData")
library(housingData)



#Reading in the collected data
countydata <- read.csv("CountySamplingData.csv", TRUE, sep = ",",na.strings = TRUE, colClasses = "character")
countydata.sub <- countydata[countydata$State != "AK" & countydata$State != "HI", ]
dataf <- data.frame(countydata.sub)
dataf
dataf.sub <- dataf[dataf$State != "AK" & dataf$State != "HI", ]
dataf.sub$FIPS <- as.numeric(dataf.sub$FIPS)
dataf.sub


dataf.sub$Median.Household.Income <- gsub(",","",dataf$Median.Household.Income )
dataf.sub$Median.Household.Income <- as.numeric(dataf$Median.Household.Income)
meanHI <- mean(dataf.sub$Median.Household.Income)
round(meanHI, digits = 2)

dataf.sub$FIPS <- as.numeric(dataf.sub$FIPS)
dataf.sub$FIPS

# #Reading in the shapefile
counties <- readOGR(dsn = "UScounties")
counties$FIPS <- as.numeric(as.character(counties$FIPS))
counties.sub <- counties[(counties$STATE_NAME != "Alaska" & counties$STATE_NAME != "Hawaii"), ]
counties.sub@data$FIPS
counties.ordered <- counties.sub[order(counties.sub$FIPS), ]
counties.ordered <- counties.ordered[counties.ordered$NAME != "Bedford City", ]
nrow(counties.ordered)

combined.data <- inner_join(counties.ordered@data, dataf.sub, by = "FIPS")
combined.data <- subset(combined.data, select = -c(STATE_FIPS, CNTY_FIPS, State, Area.Name))

means <- replicate(100, mean(sample(final.data$Median.Household.Income, 50)))
hist(means, main = "Histogram of Median Household Income" ,xlab = "Median Household Income", col = "yellow")
abline(v = 50957.72, col = "red", lwd = 4)
abline(v = mean(means), col = "blue", lwd = 4, lty = "dotted")
legend(52500, 27, legend = c("Population", "Sample"), col = c("red", "blue"), lty = 1:2, cex = 0.8)


#Grouping by geographic region for stratification
counties.West <- counties.ordered[(counties.ordered$STATE_NAME == "Washington" | counties.ordered$STATE_NAME == "Oregon" | counties.ordered$STATE_NAME == "Idaho" | 
                                     counties.ordered$STATE_NAME == "Montana" | counties.ordered$STATE_NAME == "California" | counties.ordered$STATE_NAME == "Nevada" | 
                                     counties.ordered$STATE_NAME == "Utah" | counties.ordered$STATE_NAME == "Colorado" | counties.ordered$STATE_NAME == "Wyoming"), ] 
counties.southWest <- counties.ordered[(counties.ordered$STATE_NAME == "Arizona" | counties.ordered$STATE_NAME == "New Mexico" | counties.ordered$STATE_NAME == "Texas" |
                                          counties.ordered$STATE_NAME == "Oklahoma"), ] 
counties.midWest <- counties.ordered[(counties.ordered$STATE_NAME == "North Dakota" | counties.ordered$STATE_NAME == "South Dakota" | counties.ordered$STATE_NAME == "Nebraska" | 
                                     counties.ordered$STATE_NAME == "Kansas" | counties.ordered$STATE_NAME == "Minnesota" | counties.ordered$STATE_NAME == "Iowa" | 
                                     counties.ordered$STATE_NAME == "Missouri" | counties.ordered$STATE_NAME == "Wisconsin" | counties.ordered$STATE_NAME == "Illinois" |
                                       counties.ordered$STATE_NAME == "Michigan" | counties.ordered$STATE_NAME == "Indiana" | counties.ordered$STATE_NAME == "Ohio"), ]
counties.southEast <- counties.ordered[(counties.ordered$STATE_NAME == "Arkansas" | counties.ordered$STATE_NAME == "Louisiana" | counties.ordered$STATE_NAME == "Mississippi" | 
                                          counties.ordered$STATE_NAME == "Alabama" | counties.ordered$STATE_NAME == "Georgia" | counties.ordered$STATE_NAME == "Florida" | 
                                          counties.ordered$STATE_NAME == "South Carolina" | counties.ordered$STATE_NAME == "North Carolina" | counties.ordered$STATE_NAME == "Virginia" |
                                          counties.ordered$STATE_NAME == "West Virginia" | counties.ordered$STATE_NAME == "Tennessee" | counties.ordered$STATE_NAME == "Kentucky"), ] 
counties.northEast <- counties.ordered[(counties.ordered$STATE_NAME == "Maryland" | counties.ordered$STATE_NAME == "Delaware" | counties.ordered$STATE_NAME == "New Jersey" | 
                                          counties.ordered$STATE_NAME == "Pennsylvania" | counties.ordered$STATE_NAME == "New York" | counties.ordered$STATE_NAME == "Connecticut" | 
                                          counties.ordered$STATE_NAME == "Rhode Island" | counties.ordered$STATE_NAME == "Massachusetts" | counties.ordered$STATE_NAME == "Vermont" | 
                                          counties.ordered$STATE_NAME == "New Hampshire" | counties.ordered$STATE_NAME == "Maine" | counties.ordered$STATE_NAME == "District of Columbia"), ]


missingCounty <- data.frame("Shannon", "South Dakota", "46113", "7,341", "51.9", "27,804", "2161", "2251", "3421", "1080", "24", "25", "38", "12", "14,291", "11.7", "12.08", "10.3")
names(missingCounty) <- names(combined.data)
combined.data <- rbind(combined.data, missingCounty)
combined.data <- combined.data[order(combined.data$STATE_NAME),]
combined.data$Square_Kilometers <- round(area(counties.ordered)/1000000, digits = 2)
combined.data$Population <- gsub(",", "", combined.data$Population)
combined.data$Population <- as.numeric(combined.data$Population)
combined.data <- transform(combined.data, Pop.Per.Square.Kilometer = round(Population/Square_Kilometers, digits = 2))

counties.ordered@data <- bind_cols(counties.ordered@data, combined.data)
counties.ordered@data <- subset(counties.ordered@data, select = -c(STATE_NAME1))
counties.ordered@data
nrow(counties.ordered@data)

quantile(counties.ordered@data$Pop.Per.Square.Kilometer, c(.33, .67, 1.0))
plot(counties.ordered)
plot(counties.ordered[counties.ordered$Pop.Per.Square.Kilometer <= 9, ], col = "yellow", add = TRUE)
plot(counties.ordered[counties.ordered$Pop.Per.Square.Kilometer > 9 & counties.ordered$Pop.Per.Square.Kilometer <= 31 , ], col = "orange", add = TRUE)
plot(counties.ordered[counties.ordered$Pop.Per.Square.Kilometer > 31 & counties.ordered$Pop.Per.Square.Kilometer <= 27596 , ], col = "green", add = TRUE)
counties.UR1 <- counties.ordered[counties.ordered$Pop.Per.Square.Kilometer <= 9, ]
counties.UR2 <- counties.ordered[counties.ordered$Pop.Per.Square.Kilometer > 9 & counties.ordered$Pop.Per.Square.Kilometer <= 31 , ]
counties.UR3 <- counties.ordered[counties.ordered$Pop.Per.Square.Kilometer > 31 & counties.ordered$Pop.Per.Square.Kilometer <= 27596 , ]

final.data

mean(counties.UR1$Unemployment.Rate)
mean(counties.UR2$Unemployment.Rate)
mean(counties.UR3$Unemployment.Rate)
mean(counties.northEast$Unemployment.Rate)
mean(counties.southEast$Unemployment.Rate)
mean(counties.southWest$Unemployment.Rate)
mean(counties.West$Unemployment.Rate)
mean(counties.midWest$Unemployment.Rate)





counties.ordered$Poverty.Pct <- as.numeric(counties.ordered$Poverty.Pct)
counties.ordered$Unemployment.Rate <- as.numeric(counties.ordered$Unemployment.Rate)
counties.ordered$Death.Rate <- as.numeric(counties.ordered$Death.Rate)

boxplot(counties.UR1$Unemployment.Rate, counties.UR2$Unemployment.Rate, counties.UR3$Unemployment.Rate, 
        ylim = c(1,7.5), ylab = "Unemployment Rate", xlab = "Pop. Per Square Km", names = c("Low", "Medium", "High"), 
        col = c("#FFE2B3", "#FFBDF0", "#98FFFD"), notch = TRUE, outline = FALSE)

boxplot(counties.northEast$Unemployment.Rate, counties.southEast$Unemployment.Rate, counties.southWest$Unemployment.Rate, counties.West$Unemployment.Rate, counties.midWest$Unemployment.Rate, ylim = c(1,7.5), horizontal = TRUE)   

randomTest <- sample(1:1026, 100)


counties.AL <- counties.ordered[(counties.ordered$STATE_NAME == "Alabama"),]
counties.FL <- counties.ordered[(counties.ordered$STATE_NAME == "Florida"),]
plot(counties.FL)
mean(counties.FL$Poverty.Pct)
mean(counties.AL$Poverty.Pct)
class(counties.ordered@data$STATE_NAME)
counties.ordered@data


plot(counties.ordered[counties.ordered@data$STATE_NAME %in% stateSample, ])

plot(counties.ordered)
nrow(counties.AL)

stateSample <- sample(c("Alabama", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", 
                        "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", 
                        "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
                        "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", 
                        "Rhose Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", 
                        "West Virginia", "Wisconsin", "Wyoming"), 2)
stateSample

states.blue <- counties.ordered[(counties.ordered$STATE_NAME == "Washington" | counties.ordered$STATE_NAME == "Nevada" | counties.ordered$STATE_NAME == "Wyoming" |
                                    counties.ordered$STATE_NAME == "New Mexico" | counties.ordered$STATE_NAME == "Kansas" | counties.ordered$STATE_NAME == "Iowa" |
                                    counties.ordered$STATE_NAME == "Louisiana" | counties.ordered$STATE_NAME == "Kentucky" | counties.ordered$STATE_NAME == "Maryland" | 
                                    counties.ordered$STATE_NAME == "Georgia" | counties.ordered$STATE_NAME == "Vermont" | counties.ordered$STATE_NAME == "Connecticut" |
                                    counties.ordered$STATE_NAME == "Michigan"), ]
states.yellow <- counties.ordered[(counties.ordered$STATE_NAME == "Oregon" | counties.ordered$STATE_NAME == "Utah" | counties.ordered$STATE_NAME == "South Dakota" |
                                     counties.ordered$STATE_NAME == "Texas" | counties.ordered$STATE_NAME == "Wisconsin" | counties.ordered$STATE_NAME == "Missouri" |
                                     counties.ordered$STATE_NAME == "Mississippi" | counties.ordered$STATE_NAME == "Indiana" | counties.ordered$STATE_NAME == "South Carolina" | 
                                     counties.ordered$STATE_NAME == "Virginia" | counties.ordered$STATE_NAME == "Pennsylvania" | counties.ordered$STATE_NAME == "Massachusetts"), ]
states.orange <- counties.ordered[(counties.ordered$STATE_NAME == "California" | counties.ordered$STATE_NAME == "Montana" | counties.ordered$STATE_NAME == "Colorado" |
                                     counties.ordered$STATE_NAME == "Minnesota" | counties.ordered$STATE_NAME == "Arkansas" | counties.ordered$STATE_NAME == "Ohio" |
                                     counties.ordered$STATE_NAME == "Alabama" | counties.ordered$STATE_NAME == "North Carolina" | counties.ordered$STATE_NAME == "New Jersey" | 
                                     counties.ordered$STATE_NAME == "New Hampshire" | counties.ordered$STATE_NAME == "District of Columbia"), ]
states.green <- counties.ordered[(counties.ordered$STATE_NAME == "Idaho" | counties.ordered$STATE_NAME == "Arizona" | counties.ordered$STATE_NAME == "North Dakota" |
                                     counties.ordered$STATE_NAME == "Nebraska" | counties.ordered$STATE_NAME == "Oklahoma" | counties.ordered$STATE_NAME == "Illinois" |
                                     counties.ordered$STATE_NAME == "Tennessee" | counties.ordered$STATE_NAME == "Florida" | counties.ordered$STATE_NAME == "West Virginia" | 
                                     counties.ordered$STATE_NAME == "Delaware" | counties.ordered$STATE_NAME == "New York" | counties.ordered$STATE_NAME == "Maine"), ]

#Getting lon/lat data into my the map data
geoCounty2 <- (geoCounty %>% mutate_all(~gsub(" County| Parish", "",.)))
geoCounty2 <- (geoCounty2 %>% mutate_all(~gsub("city", "City",.)))
geoCounty2 <- (geoCounty2 %>% mutate_all(~gsub("LaSalle", "La Salle",.)))
geoCounty2 <- (geoCounty2 %>% mutate_all(~gsub("Suffolk City", "Suffolk",.)))
geoCounty2 <- (geoCounty2 %>% mutate_all(~gsub("Hampton City", "Hampton",.)))
geoCounty2 <- (geoCounty2 %>% mutate_all(~gsub("Newport News City", "Newport News",.)))
geoCounty2 <- (geoCounty2 %>% mutate_all(~gsub("Norfolk City", "Norfolk",.)))
geoCounty2 <- (geoCounty2 %>% mutate_all(~gsub("Virginia Beach City", "Virginia Beach",.)))

geoCounty2$FIPS <- as.character(geoCounty2$FIPS)
names(geoCounty2)[1] <- "FIPS"
names(geoCounty2)[2] <- "NAME"


geoCounty2 <- subset(geoCounty2, select = c(NAME, lon, lat, FIPS))
a <- c('Alexandria', 'Bristol', 'Buena Vista', 'Charlottesville', 'Chesapeake', 'Colonial Heights', 'Covington', 'Danville', 'Emporia', 'Fairfax City', 
       'Falls Church', 'Franklin City', 'Fredericksburg', 'Galax', 'Harrisonburg', 'Hopewell', 'Lexington', 'Lynchburg', 'Manassas', 'Manassas Park', 
       'Martinsville', 'Norton', 'Petersburg', 'Poquoson', 'Portsmouth', 'Radford', 'Richmond City', 'Roanoke City', 'Salem', 'Staunton', 'Waynesboro', 
       'Williamsburg', 'Winchester')
b <- c(-77.050552, -82.184898, -79.356375, -78.507980, -76.288376, -77.4102607, -79.9939463, -79.3950228, -77.535975, -77.3063733,
       -77.1710914, -76.9224608, -77.466316, -80.9239671, -78.8689156, -77.28772001, -79.4928171, -79.146042, -77.475143, -77.459450,
       -79.863647, -82.6290459, -77.401924, -76.333694, -76.298271, -79.6178087, -77.434769, -79.941429, -80.054764, -79.071693, -78.8894682,
       -76.7074571, -78.1633341)
c <- c(38.820450, 36.595787, 37.731663, 38.033554, 36.779591, 37.244039, 37.7934585, 36.5859718, 36.696182, 38.8462236, 
       38.882334, 36.6776507, 38.309875, 36.6612387, 38.4495688, 37.3043154, 37.7840202, 37.412762, 38.750660, 38.784225,
       36.683527, 36.933433, 37.227928, 37.120907, 36.835426, 37.1381984, 37.541290, 37.270969, 37.293468, 38.149574, 38.0684693, 
       37.2707022, 39.1856597)

d <- c('51510', '51520', '51530', '51540', '51550', '51570', '51580', '51590', '51595', '51600',
       '51610', '51620', '51630', '51640', '51660', '51670', '51678', '51680', '51683', '51685',
       '51690', '51720', '51730', '51735', '51740', '51750', '51760', '51770', '51775', '51790', '51820', 
       '51830', '51840')

geoCounty3 <- data.frame(a,b,c,d, stringsAsFactors = FALSE)
names(geoCounty3) <- c('NAME', 'lon', 'lat', 'FIPS')
geoCounty2$lon <- as.numeric(geoCounty2$lon)
geoCounty2$lat <- as.numeric(geoCounty2$lat)
geoCounty2$FIPS <- as.numeric(geoCounty2$FIPS)
geoCounty3$FIPS <- as.numeric(geoCounty3$FIPS)

geoCounty4 <- bind_rows(geoCounty2, geoCounty3)
geoCounty4 <- geoCounty4[order(geoCounty4$FIPS),]
geoCounty4 <- subset(geoCounty4, select = -c(NAME, FIPS))


counties.ordered@data <- bind_cols(counties.ordered@data, geoCounty4)
counties.position <- counties.ordered[order(-counties.ordered$lat, counties.ordered$lon),]


loop.seq <- function(start, interval, sampleSize){
  sample <- vector(mode = "integer")
  i <- start
  for (x in (1:sampleSize)){
    sample <- append(sample, i)
    i <- i + interval
    if (i > 3108){
      i <- (i - 3108)
    }
  }
  return(sample)
}

test <- loop.seq(5, 25, 10)
test
test[1]
sample(1:10, 5.5)
