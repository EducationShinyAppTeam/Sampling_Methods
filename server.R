library(shiny)
library(shinyjs)
library(shinyBS)
library(maps)
library(ggplot2)
library(mapproj)
library(dplyr)
library(rgdal)
library(stats)
library(raster)
library(housingData)

#Reading in the collected data to use for calculations
countydata <- read.csv("CountySamplingData.csv", TRUE, sep = ",",na.strings = TRUE)
dataf <- data.frame(countydata) 
dataf.sub <- dataf[dataf$State != "AK" & dataf$State != "HI", ]
dataf.sub$Median.Household.Income <- gsub(",","",dataf.sub$Median.Household.Income )
dataf.sub$Median.Household.Income <- as.numeric(dataf.sub$Median.Household.Income)
dataf.sub$FIPS <- as.numeric(dataf.sub$FIPS)

#Read in the shapefile
counties <- readOGR(dsn = "UScounties")
counties$FIPS <- as.numeric(as.character(counties$FIPS))
counties.sub <- counties[(counties$STATE_NAME != "Alaska" & counties$STATE_NAME != "Hawaii"), ]
counties.sub$NAME <- as.character(counties.sub$NAME)
counties.ordered <- counties.sub[order(counties.sub$FIPS), ]
counties.ordered <- counties.ordered[counties.ordered$NAME != "Bedford City", ]

#Combining and cleaning the shapefile data with the imported county data for calculations
final.data <- inner_join(counties.ordered@data, dataf.sub, by = "FIPS")
final.data <- subset(final.data, select = -c(STATE_FIPS, CNTY_FIPS, State, Area.Name))
missingCounty <- data.frame("Shannon", "South Dakota", "46113", "7,341", "51.9", "27,804", "2161", "2251", "3421", "1080", "24", "25", "38", "12", "14,291", "11.7", "12.08", "10.3")
names(missingCounty) <- names(final.data)
final.data <- rbind(final.data, missingCounty)
final.data <- final.data[order(final.data$STATE_NAME),]
final.data$Square_Kilometers <- round(area(counties.ordered)/1000000, digits = 2)
final.data$Population <- gsub(",", "", final.data$Population)
final.data$Population <- as.numeric(final.data$Population)
final.data$Median.Household.Income <- gsub(",", "", final.data$Median.Household.Income)
final.data$Median.Household.Income <- as.numeric(final.data$Median.Household.Income)
final.data <- transform(final.data, Pop.Per.Square.Kilometer = round(Population/Square_Kilometers, digits = 2))
counties.ordered@data <- bind_cols(counties.ordered@data, final.data)
(counties.ordered@data)
counties.ordered$Unemployment.Rate <- as.numeric(counties.ordered$Unemployment.Rate)


#Grouping counties by region and Urban/Rural for stratification
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

counties.UR1 <- counties.ordered[counties.ordered$Pop.Per.Square.Kilometer <= 9.3124, ]
counties.UR2 <- counties.ordered[counties.ordered$Pop.Per.Square.Kilometer > 9.3124 & counties.ordered$Pop.Per.Square.Kilometer <= 31.1977, ]
counties.UR3 <- counties.ordered[counties.ordered$Pop.Per.Square.Kilometer > 31.1977 & counties.ordered$Pop.Per.Square.Kilometer <= 27596, ]

#Grouping counties by state in different colors to make clusters clear

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
names(geoCounty2)[1] <- "FIPS"
names(geoCounty2)[2] <- "NAME"
geoCounty2$FIPS <- as.character(geoCounty2$FIPS)
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

#Formatting counties.position to use for renderTable
counties.position2 <- subset(counties.position, select = c(NAME, STATE_NAME))
names(counties.position2)[1] <- "County"
names(counties.position2)[2] <- "State"
counties.position2 <- mutate(counties.position2@data, ID = row_number())

#Function for systematic sample
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

shinyServer(function(input, output, session) {
  
  #Note for table in systematic tab
  addPopover(session = session, id = "systemTable", title = "Note:",
    content = "Notice how the relationship of ID's between the counties displayed corresponds to the interval calculated above"
    , trigger = "hover", placement = "right")
  
  addPopover(session = session, id = "myBoxplots", title = "Note:",
             content = "Notice when the strata variable is geographic region, there appears to be a larger difference between strata regarding
             unemployment rate than when the strata variable is population per square kilometer. The strata variable makes a difference in this case!"
             , trigger = "hover", placement = "left")
  
  #Prerequisites link on Overview Tab
  observeEvent(input$prerequisites,{
    updateTabItems(session,"tabs","prereq")
  })
  
  #Go Button on Overview and Prereq Tabs
  observeEvent(input$go,{
    updateTabItems(session,"tabs","explore")
  })
  
  observeEvent(input$go2,{
    updateTabItems(session,"tabs","explore")
  })
  
  #Info button on header
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "Click on the various sampling methods at the top of the page on the 'Explore' tab for a more in-depth look at these sampling methods",
      type = NULL
    )
  })
  
  #Adding previous/next buttons for tabs
  # observeEvent(input$next1,{
  #   updateNavbarPage(session = session,"navMain", selected = "b")
  # })
  # 
  # observeEvent(input$next2,{
  #   updateNavbarPage(session = session,"navMain", selected = "c")
  # })
  # 
  # observeEvent(input$prev1,{
  #   updateNavbarPage(session = session,"navMain", selected = "a")
  # })
  
  #Make the random sample a reactive variable that is run through the generate button
  random <- reactive({
    input$generate
    samplesize <- input$srsSize
    sample(1:3108, samplesize)
  })
  
  #SimpleRandomSample Tab
  
  #Map
  output$myMap <- renderPlot({
    if (input$generate == 0)
     return( plot(counties.ordered) + title("US Counties"))
    plot(counties.ordered) + title("Counties in First Sample")
    randomdata <- isolate(random())
    plot(counties.ordered) + title("Counties in First Sample") 
    plot(counties.ordered[randomdata, ], col = "red", add = TRUE)
  })
  
  #Output of random numbers used in sample
  output$randNumbs <- renderText({
    if (input$generate == 0)
      return('')
    randomdata <- isolate(random())
    randomdata[1:50] 
  })
  
  #Output of countries used in the sample
  output$first50Counties <- renderPrint({
    if (input$generate == 0)
      return(cat(''))
    randomdata <-isolate(random())
    randomdataSub <- randomdata[1:50]
    randomdataSub
    cat(paste(final.data[randomdataSub,]$NAME, collapse = ",  ")) 
  })
  
  #Histogram of sample averages
  output$samplesHistogram <- renderPlot({
    if (input$generate ==0)
      return(cat(''))
    sampleCount <- isolate(input$sampleCount)
    sampleSize <- isolate(input$srsSize)
    sampleMeans <- replicate(sampleCount, mean(sample(final.data$Median.Household.Income, sampleSize)))
    hist(sampleMeans, main = '', xlab = "Median Household Income", col = "yellow")
    title("Histogram of Median Household Income Across Samples", line = 3.4)
    abline(v = 50957.72, col = "red", lwd = 4)
    abline(v = mean(sampleMeans), col = "blue", lwd = 4, lty = "dotted")
    legend("topleft", legend = c("True Population Median Household Income ($50,957.72)", "Average Median Household Income Across Samples"), 
           col = c("red", "blue"), bty = "n", pch = 18, pt.cex = 2, cex = 1.2, inset = c(-.05, -.18), xpd = TRUE)
  })
  
  ##Stratifcation Tab##

  #Making reactive random samples for each of the stratafied regions
  s <- reactiveValues()
  observeEvent(input$generate3, {
    sampleSize <- input$srsSize2
    sampleWeight <- round((sampleSize/3108) * 245)
    s$NE <- sample(1:245, sampleWeight)
  })
  observeEvent(input$generate2, {
    s$NE <- 1:245
  })
  observeEvent(input$generate3, {
    sampleSize <- input$srsSize2
    sampleWeight <- round((sampleSize/3108) * 366)
    s$W <- sample(1:366, sampleWeight)
  })
  observeEvent(input$generate2, {
    s$W <- 1:366
  })
  observeEvent(input$generate3, {
    sampleSize <- input$srsSize2
    sampleWeight <- round((sampleSize/3108) * 1055)
    s$MW <- sample(1:1055, sampleWeight)
  })
  observeEvent(input$generate2, {
    s$MW <- 1:1055
  })
  observeEvent(input$generate3, {
    sampleSize <- input$srsSize2
    sampleWeight <- round((sampleSize/3108) * 379)
    s$SW <- sample(1:379, sampleWeight)
  })
  observeEvent(input$generate2, {
    s$SW <- 1:379
  })
  observeEvent(input$generate3, {
    sampleSize <- input$srsSize2
    sampleWeight <- round((sampleSize/3108) * 1063)
    s$SE <- sample(1:1063, sampleWeight)
  })
  observeEvent(input$generate2, {
    s$SE <- 1:1063
  })
  observeEvent(input$generate3, {
    sampleSize <- input$srsSize2
    sampleWeight <- round((sampleSize/3108) * 1026)
    s$Low <- sample(1:1026, sampleWeight)
  })
  observeEvent(input$generate2, {
    s$Low <- 1:1026
  })
  observeEvent(input$generate3, {
    sampleSize <- input$srsSize2
    sampleWeight <- round((sampleSize/3108) * 1056)
    s$Med <- sample(1:1056, sampleWeight)
  })
  observeEvent(input$generate2, {
    s$Med <- 1:1056
  })
  observeEvent(input$generate3, {
    sampleSize <- input$srsSize2
    sampleWeight <- round((sampleSize/3108) * 1026)
    s$High <- sample(1:1026, sampleWeight)
  })
  observeEvent(input$generate2, {
    s$High <- 1:1026
  })
  
  #Colors for strata samples
  c <- reactiveValues()
  observeEvent(input$generate3, {
    c$NE <- "blue"
  })
  observeEvent(input$generate2, {
    c$NE <- "#B0DAFF"
  })
  observeEvent(input$generate3, {
    c$W <- "yellow"
  })
  observeEvent(input$generate2, {
    c$W <- "#FEFFB0"
  })
  observeEvent(input$generate3, {
    c$MW <- "orange"
  })
  observeEvent(input$generate2, {
    c$MW <- "#FFC795"
  })
  observeEvent(input$generate3, {
    c$SW <- "red"
  })
  observeEvent(input$generate2, {
    c$SW <- "#FF9696"
  })
  observeEvent(input$generate3, {
    c$SE <- "darkgreen"
  })
  observeEvent(input$generate2, {
    c$SE <- "darkolivegreen1"
  })
  
  observeEvent(input$generate3, {
    c$Low <- "red"
  })
  observeEvent(input$generate2, {
    c$Low <- "#FFE2B3"
  })
  observeEvent(input$generate3, {
    c$Med <- "red"
  })
  observeEvent(input$generate2, {
    c$Med <- "#FFBDF0"
  })
  observeEvent(input$generate3, {
    c$High <- "red"
  })
  observeEvent(input$generate2, {
    c$High <- "#98FFFD"
  })
  
  output$myMap2 <- renderCachedPlot(
    {
    if (input$generate2 == 0)
      return(plot(counties.ordered) + title("US Counties"))
    if (isolate(input$strataVar) == "Geographic Region"){
      plot(counties.ordered) + title("Stratified US Counties: Geographic Region")
      plot(counties.northEast, col = "#B0DAFF", add = TRUE)
      plot(counties.northEast[s$NE, ], col = c$NE, add = TRUE)
      plot(counties.midWest, col = "#FFC795", add = TRUE)
      plot(counties.midWest[s$MW, ], col = c$MW, add = TRUE)
      plot(counties.West, col = "#FEFFB0", add = TRUE)
      plot(counties.West[s$W, ], col = c$W, add = TRUE)
      plot(counties.southEast, col = "darkolivegreen1", add = TRUE)
      plot(counties.southEast[s$SE, ], col = c$SE, add = TRUE)
      plot(counties.southWest, col = "#FF9696", add = TRUE)
      plot(counties.southWest[s$SW, ], col = c$SW, add = TRUE)
      legend("bottomleft", legend = c("West (366)", "Midwest (1055)", "Southwest (379)", "Northeast (245)", "Southeast (1063"), col = c("#FEFFB0", "#FFC795", "#FF9696", "#B0DAFF", "darkolivegreen1"), bty = "n", pch = 18, pt.cex = 2, cex = 1.2)
    }
    if (isolate(input$strataVar) == "Pop. Per Square Kilometer"){
      plot(counties.ordered) + title("Stratified US Counties: Population Per Square Kilometer")
      plot(counties.UR1, col = "#FFE2B3", add = TRUE)
      plot(counties.UR1[s$Low, ], col = c$Low, add = TRUE)
      plot(counties.UR2, col = "#FFBDF0", add = TRUE)
      plot(counties.UR2[s$Med, ], col = c$Med, add = TRUE)
      plot(counties.UR3, col = "#98FFFD", add = TRUE)
      plot(counties.UR3[s$High, ], col = c$High, add = TRUE)
      legend("bottomleft", legend = c("Low (1026)", "Medium (1056)", "High (1026)"), col = c("#FFE2B3", "#FFBDF0", "#98FFFD"), bty = "n", pch = 18, pt.cex = 2, cex = 1.2)
    }
    },
    cacheKeyExpr = {list(input$generate2, (input$strataVar), input$generate3, (s$NE), (c$NE), (s$MW), (c$MW), (s$W), (c$W), (s$SW), (c$SW), (s$SE), (c$SE), (s$Low), (c$Low), (s$Med), (c$Med), (s$High), (c$High))}
    )
  
  #Boxplots across samples
  observeEvent(input$generate2, {
    hide("myBoxplots")
  })
  observeEvent(input$generate3, {
    showElement("myBoxplots")
  })
  output$myBoxplots <- renderPlot({
    if (input$generate3 == 0)
      return(cat(''))
    if (input$generate2 == 0)
      return(cat(''))
    if ((input$strataVar) == "Geographic Region"){
      boxplot(counties.northEast[s$NE, ]$Unemployment.Rate, counties.midWest[s$MW, ]$Unemployment.Rate, counties.West[s$W, ]$Unemployment.Rate, 
              counties.southEast[s$SE, ]$Unemployment.Rate, counties.southWest[s$SW, ]$Unemployment.Rate, main = "Unemployment Rate Per Strata",
              ylab = "Unemployment Rate", xlab = "Geographic Region", names = c("Northeast", "Midwest", "West", "Southeast", "Southwest"),
              col = c("blue", "orange", "yellow", "darkgreen", "red"), outline = FALSE)
    }
    if ((input$strataVar) == "Pop. Per Square Kilometer"){
      boxplot(counties.UR1[s$Low, ]$Unemployment.Rate, counties.UR2[s$Med, ]$Unemployment.Rate, counties.UR3[s$High, ]$Unemployment.Rate, 
                ylim = c(1,7.5), main = "Unemployment Rate Per Strata (Sample)", ylab = "Unemployment Rate", xlab = "Pop. Per Square Km", 
                names = c("Low", "Medium", "High"), col = c("#FFE2B3", "#FFBDF0", "#98FFFD"), outline = FALSE)
    }
  })
  
  #Show how sample size within each strata is calculated
  output$strataCalc <- renderUI({
    if (input$generate3 == 0)
      return(cat(''))
    sampleSize <- isolate(stratSampSize())
    titleLine <- (paste("Sample Size Within Each Strata:"))
    formulaLine <- paste("Formula: (Sample Size / Population Size) * Strata Size")
    geoTotal <- paste("Total: ",round((sampleSize/3108)*366), "+", round((sampleSize/3108)*1055), "+", round((sampleSize/3108)*379), "+",
                      round((sampleSize/3108)*245), "+", round((sampleSize/3108)*1063)," ≈ ", strong(sampleSize))
    popTotal <- paste("Total: ",round((sampleSize/3108)*1026), "+", round((sampleSize/3108)*1056), "+", round((sampleSize/3108)*1026),
                      " ≈ ", strong(sampleSize))
    geoLine1 <- (paste("West: ", "(", sampleSize, "/", 3108, ")", "*", 366, " = ", round((sampleSize/3108)*366)))
    geoLine2 <- (paste("Midwest: ", "(", sampleSize, "/", 3108, ")", "*", 1055, " = ", round((sampleSize/3108)*1055)))
    geoLine3 <- (paste("Southwest: ", "(", sampleSize, "/", 3108, ")", "*", 379, " = ", round((sampleSize/3108)*379)))
    geoLine4 <- (paste("Northeast: ", "(", sampleSize, "/", 3108, ")", "*", 245, " = ", round((sampleSize/3108)*245)))
    geoLine5 <- (paste("Southeast: ", "(", sampleSize, "/", 3108, ")", "*", 1063, " = ", round((sampleSize/3108)*1063)))
    popLine1 <- (paste("Low: ", "(", sampleSize, "/", 3108, ")", "*", 1026, " = ", round((sampleSize/3108)*1026)))
    popLine2 <- (paste("Medium: ", "(", sampleSize, "/", 3108, ")", "*", 1056, " = ", round((sampleSize/3108)*1056)))
    popLine3 <- (paste("High: ", "(", sampleSize, "/", 3108, ")", "*", 1026, " = ", round((sampleSize/3108)*1026)))
    if ((input$strataVar) == "Pop. Per Square Kilometer"){
      HTML(paste(strong(titleLine), em(formulaLine), popLine1, popLine2, popLine3, popTotal, sep = "<br/>"))
    } else {
      HTML(paste(strong(titleLine), em(formulaLine), geoLine1, geoLine2, geoLine3, geoLine4, geoLine5, geoTotal, sep = "<br/>"))
    }
    
  })
  
  #Making the sample size a reactive variable to use with the strata size calculations
  stratSampSize <- reactive({
    input$generate3
    samplesize <- input$srsSize2
    samplesize
  })
  
  
  ##Clustered Tab##
  
  stateSample <- reactive({
    input$generate5
    samplesize <- isolate(input$srsSize3)
    sample(c("Alabama", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", 
             "Georgia", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", 
             "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
             "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", 
             "Rhose Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", 
             "West Virginia", "Wisconsin", "Wyoming"), samplesize)
  })
  
  c2 <- reactiveValues()
  observeEvent(input$generate4,{
    c2$Cluster <- NULL
  })
  observeEvent(input$generate5,{
    c2$Cluster <- "red"
  })
  
  
  #Cluster Map
  output$myMap3 <- renderPlot({
    if (input$generate4 == 0)
      return(plot(counties.ordered) + title("US Counties"))
    stateSample <- stateSample()
    plot(counties.ordered) + title("Clustered US Counties by State")
    plot(states.blue, col = "blue", add = TRUE)
    plot(states.yellow, col = "yellow", add = TRUE)
    plot(states.orange, col = "orange", add = TRUE)
    plot(states.green, col = "green", add = TRUE)
    plot(counties.ordered[counties.ordered@data$STATE_NAME %in% stateSample, ], col = c2$Cluster, add = TRUE)
  })
  
  output$clusterLegend <- renderText({
    if (input$generate4 == 0)
      return(cat(''))
    paste("Note: The blue, yellow, orange, and green colors shown above are solely used to define state boundaries.
          (These states are not related in any way)")
  })
  
  output$sampleStates <- renderPrint({
    if (input$generate5 == 0)
      return(cat(''))
    stateSample <- isolate(stateSample())
    cat(paste(stateSample, collapse = ",  ")) 
  })
  
  ##Systematic Tab##
  
  #output of starting county
  
  randomNumber <- reactive({
    (input$startPoint)
    sample(1:3108, 1)
  })
  
  output$startingCounty <- renderPrint({
    if (input$startPoint == 0)
      return(cat(''))
    start <- randomNumber()
    cat(paste(counties.position[start,]$NAME,",",counties.position[start,]$STATE_NAME, "(", randomNumber(), ")", collapse = ", "))
  })
  
  #Systematic Sample
  
  randomSystem <- reactive({
    input$generate6
    start <- isolate(randomNumber())
    size <- isolate(input$srsSize4)
    interval <- floor(3108/size)
    loop.seq(start, interval, size)
  })
  
  #Interval Size
  output$intervalSize <- renderPrint({
    if (input$generate6 == 0)
      return(cat(''))
    size <- isolate(input$srsSize4)
    interval <- floor(3108/size)
    cat(paste(3108,'/',size,'=',interval))
  })
  
  #Map
  output$myMap4 <- renderPlot({
    if (input$generate6 == 0)
      return(plot(counties.position) + title("US Counties"))
    randomSystem <- isolate(randomSystem())
    plot(counties.position) + title("US Counties")
    plot(counties.position[randomSystem,], col = "red", add = TRUE)
    plot(counties.position[randomSystem[1],], col = "lawngreen", add = TRUE)
  })
  
  output$mapLegend <- renderText({
    if (input$generate6 == 0)
      return(cat(''))
    paste("Note: the starting county is highlighted in green (If you can't find it, try generating a new starting point)")
  })
  
  #This keeps the table from displaying until the sample is displayed 
  observeEvent(input$generate6, {
    showElement("systemTable")
  })
  
  output$systemTable <- renderTable(
    head(counties.position2[randomSystem(),]), spacing = "xs", width = '250px'
  )
  
  
  
})


