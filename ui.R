packages <- c("shiny","shinyjs","shinyBS","shinyWidgets","shinycssloaders",
            "shinydashboard","ggplot2","magrittr")
lapply(packages,library,character.only=TRUE)

#library(shiny)
#library(shinyjs)
#library(ggplot2)
#library(shinydashboard)
#library(shinyBS)
#library(shinyWidgets)
#library(shinycssloaders)
#library(magrittr)

ui <- dashboardPage(
  
  dashboardHeader(title = "Sampling Methods",
                  tags$li(class = "dropdown",
                          tags$a(href = "https://shinyapps.science.psu.edu/",
                                 icon("home", lib = "font-awesome"))),
                  tags$li(class = "dropdown",
                          actionLink("info", icon("info"), class = "myClass")),
                  titleWidth = 300),
  dashboardSidebar(width = 180, 
                   sidebarMenu(id = 'tabs',
                               menuItem("Prerequisites", tabName = "prereq", icon = icon("book")),
                               menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
                               menuItem("Explore", tabName = "explore", icon = icon("wpexplorer"))
                   )
                   
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", 
                type = "text/css", 
                href = "Feature.css")
    ),
    
    tabItems(
      
      tabItem(tabName = "overview",
              
              fluidPage(
                tags$a(href='http://stat.psu.edu/',
                       tags$img(src='logo.png', 
                                align = "left",
                                width = 180)),
                br(),br(),br(),
                h3(strong("About:")),
                h4("Explore four of the most commonly used sampling methods: 
                   Simple Random Sampling, Stratified Sampling, Cluster Sampling, 
                   and Systematic Sampling.If you would like a basic refresher on
                   Sampling, please refer to the", 
                   
                   actionLink("prerequisites",
                              "Prerequisites"), 
                   
                   "tab.  If not, read the instructions and press the 'Go' button
                   to proceed."),
                br(),
                h3(strong("Instructions: ")),
                h4(tags$li("On the Explore page, you will be simulating random
                           samples of counties in the United States.")),
                h4(tags$li("On each tab, there will be a different sampling method
                           for you to delve into so you can see the processes 
                           involved within each method")),
                
                div(style = "text-align: center",
                    bsButton("go",
                             "G O !",
                             icon("bolt"),
                             style = "danger",
                             size = "large",
                             class = "circle grow")),
                br(),
                h3(strong("Acknowledgements: ")),
                h4("This app was developed and coded by Sean Klavans. The county 
                   data was extracted from", 
                   tags$a(href = "https://www.ers.usda.gov/data-products/county-level-data-sets/download-data/",
                          "USDA: County Level Data Sets",
                          style = "text-decoration: underline; color: #cd3333"),
                   "on May 30, 2019.")
                )
              ),
      
      tabItem(tabName = "prereq",
              h3(strong("Background: Various Sampling Methods")),
              br(),
              tags$ul(
                h4(tags$li("Sampling is a process used in statistical analysis 
                           in which a set of data is selected from a larger 
                           population by using a predefined selection method in
                           order to capture information about the population as 
                           a whole.")),
                
                h4(tags$li("Usually, the population is too large to gather data 
                           about each member of the population, because this can
                           potentially beboth very time consuming and expensive.
                           This is the main reason to conduct a sample, because 
                           in theory the sample is an accurate representation of
                           the total population.")),
                
                h4(tags$li("Random sampling is important, because an unbiased 
                           representation of the total population is desired in
                           order to provide the mostaccurate representation of 
                           the population as a whole. In an unbiased sample, the
                           mean of the sampling distribution is equal to the 
                           population parameter.")),
                
                h4(tags$li("However, there are different methods for carrying out
                           a random sample. Which method to use is often determined 
                           by research goals, cost,and effectivenes in the given
                           situation. The four main types of random sampling are
                           Simple Random Sampling, Stratified Random Sampling,
                           Cluster Random Sampling, and Systematic Random Sampling"))
              ),
              div(style = "text-align: center",
                  bsButton("go2",
                           "G O !",
                           icon("bolt"),
                           style = "danger",
                           size = "large",
                           class = "circle grow"))
              ),
      
      tabItem(tabName = "explore",
              
              fluidRow(
                navbarPage(title = strong("Sampling Methods: "),
                           id = "navMain",
                           #SimpleRandomSimulation
                           tabPanel("Simple Random",
                                    value = "a",
                                    fluidPage(fluidRow(wellPanel(h4("Suppose you 
                                              want to use a sample to determine 
                                              the average median household income
                                              for counties in the US.Here you are
                                              going to be taking a", strong("simple random sample"),".
                                              Using this sampling method, each county is 
                                              assigned a number (1 - 3108) and you are 
                                              going to randomly generate numbers 
                                              based on your selected sample size. 
                                              Each number that is randomly generated 
                                              corresponds to a county that will be 
                                              included in the sample. Additionally, 
                                              you are going to be replicating your
                                              sample a certain number of times and 
                                              seeing how well your distribution of
                                              samples estimates true median population 
                                              household income. (Play around with the 
                                              sample size and the number of samples you want
                                              to be taken and take notice of its 
                                              effect on accuracy)")
                                        )
                                        ),
                                        
                                      sidebarLayout(
                                        sidebarPanel(
                                          tags$style(HTML(".js-irs-0 .irs-single, 
                                                          .js-irs-0 .irs-bar-edge,
                                                          .js-irs-0 .irs-bar 
                                                          {background: red}")),
                                          
                                          tags$style(HTML(".js-irs-1 .irs-single,
                                                          .js-irs-1 .irs-bar-edge,
                                                          .js-irs-1 .irs-bar 
                                                          {background: red}")),
                                          
                                          sliderInput("srsSize", 
                                                      "Select how large you would
                                                      like your sample sizes to be
                                                      (for each individual sample):",
                                                      min = 50,
                                                      max = 2000, 
                                                      value = 1000,
                                                      step = 25),
                                          
                                          sliderInput("sampleCount", "
                                                      Select how many samples you
                                                      would like to take:",
                                                      min = 1,
                                                      max = 1000, 
                                                      value = 500, 
                                                      step = 25),
                                          
                                          bsButton("generate", 
                                                   "Generate Random Samples", 
                                                   icon("sync-alt"), 
                                                   type = "action", 
                                                   style = "success"),
                                          
                                          br(), br(),
                                          
                                          p(strong(" Numbers Generated for First 
                                                   Sample (First 50):")),
                                          
                                          textOutput("randNumbs"),
                                          br(),
                                          
                                          p(strong("Corresponding Counties in 
                                                   First Sample (First 50):")),
                                          
                                          textOutput("first50Counties")
                                        ),
                                        
                                        mainPanel(
                                          plotOutput("myMap") %>%
                                            withSpinner(color = "red"),
                                          
                                          plotOutput("samplesHistogram")
                                        )
                                      )
                                      # conditionalPanel("input.generate !=0", 
                                      #                 fluidRow(
                                      #                   column(1, offset = 5, 
                                      #                   bsButton("next1", "Next>>",
                                      #                   style = "danger", 
                                      #                   size = "medium"))
                                      #                 )
                                      # )
                                        )
                                      ),
                           
                           #StratifiedSimulation
                           tabPanel("Stratified", value = "b",
                                    fluidPage(fluidRow(
                                      wellPanel(h4("In", 
                                                    strong("stratified random sampling"), 
                                                    ", the population is first divided 
                                                    into non-overlapping groups, 
                                                    called strata. These are normally 
                                                    larger groups where the elements
                                                    in each strata share a similar 
                                                    characteristic or are determined 
                                                    by a variable. (On this page, you
                                                    are going to be stratifying counties 
                                                    based on geographic region and 
                                                    population per square kilometer).
                                                    Next, a random sample of elements are 
                                                    taken within each strata, and then 
                                                    these samples are put together to 
                                                    achieve your stratified random sample. 
                                                    (The amount of elements taken from
                                                    each strata is calculated based on 
                                                    the size of the strata relative to the 
                                                    population size, which you will see 
                                                    on this page). After going through 
                                                    these steps, you will be visualizing
                                                    Unemployment Rate data throughout 
                                                    the counties and between the strata.")
                                        )
                                        ),
                                      
                                      sidebarLayout(
                                        sidebarPanel(
                                          selectInput("strataVar", 
                                                      "Strata Variable", 
                                                      choices = c("Geographic Region",
                                                                  "Pop. Per Square Kilometer")),
                                          
                                          bsButton("generate2",
                                                   "Generate Strata",
                                                   icon("sync-alt"),
                                                   type = "action", 
                                                   style = "primary"),
                                          
                                          br(), br(),
                                          
                                          tags$style(HTML(".js-irs-2 .irs-single,
                                                          .js-irs-2 .irs-bar-edge,
                                                          .js-irs-2 .irs-bar 
                                                          {background: red}")),
                                          
                                          sliderInput("srsSize2",
                                                      "Select how large you would 
                                                      like your sample size to be:"
                                                      , min = 50, 
                                                      max = 2000, 
                                                      value = 1000, 
                                                      step = 25),
                                          
                                          bsButton("generate3", 
                                                   "Generate Random Sample", 
                                                   icon("sync-alt"), 
                                                   type = "action",
                                                   style = "success"),
                                          
                                          br(), br(),
                                          
                                          htmlOutput("strataCalc")
                                        ),
                                        
                                        mainPanel(
                                          useShinyjs(),
                                          plotOutput("myMap2")
                                          %>% withSpinner(color = "red"),
                                          hidden(plotOutput("myBoxplots"))
                                          
                                        )
                                      )
                                      # conditionalPanel("input.generate3 !=0", 
                                      #                  fluidRow(
                                      #                    column(1, bsButton("prev1",
                                      #                   "<<Previous", style = "danger",
                                      #                     size = "medium")),
                                      #                    column(1, offset = 10,
                                      #                    bsButton("next2", "Next>>", 
                                      #                    style = "danger", 
                                      #                    size = "medium"))
                                      #                  )
                                      # )
                                        )
                                      ),
                           
                           #ClusterSimulation
                           tabPanel("Cluster", value = "c",
                                    fluidPage(
                                      fluidRow(
                                        wellPanel(h4("In", strong("cluster sampling"),
                                                     ", the population is first divided 
                                                     into non-overlapping groups, called 
                                                     clusters. Usually, the  elements
                                                     in each cluster share a similar
                                                     characteristic (On this page, you are 
                                                     going to be clustering 
                                                     counties based on their state).
                                                     Next, a random sample of clusters 
                                                     is selected and every element of the 
                                                     population inside the sampled clusters
                                                     is included in the final sample. 
                                                     (This is different from a stratified
                                                     sample because only the elements 
                                                     from the randomly selected clusters 
                                                     are included in the sample, whereas in a 
                                                     stratafied sample elements from all 
                                                     of the strata are included in the sample)")
                                        )
                                        ),
                                      
                                      sidebarLayout(
                                        sidebarPanel(
                                          p(strong("First, generate county 
                                                   clusters based on states: ")),
                                          
                                          bsButton("generate4", 
                                                   "Generate Clusters",
                                                   icon("sync-alt"), 
                                                   type = "action", 
                                                   style = "primary"),
                                          
                                          br(), br(),
                                          
                                          tags$style(HTML(".js-irs-3 .irs-single,
                                                          .js-irs-3 .irs-bar-edge, 
                                                          .js-irs-3 .irs-bar {back
                                                          ground: red}")),
                                          
                                          sliderInput("srsSize3",
                                                      "Select how large you would 
                                                      like your sample size of 
                                                      clusters to be:",
                                                      min = 5, 
                                                      max = 25, 
                                                      value = 15, 
                                                      step = 1),
                                          
                                          p(strong("Now, generate your 
                                                   sample of clusters: ")),
                                          
                                          bsButton("generate5",
                                                   "Generate Random Sample",
                                                   icon("sync-alt"),
                                                   type = "action",
                                                   style = "success"),
                                          
                                          br(), br(),
                                          
                                          p(strong("States (and respective counties)
                                                   included in Sample:")),
                                          
                                          textOutput("sampleStates")
                                          
                                        ),
                                        mainPanel(
                                          useShinyjs(),
                                          plotOutput("myMap3") %>%
                                            withSpinner(color = "red"),
                                          
                                          textOutput("clusterLegend")
                                        )
                                      )
                                        )
                                    ),
                           
                           #SystematicSimulation
                           tabPanel("Systematic", value = "d",
                                    fluidPage(
                                      fluidRow(
                                        wellPanel(h4("In a", strong("systematic random sample"), 
                                                     ", elements from a population are selected 
                                                     based on a random starting point but with a fixed, 
                                                     periodical interval (k) that is calculated from 
                                                     the sample size. The starting element is selected,
                                                     and then each kth element after that is included 
                                                     until the desired sample size is obtained. If this 
                                                     process takes you past the end of your population, 
                                                     it then loops back around to the begininng 
                                                     and continues. On this page, you are going to 
                                                     be going through this process and trying it out
                                                     for yourself. Furthermore, 
                                                     the 3108 counties on this map are ordered based
                                                     on their location instead of alphabetically 
                                                     so the map should reflect the impact of the 
                                                     different parameters you define for your 
                                                     sample. (Ordering goes from Northwestern to 
                                                     Southeastern, so Washington counties would be first, 
                                                     Florida counties would be last, etc.) ")
                                        )
                                        ),
                                      sidebarLayout(
                                        sidebarPanel(
                                          p(strong("Based on the ordering system described
                                                   above, please randomly generate a number
                                                   between 1 and 3108 to get a starting
                                                   point for your systematic sample:")),
                                          
                                          bsButton("startPoint", 
                                                   "Select Starting County",
                                                   icon("random"),
                                                   type = "action", 
                                                   style = "primary"),
                                          
                                          br(), br(),
                                          
                                          p(strong("Starting County: ")),
                                          textOutput("startingCounty"),
                                          br(), 
                                          
                                          tags$style(HTML(".js-irs-4 .irs-single, 
                                                          .js-irs-4 .irs-bar-edge,
                                                          .js-irs-4 .irs-bar 
                                                          {background: red}")),
                                          
                                          sliderInput("srsSize4", 
                                                      "Please select how large you
                                                      would like your sample size
                                                      to be:", 
                                                      min = 50,
                                                      max = 200,
                                                      value = 125,
                                                      step = 1),
                                          
                                          bsButton("generate6", 
                                                   "Generate Random Sample", 
                                                   icon("sync-alt"),
                                                   type = "action", 
                                                   style = "success"),
                                          
                                          br(), br(),
                                          
                                          p(strong("Interval Size (Total number 
                                                   of elements in population
                                                   divided by sample size rounded 
                                                   down to the nearest integer): ")),
                                          
                                          textOutput("intervalSize"),
                                          br(),
                                          
                                          p(strong("First five counties 
                                                   included in sample:")),
                                          
                                          hidden(tableOutput("systemTable"))
                                          ),
                                        
                                        mainPanel(
                                          useShinyjs(),
                                          plotOutput("myMap4") %>% 
                                            withSpinner(color = "red"),
                                          
                                          textOutput("mapLegend"),
                                          hidden(plotOutput("systemSummary"))
                                        )
                                      )
                                        )
                                      )
                           )
                
                
              )
              
      )
      
      
      
              )
    
    )
  
  
  )
