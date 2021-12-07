library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(ggrepel)
library(ggplot2)
library(bslib)
library(RColorBrewer)
library(viridis)
library(knitr)
library(shinyWidgets)
library(shinyjs)
library(leaflet)
library(leaflegend)

######################
#    Import Data     #
######################

# Import wrangled STARS data
map <- read_csv("data/map.csv") 

## Input Names

#Variables for TAB 1 Map variables selectInput
score_values <- c("rating", "total", "endowment", "area", "size",  "climate", "locale", "renewables", "gge", "water", "waste", "recycling", "classes", "real_food", "plant_based", "monday") 
score_names <- c("Rating", "STARS Report Card", "Endowment", "Area", "Population", "Climate", "Locale", "Renewables", "Emmissions", "Water", "Waste", "Recycling", "Classes", "Food Sourcing", "Plant-Based", "Meatless Monday")
names(score_values) <- toupper(score_names)

#Variables for TAB 1 Report Card selectInput
report_values <- c("total", "curriculum", "researching", "campus_engagement", "public_engagement", "air", "buildings", "energy", "food", "planning", "diversity", "investment", "wellbeing")
report_names <- c("Total", "Curriculum", "Research", "Campus Engagement", "Public Engagement", "Air", "Buildings", "Energy", "Food", "Planning", "Diversity", "Investment", "Wellbeing")
names(report_values) <- toupper(report_names)

#Variables for TAB 2 Plot selectInput
bar_values <- c("total", "renewables", "gge", "water", "waste", "recycling", "classes", "real_food", "endowment", "area", "size")
bar_names <- c("STARS Report Card", "Renewables", "Emmissions", "Water", "Waste", "Recycling", "Classes", "Food Purchasing", "Endowment", "Area", "Population")
names(bar_values) <- bar_names

#Variables for TAB 1 Threshold Buttons
threshold_values <- c(100, 10)
threshold_names <- c("ALL", "TOP 10%")
names(threshold_values) <- threshold_names

#Variables for TAB 2 Threshold Buttons
threshold2_values <- c(100, 20, 10, 5, "LAC")
threshold2_names <- c("ALL SCHOOLS", "TOP 20%", "TOP 10%", "TOP 5%", "Liberal Arts Colleges")
names(threshold2_values) <- threshold2_names


############
#    ui    #
############

ui <- navbarPage(id = "nav", 
                 
                 #Bootswatch theme
                 theme = bs_theme(base_font = font_google("Lato"),  heading_font = font_google("Lato"), version = 4, bootswatch = "lux"),  
                 
                 #Add green logo to navigation bar
                 title = img(src = "green.png", height = 100, width = 90),
                 
                 tabPanel("MAP",
                          div(class="outer",
                              
                              tags$head(tags$style(
                              
                            '#controls {
                              background-color: #edf1f2;
                              padding: 0 20px 0px 20px;
                              cursor: move;
                              opacity: .7;
                              zoom: 0.9;
                              transition: opacity 500ms 1s;}
                                     
                            #controls:hover {
                              opacity: 0.97;
                              transition-delay: 0;}
                                     
                            div.outer {
                              position: fixed;
                              top: 160px;
                              left: 0;
                              right: 0;
                              bottom: 0;
                              overflow: hidden;
                              padding: 0;}
                                       
                            #threshold {
                              line-height:2.2;}
                                                       
                            .leaflet .legend {
                              text-align: left}
                                     
                            .leaflet .legend h6{
                              text-align: center;
                              font-size: 12px}
                                     
                            .leaflet .legend h5{
                              text-align: center;
                              font-size: 9px;
                              color: grey;
                              margin-top: -4px}'
                            
                            )),
                              
                              mainPanel(
                                align = "center", width = "100%", 
                                #Side panel with controls floats on right side of the map
                                absolutePanel(id = "controls", fixed = TRUE, draggable = TRUE, top = 250, left = "auto", right = 20,bottom = "auto", width = '25%', height = 600, style = "z-index: 10;",
                                              #Select input for map variables
                                              div(style = "margin-top: 20px;",
                                                  pickerInput(inputId = "map_var",  
                                                              label = '',
                                                              choices = score_values,
                                                              options = list(`style` = "btn-primary", 'dropdownAlignRight' = TRUE),
                                                              selected = "rating")),
                                              
                                              #Render description based on variable selection
                                              div(style = "margin-top: 20px; font-size:12px; text-transform:uppercase; color:#3e84bd", htmlOutput("description")),
                                              
                                              #Select input for report card variables; panel only appears when total is selected 
                                              conditionalPanel(condition = "input.map_var == 'total'",
                                                                   pickerInput(inputId = "report_var",
                                                                               label = '',
                                                                               choices = report_values,
                                                                               options = list(`style` = "btn-danger", "dropupAuto" = FALSE)),
                                                                   hr()),
                                              hr(),
                                              
                                              #Switch input for toggling search feature
                                              div(style = "margin-top: 20px", 
                                                  switchInput(inputId = "search_switch",
                                                              label = "SEARCH",
                                                              value = FALSE,
                                                              labelWidth = "80px"
                                                  )),
                                              
                                              #Search input
                                              conditionalPanel(condition = "Boolean(input.search_switch)",
                                                               selectizeInput(inputId = "map_search",
                                                                              label = tags$div("SEARCH FOR AN INSTITUTION:", style = 'font-size:10.5px; font-style: bold'), 
                                                                              choices = map$institution,
                                                                              selected = "Amherst College", 
                                                                              multiple = TRUE)
                                              ),
                                              
                                              hr(),
                                              #Radio buttons for filtering institutionsonly appears for non-categorical variables
                                              conditionalPanel(condition = "input.map_var !== 'rating' & input.map_var !== 'climate' & input.map_var !== 'locale' & input.map_var !== 'monday'",
                                                               prettyRadioButtons(inputId = "threshold", 
                                                                                  label = tags$div("VIEW:", style = 'font-size:13px; font-weight: bold; line-height:1.2'),
                                                                                  choices = threshold_values, 
                                                                                  selected = 100, status = "danger", animation = "pulse", inline = TRUE), hr(),
                                              )),
                                        
                                        leafletOutput(outputId = "map2", height=700))
                              
                          )),
                 
                 tabPanel(
                   title = "PLOT",
                   
                   mainPanel(align = "center", width = "80%",
                             
                             fluidRow(
                               column(width = 6, align = "right",
                                      div(style = "  font-size: 12px; text-transform:uppercase;",
                                          #Select input for all y-axis variables
                                          pickerInput(inputId = "score_var",
                                                      label = div("VIEW SCORE:", style = "font-size:12px; font-weight:bold; color:black; margin-bottom:10px"),
                                                      choices = bar_values,
                                                      selected = "total",
                                                      options = list(`style` = "btn-info"),
                                                      width = "83%"))),
                               
                               column(width = 6, align = "left", 
                                      div(style = "",
                                          #Select input for report card variables
                                          conditionalPanel(
                                            condition = "input.score_var == 'total'", #Only appears when total is selected
                                            pickerInput(inputId = "report2_var",
                                                        label = div("REPORT CARD", style = "font-size:12px; font-weight:bold; color:#626663; margin-bottom:10px"),
                                                        choices = report_values,
                                                        options = list(`style` = "btn-primary", "dropupAuto" = FALSE),
                                                        width = "83%")))
                               )
                             ),
                             
                             fluidRow(
                               column(width = 12, align = "center", 
                                      div(style = "margin-top:10px",
                                          #Search input
                                          selectizeInput(inputId = "search_var",
                                                         label = div("SEARCH FOR AN INSTITUTION:", style = "font-size:11px; "), 
                                                         choices = map$institution,
                                                         selected = "Amherst College",
                                                         multiple = TRUE,
                                                         width = "70%"))
                               )),
                             
                             div(style = "font-size:13px;",
                                 prettyRadioButtons(inputId = "threshold2", 
                                                    label = "", 
                                                    choices = threshold2_values, 
                                                    selected = 100, 
                                                    status = "danger", animation = "pulse", inline = TRUE)),
                             
                             #Render reactive title for plot based on input variable
                             div(style = "margin-top:30px; font-size:17px; text-transform:uppercase; color:#3e84bd; line-height:1", textOutput("plot_title")),
                             
                             fluidRow(
                               column(3, align = "left",  div(style = "font-size:9px; text-align: center; text-transform:uppercase; margin-bottom: -200px; margin-right: 40px; margin-top: 180px; transform: rotate(-90deg)", 
                                                              #Styled y-axis title
                                                              htmlOutput("message"))),
                               column(11, align = "right", div(style = "margin-left:12%; margin-right: -8%;  z-index: 2", plotOutput(outputId = "schools_bar")))
                             ),
                             
                             #Styled x-axis title
                             div(style = "font-align: center; margin-left:12%; font-size:11px; text-transform:uppercase; margin-top: 2px; margin-bottom: 180px; margin-right: 10%", p("Percentile of Reporting Institutions"))
                             
                   )
                 )
)


############
# server   #
############

server <- function(input, output, session){
  
  # Account for 3200 metric tons of CO2, or 17.5% reduction from New England College Renewable Partnership project
  map[3, 29] <- lapply(map[3, 29], function (x) x*(0.825))
  
  # TAB 1: MAP
  
  #Plot leaflet map 
  output$map2 <- renderLeaflet({
    leaflet(data = map) %>% 
      addTiles() %>% 
      #Set map default map view to center on the US
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })
  
  #Create new copy of data set for map
  y <- map[c(1:6)]
  
  observe({
    
    #Threshold variable which takes in the radio button input and filters data set for top n% of bars
    m_bars <- as.numeric(input$threshold)*(nrow(y)/100)
    
    #Select input variables
    mapMain <- input$map_var
    mapReport <- input$report_var
    
    #Function to mutate input variable directly
    if(mapMain != "climate" & mapMain != "locale" & mapMain != "monday"){
      
      if(mapMain == "total" & mapReport == "total"){
        y <- map[c(1:5, 6)]
      } else if(mapMain == "total" & mapReport == "curriculum"){
        y <- map[c(1:5, 7)] 
      } else if(mapMain == "total" & mapReport == "researching"){
        y <- map[c(1:5, 8)] 
      } else if(mapMain == "total" & mapReport == "campus_engagement"){
        y <- map[c(1:5, 9)] 
      } else if(mapMain == "total" & mapReport == "public_engagement"){
        y <- map[c(1:5, 10)] 
      } else if(mapMain == "total" & mapReport == "air"){
        y <- map[c(1:5, 11)] 
        y <- y %>% filter(!is.na(air)) %>% arrange(air) %>% top_n(m_bars)
      } else if(mapMain == "total" & mapReport == "buildings"){
        y <- map[c(1:5, 12)] 
      } else if(mapMain == "total" & mapReport == "energy"){
        y <- map[c(1:5, 13)] 
      } else if(mapMain == "total" & mapReport == "food"){
        y <- map[c(1:5, 14)] 
      } else if(mapMain == "total" & mapReport == "planning"){
        y <- map[c(1:5, 15)] 
      } else if(mapMain == "total" & mapReport == "diversity"){
        y <- map[c(1:5, 16)] 
      } else if(mapMain == "total" & mapReport == "investment"){
        y <- map[c(1:5, 17)] 
      } else if(mapMain == "total" & mapReport == "wellbeing"){
        y <- map[c(1:5, 18)] 
      } else if(mapMain == "endowment"){
        y <- map[c(1:5, 19)] %>% 
          filter(!is.na(endowment)) %>% 
          #Create new variable for formatting endowment labels on the map
          mutate(endowment_lab = ifelse(endowment < 1000000000, 
                                        paste("$", round(endowment/1000000, digits = 1), " MIL", sep = ""), 
                                        paste("$", round(endowment/1000000000, digits = 1), " BIL", sep = ""))) %>% 
          relocate(endowment_lab, .before = "endowment")
      } else if(mapMain == "area"){
        y <- map[c(1:5, 20)] 
      } else if(mapMain == "size"){
        y <- map[c(1:5, 21)] 
      } else if(mapMain == "classes"){
        y <- map[c(1:5, 22)] 
      } else if(mapMain == "renewables"){
        y <- map[c(1:5, 23)] 
      } else if(mapMain == "water"){
        y <- map[c(1:5, 24)] 
      } else if(mapMain == "waste"){
        y <- map[c(1:5, 25)] 
      } else if(mapMain == "recycling"){
        y <- map[c(1:5, 26)] 
      } else if(mapMain == "resesarch"){
        y <- map[c(1:5, 27)] 
      } else if(mapMain == "real_food"){
        y <- map[c(1:5, 28)] 
      } else if(mapMain == "gge"){
        y <- map[c(1:5, 29)] 
      } else if(mapMain == "plant_based"){
        y <- map[c(1:5, 30)] 
      }
      
      #Filter out NA and arrange from highest to lowest value
      #Select top n % of institutions
      y <- y %>% filter(!is.na(mapMain)) %>% arrange(mapMain) %>% top_n(m_bars)
      
    } else if(mapMain == "climate" | mapMain == "locale" | mapMain == "monday") {
      
      if(mapMain == "climate"){
        y <- map[c(1:5, 31)] 
      } else if(mapMain == "locale"){
        y <- map[c(1:5, 32)] 
      } else if(mapMain == "monday"){
        y <- map[c(1:5, 34)] 
      }
      
      #Filter out NA and arrange from highest to lowest value
      y <- y %>% filter(!is.na(mapMain)) %>% arrange(mapMain) 
      
    }
    
    
    #Generate reactive legend title
    if (mapMain != "climate" & mapMain != "locale" & mapMain != "monday" & mapMain != "total") {
      legend_title <- htmltools::HTML(paste0('<h6>', score_names[score_values == mapMain], '</h6>', '<h5>', "percentile", '</h5>', '<hr>'))
    } else if (mapMain == 'total' & mapReport == 'total'){
      legend_title <- htmltools::HTML(paste0('<h6>', 'Total Score', '</h6>', '<h5>', "percentile", '</h5>', '<hr>'))
    } else if (mapMain == 'total' & mapReport != 'total'){
      legend_title <- htmltools::HTML(paste0('<h6>', report_names[report_values == mapReport], '</h6>', '<h5>', "percentile", '</h5>', '<hr>'))
    } else {
      legend_title <- htmltools::HTML(paste0('<h6>', score_names[score_values == mapMain], '</h6>', '<hr>'))
    }
    
    
    #Function to transform non-numeric legend values
    if(mapMain == "climate"){
      legend_var <- function(x) c("Very Hot", "Hot", "Warm", "Mixed", "Cool", "Cold", "Very Cold")
    } else if(mapMain == "locale"){
      legend_var <- function(x) c("Large City", "Large Town", "Mid-Size City", "Small Town", "Rural", "Urban Fringe of Large City", "Urban Fringe of Mid-Size City")
    } else if(mapMain == "monday"){
      legend_var <- function(x) c("No", "Yes")
    } else {legend_var <- function(x) 5}
    
    #Label variable containing institution, state, rating, and score information
    school_label <- paste0('<b>', y[[1]], '</b><br>', y[[2]], ' | ', y[[5]],' | ', y[[6]])
    
    #Extract values for selected variable to define color palette
    colorData <- y[[mapMain]]
    
    ##IF statement : generates leaflet map for each case
    #On ratings page, generate icon markers
    if (mapMain == "rating") {
      
      #When search feature is OFF, display labels for markers when mouse hovers over
      if (input$search_switch == FALSE) {
        
        leafletProxy("map2", data = map) %>%  clearControls() %>%  clearMarkers() %>%
          addMarkers(~long,  ~lat,  icon = ~medalIcons[rating],
                     #Pass label function through and hide labels
                     label = lapply(school_label, htmltools::HTML),
                     labelOptions = labelOptions(noHide = FALSE, direction = "bottom",
                                                 style = list("font-family" = "lato"))) %>% 
          #Legend for rating icons
          addLegendImage(c('icons/platinum.png','icons/gold.png', 'icons/silver.png', 'icons/bronze.png'), 
                         title = htmltools::HTML(paste0('<h6>', 'Rating', '</h6>', '<hr>')),
                         labels = c("Platinum", "Gold", "Silver", "Bronze"), labelStyle = "font-size:12px")
        
      } else { #when search feature is ON, display only labels for institutions when searched for
        
        #Create new column for label to display only when an institution is searched for
        y <- y %>% mutate(label = ifelse(!is.null(input$map_search) & institution %in% input$map_search, school_label, ""))
        
        #Update map
        leafletProxy("map2", data = map) %>% clearControls() %>% clearMarkers() %>%
          addMarkers(~long, ~lat, icon = ~medalIcons[rating], label = lapply(y[[7]], htmltools::HTML),
                     #Show all labels and use new column to label variables
                     labelOptions = labelOptions(noHide = TRUE, direction = "bottom", textOnly = TRUE,
                                                 style = list("font-size" = "13px", "font-family" = "lato", "color" = "#2c2736",
                                                              "font-family" = "lato", "text-shadow" = "1px 1px 1px #ffffff"))) %>%
          addLegendImage(c('icons/platinum.png','icons/gold.png', 'icons/silver.png', 'icons/bronze.png'),
                         title = htmltools::HTML(paste0('<h6>', 'Rating', '</h6>', '<hr>')),
                         labels = c("Platinum", "Gold", "Silver", "Bronze"), labelStyle = "font-size:12px")
      }
      
    } else { #When not viewing Ratings, generate circle markers instead
      
      
      #Use colorFactor for climate, locale, and monday because the values are categorical instead of continuous.
      if (mapMain == "climate" | mapMain == "locale") {
        pals <- colorFactor("plasma", unique(colorData), reverse = TRUE)
      } else if (mapMain == "monday") {
        pals <- colorFactor(c("#e07767", "#77c775"), unique(colorData))
      #When viewing report card scores, use values from report card selector
      } else if (mapMain == "total") {
        colorData <- y[[mapReport]]
        pals <- colorQuantile(c("#520b06", "#cf3227", "#de6c49", "#eb976a", "#ffe291"), unique(colorData), 5, reverse = TRUE)
      } else if (mapMain != "rating" | mapMain != "climate" | mapMain != "locale" | mapMain != "monday"){
        pals <- colorQuantile(c("#520b06", "#cf3227", "#de6c49", "#eb976a", "#ffe291"), unique(colorData), 5, reverse = TRUE)
      } 
      
     
      if (input$search_switch == FALSE) { #When search feature is OFF, display labels for markers when mouse hovers over
        
        leafletProxy("map2", data = y) %>%
          clearControls() %>%
          clearMarkers() %>%
          #Pass label function through and hide labels
          addCircleMarkers(~long, ~lat, label = lapply(school_label, htmltools::HTML),
                           labelOptions = labelOptions(noHide = FALSE, direction = "bottom", style = list("font-family" = "lato")),
                           radius = 3, stroke = FALSE, fillOpacity = 1, fillColor = pals(colorData), weight = 0) %>% 
          #Legend with transformed values from legend_var function
          addLegend("topleft", pal = pals, values = colorData, title = legend_title, labFormat = labelFormat(transform = legend_var))
        
      } else if (input$search_switch == TRUE) { #When search feature is ON, display only labels for institutions when searched for
        
        #New column for searched institutions 
        y <- y %>% mutate(label = ifelse(!is.null(input$map_search) & institution %in% input$map_search, school_label, ""))
        #Relocate variable in endowment case with extra column
        if (mapMain == "endowment"){ y <- y %>% relocate(label, .before = "endowment") }
        
        leafletProxy("map2", data = y) %>%
          clearControls() %>%
          clearMarkers() %>%
          #Show all labels and use new column to label variables
          addCircleMarkers(~long, ~lat, label = lapply(y[[7]], htmltools::HTML),
                           labelOptions = labelOptions(noHide = TRUE, direction = "bottom", textOnly = TRUE,
                                                       style = list("font-size" = "13px", "font-family" = "lato", "color" = "#2c2736",
                                                                    "font-family" = "lato", "text-shadow" = "1px 1px 1px #ffffff")),
                           radius = 3,stroke = FALSE, fillOpacity = 1, fillColor = pals(colorData), weight = 0) %>%
          #Legend with transformed values from legend_var function
          addLegend("topleft", pal = pals, values = colorData, title = legend_title, labFormat = labelFormat(transform = legend_var))
      }
    }
    
  })
  
  #Update search input to only show institutions when data is available
  observe({
    if (!is.null(input$map_search)){
      update <- input$map_search
      updateSelectizeInput(session, "map_search", choices = y$institution, selected = update)
    }
  })
  
  #Medal icons for ratings map
  medalIcons <- iconList(
    Platinum = makeIcon(iconUrl = "icons/platinum.png", iconWidth = 15, iconHeight = 15),
    Gold = makeIcon(iconUrl = "icons/gold.png", iconWidth = 10, iconHeight = 11),
    Silver = makeIcon(iconUrl = "icons/silver.png", iconWidth = 10, iconHeight = 11),
    Bronze = makeIcon(iconUrl = "icons/bronze.png", iconWidth = 10, iconHeight = 11)
  )
  
  #Render text description for selected map variable
  output$description <- renderUI({
    
    #Select input variables
    mapMain <- input$map_var
    mapReport <- input$report_var
    
    if(mapMain == "rating"){
      HTML(paste(""))
    } else if(mapMain == "total"){
      HTML(paste("report card categories:"))
    } else if(mapMain == "endowment"){
      HTML(paste("$"))
    } else if(mapMain == "area"){
      HTML(paste("acres"))
    } else if(mapMain == "size"){
      HTML(paste("weighted campus users"))
    } else if(mapMain == "classes"){
      HTML(paste("% of classses engaged in sustainability"))
    } else if(mapMain == "renewables"){
      HTML(paste("% of energy sourced from renewables"))
    } else if(mapMain == "water"){
      HTML(paste("gallons of water consumed per person"))
    } else if(mapMain == "waste"){
      HTML(paste("metric tons of waste produced per person"))
    } else if(mapMain == "recycling"){
      HTML(paste("% of waste diverted to recycling or compost"))
    } else if(mapMain == "real_food"){
      HTML(paste("% of purchased food verified as REAL"))
    } else if(mapMain == "gge"){
      HTML(paste("annual greenhouse gas emmissions (metric tons)"))
    } else if(mapMain == "plant_based"){
      HTML(paste("% of purchased food that's plant-based"))
    } else if(mapMain == "climate"){
      HTML(paste("zone"))
    } else if(mapMain == "locale"){
      HTML(paste(""))
    } else if(mapMain == "monday"){
      HTML(paste("meatless monday"))
    }
  })
  
  
  ## TAB 2: PLOT
  
  x <- map[c(1:6)]
  
  observe({
    
    #Select input variables
    barMain <- input$score_var
    barReport <- input$report2_var
    
    if(input$threshold2 != "LAC") { #Threshold variable to select top n % of institutions
      n_bars <- as.numeric(input$threshold2)*(nrow(x)/100)
    } else { #Threshold variable to select Liberal Arts Colleges - schools in us_news dataset
      map <- map %>% filter(!is.na(IPEDS.ID))
      #Set n_bars to match number of LACs in data set
      n_bars <- 53
    }
    
    if(barMain == "total"){ #When selecting from report card variables
      
        if(barReport == "total"){
          x <- map[c(1:5, 6)]
          x <- x %>% filter(!is.na(total)) %>% arrange(total) %>% top_n(n_bars)
        } else if(barReport == "curriculum"){
          x <- map[c(1:5, 7)] 
          x <- x %>% filter(!is.na(curriculum)) %>% arrange(curriculum) %>% top_n(n_bars)
        } else if(barReport == "researching"){
          x <- map[c(1:5, 8)] 
          x <- x %>% filter(!is.na(researching)) %>% arrange(researching) %>% top_n(n_bars)
        } else if(barReport == "campus_engagement"){
          x <- map[c(1:5, 9)] 
          x <- x %>% filter(!is.na(campus_engagement)) %>% arrange(campus_engagement) %>% top_n(n_bars)
        } else if(barReport == "public_engagement"){
          x <- map[c(1:5, 10)] 
          x <- x %>% filter(!is.na(public_engagement)) %>% arrange(public_engagement) %>% top_n(n_bars)
        } else if(barReport == "air"){
          x <- map[c(1:5, 11)] 
          x <- x %>% filter(!is.na(air)) %>% arrange(air) %>% top_n(n_bars)
        } else if(barReport == "buildings"){
          x <- map[c(1:5, 12)] 
          x <- x %>% filter(!is.na(buildings)) %>% arrange(buildings) %>% top_n(n_bars)
        } else if(barReport == "energy"){
          x <- map[c(1:5, 13)] 
          x <- x %>% filter(!is.na(energy)) %>% arrange(energy) %>% top_n(n_bars)
        } else if(barReport == "food"){
          x <- map[c(1:5, 14)] 
          x <- x %>% filter(!is.na(food)) %>% arrange(food) %>% top_n(n_bars)
        } else if(barReport == "planning"){
          x <- map[c(1:5, 15)] 
          x <- x %>% filter(!is.na(planning)) %>% arrange(planning) %>% top_n(n_bars)
        } else if(barReport == "diversity"){
          x <- map[c(1:5, 16)] 
          x <- x %>% filter(!is.na(diversity)) %>% arrange(diversity) %>% top_n(n_bars)
        } else if(barReport == "investment"){
          x <- map[c(1:5, 17)] 
          x <- x %>% filter(!is.na(investment)) %>% arrange(investment) %>% top_n(n_bars)
        } else if(barReport == "wellbeing"){
          x <- map[c(1:5, 18)] 
          x <- x %>% filter(!is.na(wellbeing)) %>% arrange(wellbeing) %>% top_n(n_bars)
        }
      
    } else { #When selecting variables not from report card
      
        if(barMain == "endowment"){
          x <- map[c(1:5, 19)] 
          x <- x %>% filter(!is.na(endowment)) %>% arrange(endowment) %>% top_n(n_bars) %>% mutate(endowment = endowment/1000000000)
        } else if(barMain == "area"){
          x <- map[c(1:5, 20)] 
          x <- x %>% filter(!is.na(area)) %>% arrange(area) %>% top_n(n_bars)
        } else if(barMain == "size"){
          x <- map[c(1:5, 21)] 
          x <- x %>% filter(!is.na(size)) %>% arrange(size) %>% top_n(n_bars)
        } else if(barMain == "classes"){
          x <- map[c(1:5, 22)] 
          x <- x %>% filter(!is.na(classes)) %>% arrange(classes) %>% top_n(n_bars)
        } else if(barMain == "renewables"){
          x <- map[c(1:5, 23)] 
          x <- x %>% filter(!is.na(renewables)) %>% arrange(renewables) %>% top_n(n_bars)
        } else if(barMain == "water"){
          x <- map[c(1:5, 24)] 
          x <- x %>% filter(!is.na(water)) %>% arrange(water) %>% top_n(n_bars)
        } else if(barMain == "waste"){
          x <- map[c(1:5, 25)] 
          x <- x %>% filter(!is.na(waste)) %>% arrange(waste) %>% top_n(n_bars)
        } else if(barMain == "recycling"){
          x <- map[c(1:5, 26)] 
          x <- x %>% filter(!is.na(recycling)) %>% arrange(recycling) %>% top_n(n_bars)
        } else if(barMain == "resesarch"){
          x <- map[c(1:5, 27)] 
          x <- x %>% filter(!is.na(resesarch)) %>% arrange(resesarch) %>% top_n(n_bars)
        } else if(barMain == "real_food"){
          x <- map[c(1:5, 28)] 
          x <- x %>% filter(!is.na(real_food)) %>% arrange(real_food) %>% top_n(n_bars)
        } else if(barMain == "gge"){
          x <- map[c(1:5, 29)] 
          x <- x %>% filter(!is.na(gge)) %>% arrange(gge) %>% top_n(n_bars)
        } else if(barMain == "plant_based"){
          x <- map[c(1:5, 30)] 
          x <- x %>% filter(!is.na(plant_based)) %>% arrange(plant_based) %>% top_n(n_bars)
        }
      
    }
    
    #Update search input to only show institutions when data is available
    observe({
      if (!is.null(input$search_var)){
        edit <- input$search_var
        updateSelectizeInput(session, "search_var", choices = x$institution, selected = edit)
      }
    })
    
    ##Create x-axis label breaks for visualization clarity
    #Dependent on threshold value
    
    if(input$threshold2 == 100 | input$threshold2 == "LAC"){
      percentile_label <- c("0%", "20%", "40%", "60%", "80%", "100%") #Break labels
      breaks <- 5 #Set number of breaks
    } else if (input$threshold2 == 20){
      percentile_label <- c("80%", "85%", "90%", "95%", "100%")
      breaks <- 4
    } else if (input$threshold2 == 10){
      percentile_label <- c("90%", "92.5%", "95%", "97%", "100%")
      breaks <- 4
    } else if (input$threshold2 == 5 & nrow(x) < 15){
      breaks <- 14
      #Use dashes to add institution names without percentile labels
      percentile_label <- c("95%", "________", "________", "________", "________", "________", "________", "________", "________", "________", "________", "________", "________", "________", "100%")
    } else if (input$threshold2 == 5 & nrow(x) >= 15){
      breaks <- 6
      #Use dashes to add institution names without percentile labels
      percentile_label <- c("95%", "________", "________", "________", "________", "________", "100%")
    }
    
    ## Arrange institutions on x-axis in ascending order
    
    x$institution <- as.vector(x$institution) #Get rid of factors
    x$institution = factor(x$institution, x$institution) #Add back ordered factors 
    
    #Create list for storing label values
    plot_label <- c(as.character(x$institution[1])) #Add lowest scoring institution to the beginning of the list
    
    for(i in seq_len(breaks)){ #Sequence through list to create breaks depending on threshold value
      plot_label[i+1] <- as.character(x$institution[round((i/breaks)*nrow(x))]) 
    }
    
    plot_label[breaks+1] <- as.character(x$institution[nrow(x)]) #Add highest scoring institution to the end of the list
    
    ##
    
    #Variable to highlight institutions from search bar
    #Categorical variable fill = 1 or 0 depending on whether or not institution has been searched for
    x <- x %>% mutate(fill = ifelse(!is.null(input$map_search) & institution %in% input$search_var, 0, 1))
    
    #Variable to alternate between main select input and report card input for y-axis 
    y_val <- ifelse(barMain == "total" & barReport != "total", barReport, barMain)
    
    #Reactive bar plot
    output$schools_bar <- renderPlot({
      
      ggplot(x, aes_string(x = "institution", y = y_val)) +
        geom_bar(stat = "identity", position = position_dodge(width = 0.2), aes(fill = fill)) +
        labs(title = "", x = "", y = "") +
        #Wrap labels to avoid overlap on x-axis
        scale_x_discrete(breaks = plot_label, labels = stringr::str_wrap(paste(percentile_label, plot_label, sep = "\n"), width = 8)) +
        theme(legend.position = "none",
              plot.margin = margin(10, 100, 0, 0),
              axis.title.x = element_text(size = 11, margin = margin(t = 0, r = 0, b = 0, l = 0), color = "#66686b")) +
        #Generate text label for bars when institutions have been searched for
        geom_text_repel(data = filter(x, institution %in% input$search_var), aes(label = institution), 
                        nudge_y = 6, show.legend = FALSE, color = "black", segment.color = "grey")
    })
    
  })
  
  #Reactive title for plot
  output$plot_title <- renderText({
    paste(score_names[score_values == input$score_var])
  })
  
  #Render y-axis label based on selected variable
  output$message <- renderUI({
    
    #Select input variables
    barMain <- input$score_var
    barReport <- input$report2_var
    
    if(barMain == "total" & barReport == "total"){
      HTML(paste("Total Score"))
    } else if(barMain == "total" & barReport != "total"){
      HTML(paste(report_names[report_values == barReport], "Score"))
    } else if(barMain == "renewables"){
      HTML(paste("% of energy sourced from renewables"))
    } else if(barMain == "water"){
      HTML(paste("water consumed per campus user (gallons)"))
    } else if(barMain == "waste"){
      HTML(paste("metric tons of waste produced per campus user"))
    } else if(barMain == "recycling"){
      HTML(paste("% of waste diverted to recycling or compost"))
    } else if(barMain == "classes"){
      HTML(paste("% of classses focused on sustainability"))
    } else if(barMain == "gge"){
      HTML(paste("annual greenhouse gas emmissions (metric tons)"))
    } else if(barMain == "real_food"){
      HTML(paste("% of ethically or sustainably sourced food"))
    } else if(barMain == "endowment"){
      HTML(paste("$ billions"))
    } else if(barMain == "area"){
      HTML(paste("acres"))
    } else if(barMain == "size"){
      HTML(paste("weighted campus users"))
    } 
  })
  
}

####################
# call to shinyApp #
####################

shinyApp(ui = ui, server = server)