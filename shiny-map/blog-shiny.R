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

# Import STARS data
map <- read_csv("map.csv") 

## For TAB 1 MAP selectInput
score_values <- c("rating", "total", "endowment", "area", "size",  "climate", "locale", "renewables", "gge", "water", "waste", "recycling", "classes", "real_food", "plant_based", "monday") 
score_names <- c("Rating", "STARS Report Card", "Endowment", "Area", "Population", "Climate", "Locale", "Renewables", "Emmissions", "Water", "Waste", "Recycling", "Classes", "Food Sourcing", "Plant-Based", "Meatless Monday")
names(score_values) <- toupper(score_names)

report_values <- c("total", "curriculum", "researching", "campus_engagement", "public_engagement", "air", "buildings", "energy", "food", "planning", "diversity", "investment", "wellbeing")
report_names <- c("Total", "Curriculum", "Research", "Campus Engagement", "Public Engagement", "Air", "Buildings", "Energy", "Food", "Planning", "Diversity", "Investment", "Wellbeing")
names(report_values) <- toupper(report_names)

bar_values <- c("total", "renewables", "gge", "water", "waste", "recycling", "research", "classes", "real_food", "endowment", "area", "size")
bar_names <- c("STARS Report Card", "Renewables", "Emmissions", "Water", "Waste", "Recycling", "Research", "Classes", "Food Purchasing", "Endowment", "Area", "Population")
names(bar_values) <- bar_names

threshold_values <- c(100, 10)
threshold_names <- c("ALL", "TOP 10%")
names(threshold_values) <- threshold_names

threshold2_values <- c(100, 20, 10, 5, "LAC")
threshold2_names <- c("ALL SCHOOLS", "TOP 20%", "TOP 10%", "TOP 5%", "Liberal Arts Colleges")
names(threshold2_values) <- threshold2_names


############
#    ui    #
############

ui <- navbarPage(id = "nav",
                 
   theme = bs_theme(base_font = font_google("Lato"), 
                    heading_font = font_google("Lato"),
                    version = 4, bootswatch = "lux"),  

  title = img(src = "green.png", height = 100, width = 90),

  tabPanel("MAP",
           div(class="outer",
               
               tags$head(
                tags$style('#controls {
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
                              text-align: left;}
                                     
                            .leaflet .legend h6{
                              text-align: center;
                              font-size: 12px}
                                     
                            .leaflet .legend h5{
                              text-align: center;
                              font-size: 9px;
                              color: grey;
                              margin-top: -4px;}
                              ')),
    
      mainPanel(align = "center", width = "100%", 
                absolutePanel(id = "controls", fixed = TRUE,
                              draggable = TRUE, top = 300, left = "auto", right = 20, bottom = "auto",
                              width = '25%', height = 550, style = "z-index: 10;",
                              
                              pickerInput(inputId = "map_var",
                                          label = '',
                                          choices = score_values,
                                          options = list(`style` = "btn-primary",
                                                         'dropdownAlignRight' = T),
                                          selected = "rating"),
                              
                              div(style = "margin-top: 20px; font-size:12px; text-transform:uppercase; color:#3e84bd", htmlOutput("description")),
                              
                              conditionalPanel(
                                condition = "input.map_var == 'total'",
                                pickerInput(inputId = "report_var",
                                            label = '',
                                            choices = report_values,
                                            options = list(`style` = "btn-danger",
                                                           "dropupAuto" = FALSE))
                                ),
                              hr(),
                              conditionalPanel(
                                condition = "input.map_var !== 'rating' & input.map_var !== 'climate' & input.map_var !== 'locale' & input.map_var !== 'monday'",
                                prettyRadioButtons(inputId = "threshold", 
                                                   label = tags$div("VIEW:", style = 'font-size:13px; font-weight: bold; line-height:1.2'),
                                                   status = "danger",
                                                   animation = "pulse",
                                                   inline = T,
                                                   choices = threshold_values, 
                                                   selected = 100),
                                hr(),
                              ),
                              
                              div(style = "margin-top: 20px", 
                              switchInput(
                                inputId = "search_switch",
                                label = "SEARCH",
                                value = FALSE,
                                labelWidth = "80px"
                              )),
                              
                              conditionalPanel(
                                condition = "Boolean(input.search_switch)",
                                selectizeInput(inputId = "map_search",
                                               label = tags$div("SEARCH FOR AN INSTITUTION:", style = 'font-size:10.5px; font-style: bold'), 
                                               choices = map$institution,
                                               selected = "Amherst College",
                                               multiple = TRUE)
                                
                              ),
                              
                              ),
                
                leafletOutput(outputId = "map2", height=700))
      
    )),
  
  tabPanel(
    title = "PLOT",

      mainPanel(align = "center", width = "80%",
                
                fluidRow(
                  column(width=6, align = "right",
                         div(style = "margin-top:20px;  font-size: 12px; line-height: 3; text-transform:uppercase;",
                            pickerInput(inputId = "score_var",
                                        label = tags$h5("view score:"),
                                        choices = bar_values,
                                        selected = "total",
                                        options = list(`style` = "btn-info"),
                                        width = "83%"))),
                  
                  column(width=6, align = "left", 
                         div(style = "margin-top:20px;",
                             conditionalPanel(
                               condition = "input.score_var == 'total'",
                               pickerInput(inputId = "report2_var",
                                           label =  tags$h5('report card'),
                                           choices = report_values,
                                           options = list(`style` = "btn-primary",
                                                          "dropupAuto" = FALSE),
                                           width = "83%")))
                         )
                  ),
                
                fluidRow(
                  column(width=12, align = "center", 
                         div(style = "",
                             selectizeInput(inputId = "search_var",
                                            label = tags$h6("search:"), 
                                            choices = map$institution,
                                            selected = "Amherst College",
                                            multiple = TRUE,
                                            width = "70%"))
                )),
                
                prettyRadioButtons(inputId = "threshold2", 
                                   label = "", 
                                   status = "danger",
                                   animation = "pulse",
                                   inline = T,
                                   choices = threshold2_values, 
                                   selected = 100),
                
                br(),
                
                div(style = "margin-top:0px; font-size:17px; text-transform:uppercase; color:#3e84bd", textOutput("title_var3")),
                
                span(htmlOutput("message"), style = "font-size:11px; text-transform:uppercase; line-height:1.5"),
                
                div(style = "margin-left:10%, margin-right:10%;", plotOutput(outputId = "schools_bar")),
                
                )
    )
)

############
# server   #
############

server <- function(input, output, session){
  
  # TAB 1: MAP
  
  # Account for 3200 metric tons of CO2, or 17.5% reduction from New England College Renewable Partnership project
  map[3, 29] <- lapply(map[3, 29], function (x) x*(0.825))
  
  
  #Render reactive titles
  output$title_var2 <- renderText({
    paste(score_names[score_values == input$map_var])
  })
  
  output$title_var3 <- renderText({
    paste(score_names[score_values == input$score_var])
  })

  #Make icons for ratings map
  medalIcons <- iconList(
    Platinum = makeIcon(iconUrl = "icons/platinum.png",
                        iconWidth = 15, iconHeight = 15),
    Gold = makeIcon(iconUrl = "icons/gold.png",
                    iconWidth = 10, iconHeight = 11),
    Silver = makeIcon(iconUrl = "icons/silver.png",
                      iconWidth = 10, iconHeight = 11),
    Bronze = makeIcon(iconUrl = "icons/bronze.png",
                      iconWidth = 10, iconHeight = 11)
    
  )
  
  output$map2 <- renderLeaflet({
    
    leaflet(data = map) %>% 
      addTiles() %>% 
      setView(lng = -93.85, lat = 38.45, zoom = 4) %>% 
      addMarkers(~long, 
                 ~lat, 
                 group = "nc",
                 icon = ~medalIcons[rating],
                 label = lapply(paste0( '<b>', y[[1]], '</b><br>', y[[2]], ' | ', 
                                        y[[5]],' | ', y[[6]]), htmltools::HTML),
                 labelOptions = labelOptions(noHide = F, direction = "bottom",
                                             style = list(
                                               "font-size" = 12,
                                               "font-family" = "lato"))) %>% 
      addLegendImage(c('icons/platinum.png','icons/gold.png', 'icons/silver.png', 'icons/bronze.png'), 
                     title = htmltools::HTML(paste0('<h6>', 'Rating', '</h6>', '<hr>')), 
                     labels = c("Platinum", "Gold", "Silver", "Bronze"),
                     labelStyle = "font-size:12px") 
  })
  
  y <- map[c(1:6)]
  
  observe({
    
    m_bars <- as.numeric(input$threshold)*(nrow(y)/100)
    colorBy <- input$map_var
    colorReport <- input$report_var
    
    #Function to mutate input variable directly
    if(input$map_var == "total" & input$report_var == "total"){
      y <- map[c(1:5, 6)]
      y <- y %>% filter(!is.na(total)) %>% arrange(total) %>% top_n(m_bars)
    } else if(input$map_var == "total" & input$report_var == "curriculum"){
      y <- map[c(1:5, 7)] 
      y <- y %>% filter(!is.na(curriculum)) %>% arrange(curriculum) %>% top_n(m_bars)
    } else if(input$map_var == "total" & input$report_var == "researching"){
      y <- map[c(1:5, 8)] 
      y <- y %>% filter(!is.na(researching)) %>% arrange(researching) %>% top_n(m_bars)
    } else if(input$map_var == "total" & input$report_var == "campus_engagement"){
      y <- map[c(1:5, 9)] 
      y <- y %>% filter(!is.na(campus_engagement)) %>% arrange(campus_engagement) %>% top_n(m_bars)
    } else if(input$map_var == "total" & input$report_var == "public_engagement"){
      y <- map[c(1:5, 10)] 
      y <- y %>% filter(!is.na(public_engagement)) %>% arrange(public_engagement) %>% top_n(m_bars)
    } else if(input$map_var == "total" & input$report_var == "air"){
      y <- map[c(1:5, 11)] 
      y <- y %>% filter(!is.na(air)) %>% arrange(air) %>% top_n(m_bars)
    } else if(input$map_var == "total" & input$report_var == "buildings"){
      y <- map[c(1:5, 12)] 
      y <- y %>% filter(!is.na(buildings)) %>% arrange(buildings) %>% top_n(m_bars)
    } else if(input$map_var == "total" & input$report_var == "energy"){
      y <- map[c(1:5, 13)] 
      y <- y %>% filter(!is.na(energy)) %>% arrange(energy) %>% top_n(m_bars)
    } else if(input$map_var == "total" & input$report_var == "food"){
      y <- map[c(1:5, 14)] 
      y <- y %>% filter(!is.na(food)) %>% arrange(food) %>% top_n(m_bars)
    } else if(input$map_var == "total" & input$report_var == "planning"){
      y <- map[c(1:5, 15)] 
      y <- y %>% filter(!is.na(planning)) %>% arrange(planning) %>% top_n(m_bars)
    } else if(input$map_var == "total" & input$report_var == "diversity"){
      y <- map[c(1:5, 16)] 
      y <- y %>% filter(!is.na(diversity)) %>% arrange(diversity) %>% top_n(m_bars)
    } else if(input$map_var == "total" & input$report_var == "investment"){
      y <- map[c(1:5, 17)] 
      y <- y %>% filter(!is.na(investment)) %>% arrange(investment) %>% top_n(m_bars)
    } else if(input$map_var == "total" & input$report_var == "wellbeing"){
      y <- map[c(1:5, 18)] 
      y <- y %>% filter(!is.na(wellbeing)) %>% arrange(wellbeing) %>% top_n(m_bars)
    } else if(input$map_var == "endowment"){
      y <- map[c(1:5, 19)] 
      y <- y %>% filter(!is.na(endowment)) %>% arrange(endowment) %>% top_n(m_bars)
    } else if(input$map_var == "area"){
      y <- map[c(1:5, 20)] 
      y <- y %>% filter(!is.na(area)) %>% arrange(area) %>% top_n(m_bars)
    } else if(input$map_var == "size"){
      y <- map[c(1:5, 21)] 
      y <- y %>% filter(!is.na(size)) %>% arrange(size) %>% top_n(m_bars)
    } else if(input$map_var == "classes"){
      y <- map[c(1:5, 22)] 
      y <- y %>% filter(!is.na(classes)) %>% arrange(classes) %>% top_n(m_bars)
    } else if(input$map_var == "renewables"){
      y <- map[c(1:5, 23)] 
      y <- y %>% filter(!is.na(renewables)) %>% arrange(renewables) %>% top_n(m_bars)
    } else if(input$map_var == "water"){
      y <- map[c(1:5, 24)] 
      y <- y %>% filter(!is.na(water)) %>% arrange(water) %>% top_n(m_bars)
    } else if(input$map_var == "waste"){
      y <- map[c(1:5, 25)] 
      y <- y %>% filter(!is.na(waste)) %>% arrange(waste) %>% top_n(m_bars)
    } else if(input$map_var == "recycling"){
      y <- map[c(1:5, 26)] 
      y <- y %>% filter(!is.na(recycling)) %>% arrange(recycling) %>% top_n(m_bars)
    } else if(input$map_var == "resesarch"){
      y <- map[c(1:5, 27)] 
      y <- y %>% filter(!is.na(resesarch)) %>% arrange(resesarch) %>% top_n(m_bars)
    } else if(input$map_var == "real_food"){
      y <- map[c(1:5, 28)] 
      y <- y %>% filter(!is.na(real_food)) %>% arrange(real_food) %>% top_n(m_bars)
    } else if(input$map_var == "gge"){
      y <- map[c(1:5, 29)] 
      y <- y %>% filter(!is.na(gge)) %>% arrange(gge) %>% top_n(m_bars)
    } else if(input$map_var == "plant_based"){
      y <- map[c(1:5, 30)] 
      y <- y %>% filter(!is.na(plant_based)) %>% arrange(plant_based) %>% top_n(m_bars)
    } else if(input$map_var == "climate"){
      y <- map[c(1:5, 31)] 
      y <- y %>% filter(!is.na(climate)) %>% arrange(climate) 
    } else if(input$map_var == "locale"){
      y <- map[c(1:5, 32)] 
      y <- y %>% filter(!is.na(locale)) %>% arrange(locale) 
    } else if(input$map_var == "monday"){
      y <- map[c(1:5, 34)] 
      y <- y %>% filter(!is.na(monday)) %>% arrange(monday) 
    } 
    
    
    
    #Function for Legend title
    if (input$map_var != "climate" & input$map_var != "locale" & input$map_var != "monday" & input$map_var != "total") {
      legend_title <- htmltools::HTML(paste0('<h6>', score_names[score_values == colorBy], '</h6>', '<h5>', "percentile", '</h5>', '<hr>'))
    } else if (input$map_var == 'total' & input$report_var == 'total'){
      legend_title <- htmltools::HTML(paste0('<h6>', 'Total Score', '</h6>', '<h5>', "percentile", '</h5>', '<hr>'))
    } else if (input$map_var == 'total' & input$report_var != 'total'){
      legend_title <- htmltools::HTML(paste0('<h6>', report_names[report_values == colorReport], '</h6>', '<h5>', "percentile", '</h5>', '<hr>'))
    } else {
      legend_title <- htmltools::HTML(paste0('<h6>', score_names[score_values == colorBy], '</h6>', '<hr>'))
    }
    
    
    #Function for non-numeric legend values
    if(input$map_var == "climate"){
      cf <- function(x) c("Very Hot", "Hot", "Warm", "Mixed", "Cool", "Cold", "Very Cold")
    } else if(input$map_var == "locale"){
      cf <- function(x) c("Large City", "Large Town", "Mid-Size City", "Small Town", "Rural", "Urban Fringe of Large City", "Urban Fringe of Mid-Size City")
    } else if(input$map_var == "monday"){
      cf <- function(x) c("No", "Yes")
    } else {cf <- function(x) 5}
    
    
    #SEARCH SWITCH OFF
    if (input$search_switch == F) {
      
      #Function to set marker/color values and map each case
      if (colorBy == "climate" | colorBy == "locale") {
        # Color and palette are treated specially in these three cases because the values are categorical instead of continuous.
        colorData2 <- y[[colorBy]]
        pals <- colorFactor("plasma", unique(colorData2), reverse = TRUE)
        
      } else if (colorBy == "monday") {
        
        colorData2 <- y[[colorBy]]
        pals <- colorFactor(c("#e07767", "#77c775"), unique(colorData2))
        
      } else if (colorBy == "rating") {
        leafletProxy("map2", data = map) %>% 
          clearControls() %>% 
          clearMarkers() %>%
          addMarkers(~long, 
                     ~lat, 
                     icon = ~medalIcons[rating],
                     label = lapply(paste0( '<b>', y[[1]], '</b><br>', y[[2]], ' | ',
                                            y[[5]],' | ', y[[6]]), htmltools::HTML),
                     labelOptions = labelOptions(noHide = F, direction = "bottom",
                                                 style = list(
                                                   "font-family" = "lato"))) %>% 
          addLegendImage(c('icons/platinum.png','icons/gold.png', 
                           'icons/silver.png', 'icons/bronze.png'), 
                         title = htmltools::HTML(paste0('<h6>', 'Rating', '</h6>', '<hr>')),
                         labels = c("Platinum", "Gold", "Silver", "Bronze"),
                         labelStyle = "font-size:12px")
        
      } else if (colorBy == "total") {
        colorData2 <- y[[colorReport]]
        pals <- colorQuantile(c("#520b06", "#cf3227", "#de6c49", "#eb976a", "#ffe291"), unique(colorData2), 5, reverse = TRUE)
        
      } else if (colorBy != "rating" | colorBy != "climate" | colorBy != "locale" | colorBy != "monday"){
        colorData2 <- y[[colorBy]]
        pals <- colorQuantile(c("#520b06", "#cf3227", "#de6c49", "#eb976a", "#ffe291"), unique(colorData2), 5, reverse = TRUE)
        
      }
      
      
      #If not on ratings page, generate circle markers
      if (colorBy != "rating") {
        
        leafletProxy("map2", data = y) %>%
          clearControls() %>%
          clearMarkers() %>%
          addCircleMarkers(~long,
                           ~lat,
                           label = lapply(paste0( '<b>', y[[1]], '</b><br>', y[[2]], ' | ',
                                                  y[[5]],' | ', y[[6]]), htmltools::HTML),
                           labelOptions = labelOptions(noHide = F, direction = "bottom",
                                                       style = list("font-family" = "lato")),
                           radius = 3,
                           stroke = FALSE,
                           fillOpacity = 1,
                           fillColor= pals(colorData2),
                           weight = 0) %>% 
          addLegend("topleft", pal=pals, values=colorData2, title=legend_title,
                    layerId="colorLegend2", labFormat = labelFormat(transform = cf))
      }
    }
    
    
    #SEARCH SWITCH ON
    
    else{
      
      if (!is.null(input$map_search)){
        y <- y %>% mutate(label = ifelse(institution %in% input$map_search, 
                                         paste0( '<b>', y[[1]], '</b><br>', y[[2]], ' | ',
                                                 y[[5]],' | ', y[[6]]), ""))
      }
      else {
        y <- y %>% mutate(label = "")
      }
      
    #Function to set marker/color values and map each case
    if (colorBy == "climate" | colorBy == "locale") {
      
      colorData2 <- y[[colorBy]]
      pals <- colorFactor("plasma", unique(colorData2), reverse = TRUE)
      
    } else if (colorBy == "monday") {
      
      colorData2 <- y[[colorBy]]
      pals <- colorFactor(c("#e07767", "#77c775"), unique(colorData2))
      
    } else if (colorBy == "rating") {
      leafletProxy("map2", data = map) %>% 
        clearControls() %>% 
        clearMarkers() %>%
        addMarkers(~long, 
                   ~lat, 
                   icon = ~medalIcons[rating],
                   label = lapply(y[[7]], htmltools::HTML),
                   labelOptions = labelOptions(noHide = T, direction = "bottom",
                                               textOnly = TRUE,
                                               style = list(
                                                 "font-size" = "13px",
                                                 "font-family" = "lato",
                                                 "color" = "#2c2736",
                                                 "font-family" = "lato",
                                                 "text-shadow" = "1px 1px 1px #ffffff"))) %>% 
        addLegendImage(c('icons/platinum.png','icons/gold.png', 
                         'icons/silver.png', 'icons/bronze.png'), 
                       title = htmltools::HTML(paste0('<h6>', 'Rating', '</h6>', '<hr>')),
                       labels = c("Platinum", "Gold", "Silver", "Bronze"),
                       labelStyle = "font-size:12px")
      
    } else if (colorBy == "total") {
      colorData2 <- y[[colorReport]]
      pals <- colorQuantile(c("#520b06", "#cf3227", "#de6c49", "#eb976a", "#ffe291"), unique(colorData2), 5, reverse = TRUE)

    } else if (colorBy != "rating" | colorBy != "climate" | colorBy != "locale" | colorBy != "monday"){
      colorData2 <- y[[colorBy]]
      pals <- colorQuantile(c("#520b06", "#cf3227", "#de6c49", "#eb976a", "#ffe291"), unique(colorData2), 5, reverse = TRUE)

    }
    
    
    #If not on ratings page, generate circle markers
    if (colorBy != "rating") {

      leafletProxy("map2", data = y) %>%
        clearControls() %>%
        clearMarkers() %>%
        addCircleMarkers(~long,
                         ~lat,
                         label = lapply(y[[7]], htmltools::HTML),
                         labelOptions = labelOptions(noHide = T, direction = "bottom",
                                                     textOnly = TRUE,
                                                     style = list(
                                                       "font-size" = "13px",
                                                       "font-family" = "lato",
                                                       "color" = "#2c2736",
                                                       "font-family" = "lato",
                                                       "text-shadow" = "1px 1px 1px #ffffff")),
                         radius = 3,
                         stroke = FALSE,
                         fillOpacity = 1,
                         fillColor= pals(colorData2),
                         weight = 0) %>% 
        addLegend("topleft", pal=pals, values=colorData2, title=legend_title,
                  layerId="colorLegend2", labFormat = labelFormat(transform = cf))
    }
    }
    
    #Update search input to only show choices for when institution data is available
    observe({
      if (!is.null(input$map_search)){
        update <- input$map_search
        updateSelectizeInput(session, "map_search", choices = y$institution, selected = update)
      }
    })
    
  })
  
  #Render text for map variable description
  output$description <- renderUI({
    if(input$map_var == "rating"){
      HTML(paste(""))
    } else if(input$map_var == "total"){
      HTML(paste("report card categories:"))
    } else if(input$map_var == "endowment"){
      HTML(paste("$ Millions"))
    } else if(input$map_var == "area"){
      HTML(paste("acres"))
    } else if(input$map_var == "size"){
      HTML(paste("weighted campus users"))
    } else if(input$map_var == "classes"){
      HTML(paste("% of classses engaged in sustainability"))
    } else if(input$map_var == "renewables"){
      HTML(paste("% of energy sourced from renewables"))
    } else if(input$map_var == "water"){
      HTML(paste("gallons of water consumed per person"))
    } else if(input$map_var == "waste"){
      HTML(paste("metric tons of waste produced per person"))
    } else if(input$map_var == "recycling"){
      HTML(paste("% of waste diverted to recycling or compost"))
    } else if(input$map_var == "research"){
      HTML(paste("% of faculty engaged in sustainability research"))
    } else if(input$map_var == "real_food"){
      HTML(paste("% of purchased food verified as REAL"))
    } else if(input$map_var == "gge"){
      HTML(paste("annual greenhouse gas emmissions (metric tons)"))
    } else if(input$map_var == "plant_based"){
      HTML(paste("% of purchased food that's plant-bassed"))
    } else if(input$map_var == "climate"){
      HTML(paste("zone"))
    } else if(input$map_var == "locale"){
      HTML(paste(""))
    } else if(input$map_var == "monday"){
      HTML(paste("meatless monday"))
    }
  })
  
  
  ## TAB 2: PLOT
  
  x <- map[c(1:6)]
  
  observe({
    
    if(input$threshold2 != "LAC") {
      n_bars <- as.numeric(input$threshold2)*(nrow(x)/100)
    }
    else if(input$threshold2 == "LAC") {
      map <- map %>% filter(!is.na(IPEDS.ID))
      n_bars <- 53
    }
    
    #variable for alternating between different input variables on the y-axis 
    y_val <- ifelse(input$score_var == "total" & input$report2_var != "total", input$report2_var, input$score_var)
    
    if(input$score_var == "total" & input$report2_var == "total"){
      x <- map[c(1:5, 6)]
      x <- x %>% filter(!is.na(total)) %>% arrange(total) %>% top_n(n_bars)
    } else if(input$score_var == "total" & input$report2_var == "curriculum"){
      x <- map[c(1:5, 7)] 
      x <- x %>% filter(!is.na(curriculum)) %>% arrange(curriculum) %>% top_n(n_bars)
    } else if(input$score_var == "total" & input$report2_var == "researching"){
      x <- map[c(1:5, 8)] 
      x <- x %>% filter(!is.na(researching)) %>% arrange(researching) %>% top_n(n_bars)
    } else if(input$score_var == "total" & input$report2_var == "campus_engagement"){
      x <- map[c(1:5, 9)] 
      x <- x %>% filter(!is.na(campus_engagement)) %>% arrange(campus_engagement) %>% top_n(n_bars)
    } else if(input$score_var == "total" & input$report2_var == "public_engagement"){
      x <- map[c(1:5, 10)] 
      x <- x %>% filter(!is.na(public_engagement)) %>% arrange(public_engagement) %>% top_n(n_bars)
    } else if(input$score_var == "total" & input$report2_var == "air"){
      x <- map[c(1:5, 11)] 
      x <- x %>% filter(!is.na(air)) %>% arrange(air) %>% top_n(n_bars)
    } else if(input$score_var == "total" & input$report2_var == "buildings"){
      x <- map[c(1:5, 12)] 
      x <- x %>% filter(!is.na(buildings)) %>% arrange(buildings) %>% top_n(n_bars)
    } else if(input$score_var == "total" & input$report2_var == "energy"){
      x <- map[c(1:5, 13)] 
      x <- x %>% filter(!is.na(energy)) %>% arrange(energy) %>% top_n(n_bars)
    } else if(input$score_var == "total" & input$report2_var == "food"){
      x <- map[c(1:5, 14)] 
      x <- x %>% filter(!is.na(food)) %>% arrange(food) %>% top_n(n_bars)
    } else if(input$score_var == "total" & input$report2_var == "planning"){
      x <- map[c(1:5, 15)] 
      x <- x %>% filter(!is.na(planning)) %>% arrange(planning) %>% top_n(n_bars)
    } else if(input$score_var == "total" & input$report2_var == "diversity"){
      x <- map[c(1:5, 16)] 
      x <- x %>% filter(!is.na(diversity)) %>% arrange(diversity) %>% top_n(n_bars)
    } else if(input$score_var == "total" & input$report2_var == "investment"){
      x <- map[c(1:5, 17)] 
      x <- x %>% filter(!is.na(investment)) %>% arrange(investment) %>% top_n(n_bars)
    } else if(input$score_var == "total" & input$report2_var == "wellbeing"){
      x <- map[c(1:5, 18)] 
      x <- x %>% filter(!is.na(wellbeing)) %>% arrange(wellbeing) %>% top_n(n_bars)
    } else if(input$score_var == "endowment"){
      x <- map[c(1:5, 19)] 
      x <- x %>% filter(!is.na(endowment)) %>% arrange(endowment) %>% top_n(n_bars)
    } else if(input$score_var == "area"){
      x <- map[c(1:5, 20)] 
      x <- x %>% filter(!is.na(area)) %>% arrange(area) %>% top_n(n_bars)
    } else if(input$score_var == "size"){
      x <- map[c(1:5, 21)] 
      x <- x %>% filter(!is.na(size)) %>% arrange(size) %>% top_n(n_bars)
    } else if(input$score_var == "classes"){
      x <- map[c(1:5, 22)] 
      x <- x %>% filter(!is.na(classes)) %>% arrange(classes) %>% top_n(n_bars)
    } else if(input$score_var == "renewables"){
      x <- map[c(1:5, 23)] 
      x <- x %>% filter(!is.na(renewables)) %>% arrange(renewables) %>% top_n(n_bars)
    } else if(input$score_var == "water"){
      x <- map[c(1:5, 24)] 
      x <- x %>% filter(!is.na(water)) %>% arrange(water) %>% top_n(n_bars)
    } else if(input$score_var == "waste"){
      x <- map[c(1:5, 25)] 
      x <- x %>% filter(!is.na(waste)) %>% arrange(waste) %>% top_n(n_bars)
    } else if(input$score_var == "recycling"){
      x <- map[c(1:5, 26)] 
      x <- x %>% filter(!is.na(recycling)) %>% arrange(recycling) %>% top_n(n_bars)
    } else if(input$score_var == "resesarch"){
      x <- map[c(1:5, 27)] 
      x <- x %>% filter(!is.na(resesarch)) %>% arrange(resesarch) %>% top_n(n_bars)
    } else if(input$score_var == "real_food"){
      x <- map[c(1:5, 28)] 
      x <- x %>% filter(!is.na(real_food)) %>% arrange(real_food) %>% top_n(n_bars)
    } else if(input$score_var == "gge"){
      x <- map[c(1:5, 29)] 
      x <- x %>% filter(!is.na(gge)) %>% arrange(gge) %>% top_n(n_bars)
    } else if(input$score_var == "plant_based"){
      x <- map[c(1:5, 30)] 
      x <- x %>% filter(!is.na(plant_based)) %>% arrange(plant_based) %>% top_n(n_bars)
    } 

    
    #Update search input to only show choices for when institution data is available
    observe({
      if (!is.null(input$search_var)){
        edit <- input$search_var
        updateSelectizeInput(session, "search_var", choices = x$institution, selected = edit)
      }
    })

    #Deal with factors
    x$institution <- as.vector(x$institution) #get rid of factors
    x$institution = factor(x$institution, x$institution) #add ordered factors back
    
    #Determine x-axis label breaks for visualization clarity
    breaks <- 10
    v <- c(as.character(x$institution[1]))
    
    for(i in seq_len(breaks)){
      if(n_bars > 10){
        v[i+1] <- as.character(x$institution[round((i/breaks)*n_bars)])
      } else{
        v <- c(as.character(x$institution[1:n_bars]))
      }}
    
    v[breaks+1] <- as.character(x$institution[nrow(x)])
    label <-  function(x) stringr::str_wrap(x, width = 30)
    
    #Reactive bar plot
    output$schools_bar <- renderPlot({
      
      #Function to highlight institutions from search bar
      if(!is.null(input$search_var)){
        x <- x %>% mutate(fill = ifelse(institution %in% input$search_var, 0, 1))
      }
      else if (is.null(input$search_var)){
        x <- x %>% mutate(fill = 1)
      }
      
      ggplot(x, aes_string(x = "institution", y = y_val)) +
        geom_bar(stat = "identity", position = position_dodge(width=0.2), aes(fill = fill)) +
        labs(title = "", x = "", y = "") +
        scale_x_discrete(breaks = v, labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%")) +
        theme(legend.position = "none",
              plot.margin = margin(10, 100, 10, 100)) +
        geom_text_repel(data = filter(x, institution %in% input$search_var),
                        aes(label = institution), nudge_y = 6, show.legend = FALSE, 
                        colour="black", segment.colour="grey")
    })
    
  })

  #Render text for plot variable description
  output$message <- renderUI({
    if(input$score_var == "total" & input$report2_var == "total"){
      HTML(paste("Total Score"))
    } else if(input$score_var == "total" & input$report2_var != "total"){
      HTML(paste(report_names[report_values == input$report2_var], "Score"))
    } else if(input$score_var == "renewables"){
      HTML(paste("% of energy sourced from renewables"))
    } else if(input$score_var == "water"){
      HTML(paste("cubic gallons of water consumed per weighted campus user"))
    } else if(input$score_var == "waste"){
      HTML(paste("metric tons of waste produced per weighted campus user"))
    } else if(input$score_var == "recycling"){
      HTML(paste("% of waste diverted to recycling or compost"))
    } else if(input$score_var == "research"){
      HTML(paste("% of faculty engaged in sustainability research"))
    } else if(input$score_var == "classes"){
      HTML(paste("% of classses engaged in sustainability"))
    } else if(input$score_var == "real_food"){
      HTML(paste("% of food verified as REAL"))
    } else if(input$score_var == "endowment"){
      HTML(paste("$ Millions"))
    } else if(input$map_var == "area"){
      HTML(paste("acres"))
    } else if(input$map_var == "size"){
      HTML(paste("weighted campus users"))
    } 
    
  })
   
}

####################
# call to shinyApp #
####################

shinyApp(ui = ui, server = server)