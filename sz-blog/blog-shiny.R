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

library(kableExtra)
library(robotstxt) 
library(rvest) 
library(purrr)
library(janitor)
library(maps)
library(sf)
library(ggspatial)
library(fuzzyjoin)
library(leaflet)
library(leaflegend)



######################
#    Import Data     #
######################

# Import STARS data
schools <- read_csv("data/master.csv") 

# Import results
results <- read_csv("data/stars_df.csv")

#Import sustainability data
sustainability <- read_csv("data/sustainabilityrankings.csv")

#Import sustainability report
report <- read_csv(file = "data/amherstsustainabilityreport.txt")


## For TAB 1 MAP selectInput
map_choice_values <- c("total", "curriculum", "researching", "campus_engagement", "public_engagement", "air", "buildings", "energy", "food", "planning", "diversity", "investment", "wellbeing")
map_choice_names <- c("Total Score", "Curriculum", "Research", "Campus Engagement", "Public Engagement", "Air", "Buildings", "Energy", "Food", "Planning", "Diversity", "Investment", "Wellbeing")
names(map_choice_values) <- map_choice_names

# For TAB 4 selectizeInput: choices for school name, pull directly from data
school_choices <- unique(sustainability$Institution)


############
#    ui    #
############

ui <- navbarPage(
  
  theme = bs_theme(base_font = font_google("Lato"), 
                   heading_font = font_google("Lato"),
                   version = 4, bootswatch = "lux"),
  title = img(src = "green.png", height = 100, width = 100),
  

  
  # Tab 1: STARS
  
  tabPanel(
    title = "MAP",
    tags$head(tags$style('
      #sidebar2{
        width: 10%;
        margin-left: 15%;
        outline: none;
        border-width: 0;
        background-color: white} 
      
      #refill, #value{
        color: black;
        font-family: Lato;
        font-size:15px;
        line-height: 2}
      
      #textanalysis{
        color: black;
        font-family: Lato;
        margin-right: 80px;
        margin-left: 80px;
        font-size:15px;
        line-height: 2}
        
      p{
        width: 665px;
        font-size: 15px;
        text-align: justify}'
    )),
    
    sidebarLayout(
      sidebarPanel(id = "sidebar2", align = "center",
                   div(style = "text-align:left; margin-top:20px;
                           font-size: 13px; line-height: 2.6; text-transform:uppercase; font-weight:bold",
                       prettyRadioButtons(inputId = "map_var", 
                                          label = tags$h5(""),
                                          choices = map_choice_values,
                                          status = "danger",
                                          animation = "pulse",
                                          selected = "total"))
      ),
      mainPanel(align = "center",
                div(style = "margin-top: 50px; font-size:17px; margin-right:15%; text-transform:uppercase; color:#3e84bd", textOutput("title_var")),
                div(style = "margin-top: 40px; margin-right:10%", leafletOutput(outputId = "map")))
    ),
  ),
  
  # Tab 2: GRAPH
  
  tabPanel(
    title = "STARS RANKING",
    
    mainPanel(width = "100%", align = "center",
              div(style = "margin-top:30px",
                  h4("SCHOOL RANKING")),
              div(style = "margin-left:13%; margin-right:8%; margin-top:3%",
                  leafletOutput(outputId = "map_medals"))
    
  )),
  
  # Tab 5: Sustainability Rankings
  
  tabPanel(
    title = "TABLE",
    
    fluidRow(
      column(12, align="center", 
             h5("See how Amherst College stacks up against peer institutions:"))
    ),
    br(),
    fluidRow(
      
      column(12, align="center", selectizeInput(inputId = "school",
                                                label = "Choose one or more schools:",
                                                choices = school_choices,
                                                selected = "Amherst College",
                                                multiple = TRUE,
                                                width = "60%"))),
    
    
    mainPanel(width = "100%", align = "center", 
              
              img(src = "school.png", width = 400, height = 140),
              DT::dataTableOutput(outputId = "table"))
    
  ),
  
  # Tab 6: Text Analysis
  
  tabPanel(
    
    title = "AC SUSTAINABILITY",
    
    fluidRow(
      column(12, align="center", img(src = "report.png", height = 200, width = 225))
    ),
    br(),
    
    fluidRow(
      column(12, align="center", 
             p("Below is the Amherst Sustainability Report from October 2017 (most recent). 
                Enter in a desired keyword to receive the sections of the report that contain 
                that word (ex: justice):"))
    ),
    br(),
    
    fluidRow(
      column(12, align="center", textInput(inputId = "keyword",  
                                           label = "Type in keyword",
                                           width = "83%")),
      column(12, 
             mainPanel(width="100%",
                       textOutput(outputId = "textanalysis")))
      
    )
  )
  
)

############
# server   #
############

server <- function(input, output, session){
  
  # Tab 4: Table
  
  {
    data_for_table <- reactive({
      data <- filter(sustainability, Institution %in% input$school)
    })
    
    
    output$table <- DT::renderDataTable({ 
      data_for_table()
    })
  }  
  
  # Tab 5: Text Analysis 
  
  {
    data_for_text <- reactive({
      text_data <- report %>%
        str_split("\n") %>%
        pluck(1) %>%
        str_subset(input$keyword)
    })
    
    output$textanalysis <- renderText({ 
      data_for_text()
    })
  }
  


# TAB 1: MAP
  
map <- schools

pal0 <- colorFactor(c("#7aa7d6", "#f0dd60", "#9e9e9e", "#ba8a65"), 
                   domain = c("Platinum", "Gold", "Silver", "Bronze"))


output$title_var <- renderText({
  paste(map_choice_names[map_choice_values == input$map_var])
})

##
output$map <- renderLeaflet({
  leaflet(data = map) %>% 
    addTiles() %>%
    addCircleMarkers(
      ~long,
      ~lat,
      popup = ~institution,
      radius = 3,
      stroke = FALSE, 
      fillOpacity = 1,
      weight = 0)

})

observe({
  colorBy <- input$map_var

  colorData <- map[[colorBy]]
  pal <- colorQuantile("Blues", colorData , 8)

  leafletProxy("map", data = map) %>%
    clearShapes() %>%
    addCircleMarkers(~long,
               ~lat,
               popup = ~institution,
               radius = 3,
               stroke = FALSE, 
               fillOpacity = 1, 
               fillColor=pal(colorData), 
               weight = 0) %>%
    addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
              layerId="colorLegend")
})

# Make list of icons
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

output$map_medals <- renderLeaflet({
  leaflet(data = map) %>% 
    addTiles() %>%
    addMarkers(~long, 
               ~lat, 
               icon = ~medalIcons[rating],
               popup = ~institution) %>% 
    addLegendImage(c('icons/platinum.png','icons/gold.png', 
                     'icons/silver.png', 'icons/bronze.png'), 
                   title = htmltools::tags$div('schools',
                                               style = 'font-size: 14px; text-align: center; margin-bottom: 5px;'), 
                   labels = c("Platinum", "Gold", "Silver", "Bronze"),
                   labelStyle = "font-size:12px")
  
})


}

####################
# call to shinyApp #
####################

shinyApp(ui = ui, server = server)