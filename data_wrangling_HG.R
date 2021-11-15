#Data wrangling for top 30 sustainable colleges data
library(tidyverse)
library(robotstxt) 
library(rvest) 
library(purrr) 
library(readxl)
library(readr)
library(fuzzyjoin)
#url for dataset
Top30_url <- "https://www.epa.gov/greenpower/green-power-partnership-top-30-college-university"
#Confirm bots allowed to access page
paths_allowed(Top30_url)
#get data
Top30_data <- Top30_url %>%
  read_html() %>%
  html_elements("table") %>%
  pluck(1) %>%
  html_table

#Print table

Top30_data <- Top30_data %>%
  janitor::clean_names()

# Top30_text <- Top30_url %>%
#   read_html() %>%
#   html_elements("#listing") %>%
#   html_text()

#trying to read in my excel sheet

Top30 <- read_excel("~/Documents/COLLEGE/Data Science/Data Science/Top30.xlsx")

#reading in sustainability data
Institutions <- read_csv("data/Institutions.csv")
Tuition <- read_csv("data/Tuition Costs for Common Institutions.csv")
Degrees_by_County <- read_csv("data/Degrees Awarded by County.csv")
Growth_by_County <-  read_csv("data/Growth in Awarded Degrees.csv")

#working on making a map based on my data

#Getting college data

joined_uni <- left_join(Tuition, Institutions, by = c("ID University", "Year"))

View(joined_uni)
sum(is.na(joined_uni))

#cleaning up joined_uni data so only have information I need
clean_joined_uni <- joined_uni %>%
  janitor::clean_names() %>%
  select(id_university, university_x, year, state_tuition, total_graduates, out_of_state_tuition, sector, completions) %>%
rename("institution" = "university_x") 
fuzzy_uni <- stringdist_left_join(clean_joined_uni, colleges, by = "institution")
fuzzy_uni %>%
  filter(is.na(state)) %>%
  distinct(institution.x)
sum(is.na(fuzzy_uni$state))

uni_map <- fuzzy_uni %>%
  filter(!institution.x %in% c("Jefferson (Philadelphia University + Thomas Jefferson University)", "Messiah University", "Lyndon State College"))

#making map
uni_map <- uni_map %>%
mutate(size = case_when(out_of_state_tuition <= 20000 ~ 1,
                 (out_of_state_tuition > 20000) & (out_of_state_tuition <= 40000) ~ 2,
                 out_of_state_tuition > 40000 ~ 3)) %>%
  filter(!is.na(size))

sum(is.na(uni_map$size))
pal <- colorFactor(
  palette = 'Dark2',
  domain = uni_map$completions
)

leaflet(data = uni_map) %>% 
  addTiles() %>%
  addCircleMarkers(lat = ~lat, lng = ~long, popup = ~ paste0("Institution: ", institution.x, "\nOut of state tuition: $", out_of_state_tuition), weight = 1,  color=~pal(completions), radius = ~size)

hist(uni_map$out_of_state_tuition)
#changing the radius of the different circles

#working on the county map


