#Data wrangling for top 30 sustainable colleges data
library(tidyverse)
library(robotstxt) 
library(rvest) 
library(purrr) 
library(readxl)
library(readr)
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




