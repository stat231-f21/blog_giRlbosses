#Data wrangling for top 30 sustainable colleges data
library(tidyverse)
library(robotstxt) 
library(rvest) 
library(purrr) 
library(readxl)
#url for dataset
Top30_url <- "https://www.epa.gov/system/files/documents/2021-10/top30candu_july2021.pdf"
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


  
  


