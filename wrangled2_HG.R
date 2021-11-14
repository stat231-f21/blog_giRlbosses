library(tidyverse)
library(kableExtra)
library(robotstxt) 
library(rvest) 
library(purrr)
library(janitor)
library(maps)
library(fuzzyjoin)
library(ggplot2)
library(sf)
library(leaflet)
library(viridis)

stars_data <- "https://reports.aashe.org/institutions/participants-and-reports/?sort=rating"

# Check if bots are allowed
paths_allowed(stars_data)

# Grab table of schools
schools_table <- stars_data %>%
  read_html() %>%
  # Only one matching table so use singular `html_element()` (no "s")
  html_element("#institution_list") %>%
  html_table() %>% 
  # Clean up variable names
  janitor::clean_names() %>% 
  drop_na()


# Grab URL of each school's page 
url_href <- stars_data %>% 
  read_html() %>% 
  html_elements("#scorecard-list") %>% 
  html_attr("href") %>% 
  as.data.frame() %>%
  select(URL = ".") %>% 
  mutate(URL = paste("https://reports.aashe.org", URL, sep = ""))

schools_table <- schools_table %>% 
  cbind(url_href) 

## Iterate through to grab the latest report from each school page
n_links <- 553

# Pre-allocate space in dataframe for link
school_tibble <- schools_table[seq_len(n_links), ] %>% 
  mutate(latest_report = "") 

# Iterate through links 
for(i in seq_len(n_links)){
  
    school_tibble$latest_report[i] <- school_tibble$URL[i] %>%
      read_html() %>% 
      html_element(css = "#main > div > div > div > section > div > div > table > tbody > tr:nth-child(1) > td:nth-child(2) > a") %>% 
      html_attr("href") 
}

school_tibble <- school_tibble %>% 
  mutate(latest_report = paste("https://reports.aashe.org", latest_report, sep = ""))

write.table(school_tibble, "all_schools.txt", sep = "\t",
            row.names = FALSE)

##
schools <- all_schools[seq_len(nrow(all_schools)), ] %>% 
  mutate(purchasing = "", 
         dining = "") %>% 
  filter(latest_report != "https://reports.aashe.orgNA") %>% 
  # Remove expired 
  filter(!grepl("2014|2015|2016|2017|2018|2019|2020", valid_through))

#scrape
for(i in seq_len(n_links)){
  
  #purchasing URL
  schools$purchasing[i] <- schools$latest_report[i] %>%
    read_html() %>%
    html_elements(xpath = "/html/body/div/main/div/div/main/div/div/div
                    /section/div/div[2]/div[3]/div[4]/div[2]/div/div/div[4]
                    /div[2]/div/table/tbody/tr[1]/td[1]/a") %>%
    html_attr("href")
  
  schools$dining[i] <- schools2$latest_report[i] %>%
    read_html() %>%
    html_elements(xpath = "/html/body/div/main/div/div/main/div
                    /div/div/section/div/div[2]/div[3]/div[4]/div[2]
                    /div/div/div[4]/div[2]/div/table/tbody/tr[2]/td[1]/a") %>%
    html_attr("href")
  
}

## Now deal with rows where scraping report URL failed
schools_na <- schools %>% 
  filter(latest_report == "https://reports.aashe.orgNA") %>% 
  # Remove expired and reporter status
  filter(!grepl("2014|2015|2016|2017|2018|2019|2020", valid_through)) %>% 
  filter(rating != "Reporter") %>% 
  select(institution, URL, latest_report) %>% 
  mutate(purchasing = "", dining = "") 

nas <- nrow(schools_na)

for(i in seq_len(nas)){
  
    schools_na$purchasing[i] <- schools_na$URL[i] %>%
      read_html() %>% 
      html_element(xpath = "/html/body/div/main/div/div/main/div/div/div/
                   section/div/div[2]/div[3]/div[4]/div[2]/div/div/div[4]/
                   div[2]/div/table/tbody/tr[1]/td[1]/a") %>% 
      html_attr("href")
}

complete <- all %>% 
  left_join(select(schools, c(purchasing, institution, dining)), by = c("institution")) %>% 
  unite("purchasing", purchasing.x:purchasing.y, sep = "") %>% 
  left_join(select(schools_na, c(dining, institution)), by = "institution") %>% 
  unite("dining", dining.x:dining.y, sep = "") %>% 
  mutate(purchasing = gsub("NA", "", purchasing),
         dining = gsub("NA", "", dining)) %>% 
  mutate(across(c(purchasing, dining), ~paste("https://reports.aashe.org", ., sep = "")))

write.table(complete, "school_dining.txt", sep = "\t",
            row.names = FALSE)


## MAP

schools <- read.delim(file = "school_dining.txt", header=TRUE, sep="\t") 

colleges <- read_csv("sz-data-wrangling/data/ipeds_directory_info.csv") %>%
  janitor::clean_names() %>% 
  select(long = longitude_location_of_institution_hd2019,
         lat = latitude_location_of_institution_hd2019,
         institution = institution_name,
         state = state_abbreviation_hd2019,
         Type = control_of_institution_hd2019) %>% 
  mutate(Type = factor(Type, 
                       levels = c(1,2,3),
                       labels = c("Public", 
                                  "Private, Not-for-profit",
                                  "Private, For-profit")),
         institution = gsub("-", " ", institution)) 


# Make sure coordinate projection matches our data
# st_as_sf(coords = c("long", "lat"), 
#          crs = 4326, agr = "constant")

all <- schools %>% 
  mutate(institution = gsub("[[:punct:]]", "", institution)) 

#43 Canadian schools removed
summary(grepl("Canada", all$location))

college_map <- colleges %>% 
  regex_inner_join(all, by = "institution") %>% 
  select(institution = institution.x, state, long, lat, rating, URL, latest_report, purchasing, dining)

map_df <- college_map %>% 
  filter(!duplicated(URL)) %>% 
  filter(rating != "Reporter") %>% 
  filter(state != "AK") %>% 
  mutate(rating = ifelse(grepl("Platinum", rating), "Platinum",
                         ifelse(grepl("Gold", rating), "Gold",
                                ifelse(grepl("Silver", rating), "Silver", 
                                       ifelse(grepl("Bronze", rating), "Bronze", NA)))))

write.table(map_df, "map_schools.txt", sep = "\t",
            row.names = FALSE)

#my map

county_df <- map_data("county") 
county_map <- maps::map("county", regions = "massachusetts",
                    plot = FALSE, fill = TRUE) %>% 
  st_as_sf()

ggplot(data = county_map) +
  geom_sf(fill = "lightgrey", color = "white") +
  theme_void()

# Making map of U.S. with 

usa_df <- map_data("usa") 
county_df <- map_data("county")
# Run in console to see how data appear: View(county_df)

# Create sf object for Massachusetts counties
usa_map <- maps::map("state",
                     plot = FALSE, fill = TRUE) %>% 
  st_as_sf()

county_map <- maps::map("county",
                        plot = FALSE, fill = TRUE) %>% 
  st_as_sf()

ggplot(data = usa_map) +
  geom_sf(data = county_map, fill = "lightgrey", color = "white") +
  geom_sf(data = usa_map, fill=NA, color = "black")
theme_void() 

#chloropleths/shading regions based on the value of a variable
#Now I am going to want to do my wrangling for my dataset below and figure out what kind of coloring I want

#Then I'll join to make final dataset

##Then I will make a state info dataset
# Peek at data
head(college_plan_summary)
head(usa_map)

# State objects should appear in your Environment pane
data(state) 

# Create data frame with state info
state_info <- data.frame(Region = state.region,
                         # Match state variable name in map data
                         ID = tolower(state.name), 
                         # Match state variable name in summary data
                         State = state.abb)
head(state_info)

# Join datasets from the left starting with the sf object ()
college_plans_map <- usa_map %>% 
  left_join(state_info) %>% 
  left_join(college_plan_summary)

head(college_plans_map)

#Now I want to plot a point for each college and color points by how sustainable they are

colleges <- read_csv("data/ipeds_directory_info.csv") %>%
  janitor::clean_names() %>% 
  select(long = longitude_location_of_institution_hd2019,
         lat = latitude_location_of_institution_hd2019,
         Institution = institution_name,
         Type = control_of_institution_hd2019) %>% 
  mutate(Type = factor(Type, 
                       levels = c(1,2,3),
                       labels = c("Public", 
                                  "Private, Not-for-profit",
                                  "Private, For-profit"))) %>% 
  # Make sure coordinate projection matches our data
  st_as_sf(coords = c("long", "lat"), 
           crs = 4326, agr = "constant")

colleges_map <- colleges %>% 
  right_join(college_plans) %>% 
  # State map is only of contiguous US (sorry, Alaska and Hawaii!!)
  filter(!(State %in% c("AK", "HI")))

ggplot(college_plans_map) +
  geom_sf(aes(fill = proportion_in_person)) +
  scale_fill_viridis(option = "magma", direction = -1) +
  geom_sf(data = colleges_map, aes(color = plans)) + 
  theme_void() +
  labs(fill = "Proportion",
       color = "Plans",
       title = "Proportion of colleges planning for in-person learning for Fall 2020, by state",
       subtitle = "as of July 2020") +
  theme(legend.position = "bottom")


