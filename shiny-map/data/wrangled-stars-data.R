library(tidyverse)
library(kableExtra)
library(robotstxt) 
library(rvest) 
library(purrr)
library(janitor)
library(maps)
library(fuzzyjoin)
library(DataCombine)

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

write.table(complete, "complete_schools.txt", sep = "\t", row.names = FALSE)

## MAP

schools <- read.delim(file = "complete_schools.txt", header=TRUE, sep="\t") 

colleges <- read_csv("ipeds_directory_info.csv") %>%
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

write.table(map_df, "map_schools.txt", sep = "\t", row.names = FALSE)

### scrape report scores

complete <- read.delim(file = "/Users/sara/Desktop/STAT231/stat231-content/blog/data/map_df.txt", header=TRUE, sep="\t") 

df <- complete %>% 
  mutate(latest = gsub("OP/food-dining/OP-8/", "", dining), curriculum)

names(tb)[names(tb) == 'X1'] <- "institution"

td <- data.frame(matrix(nrow = 0, ncol = 16))
y = 0

for (i in 271:279){
  
  y = y + 1
  x <- data.frame(a = 1:4, b = 1:4, c = 1:4, d = 1:4)
  paste(df$institution[i])
  
  for(k in 2:5){
    
    x[ , k-1] <- df$latest_report[i] %>%
      read_html() %>%
      html_elements(xpath = paste("/html/body/div/main/div/div/main/div/div/div/section/div/div[2]/div[3]/div[", k, 
                                  "]/div[2]/div/div/div/div[1]/a/div", sep = "")) %>%
      
      html_text() %>% 
      as.data.frame() %>% 
      select(score = ".") %>% 
      mutate(score = as.numeric(regmatches(score, regexpr("(?>-)*[[:digit:]]+\\.*[[:digit:]]*", 
                                                          score, perl=TRUE))))
    
  }   
  
  x <- x %>% 
    pivot_wider(names_from = a:d, values_from = a:d)
  
  names(x) <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p")
  names(td) <- names(x)
  
  td <- td %>% 
    rbind(x) 
  
  td[i, 3] <- y
  
}

td <- td %>% 
  select(-c, -d, -e, -f) %>% 
  relocate(c, .before = a)

school_scores <- td %>% 
  select(institution, state, rating, latest_report,
         curriculum = a, research = b, campus_engagement = g, 
         public_engagement = h, air = i, buildings = j, energy = k, food = l, 
         planning = m, diversity = n, investment = o, wellbeing = p)

write.table(school_scores, "school_scores.txt", sep = "\t", row.names = FALSE)

###

all <- all_schools %>% 
  select(URL, stars_version, institution_rename = institution)

rschools <- map_df %>% 
  left_join(all, by = "URL") %>% 
  left_join(select(dfl, c(latest, institution)), by = "institution") %>% 
  mutate(institution = institution_rename) %>% 
  mutate(latest_report = ifelse(!grepl("20", latest_report), latest, latest_report)) %>% 
  select(-latest, -institution_rename) 

v1 <- rschools %>% 
  select(institution, stars_version, latest_report, purchasing) %>% 
  filter(stars_version == "2.1") %>% 
  mutate(purchasing = gsub("OP.*", "", purchasing),
         latest_report = ifelse(is.na(latest_report), purchasing, latest_report),
         air1 = paste(latest_report, "OP/air-climate/OP-1/", sep = ""),
         wasted1 = paste(latest_report, "OP/waste/OP-19/", sep = ""),
         ic1 = paste(latest_report, "IC/institutional-characteristics/IC-2/", sep = ""),
         p1 = paste(latest_report, "IC/institutional-characteristics/IC-3/", sep = ""),
         water1 = paste(latest_report, "OP/water/OP-22/", sep = ""),
         purchasing = paste(latest_report, "OP/food-dining/OP-7/", sep = ""),
         dining = paste(latest_report, "OP/food-dining/OP-8/", sep = ""),
         energy1 = paste(latest_report, "OP/energy/OP-6/", sep = "")
  )

v2 <- rschools %>% 
  select(institution, stars_version, latest_report, purchasing) %>% 
  filter(stars_version == "2.2") %>% 
  mutate(purchasing = gsub("OP.*", "", purchasing),
         latest_report = ifelse(is.na(latest_report), purchasing, latest_report),
         air2 = paste(latest_report, "OP/air-climate/OP-2/", sep = ""),
         wasted2 = paste(latest_report, "OP/waste/OP-18/", sep = ""),
         ic2 = paste(latest_report, "PRE/institutional-characteristics/PRE-4/", sep = ""),
         p2 = paste(latest_report, "PRE/institutional-characteristics/PRE-5/", sep = ""),
         water2 = paste(latest_report, "OP/water/OP-21/", sep = ""),
         purchasing = paste(latest_report, "OP/food-dining/OP-7/", sep = ""),
         dining = paste(latest_report, "OP/food-dining/OP-8/", sep = ""),
         energy2 = paste(latest_report, "OP/energy/OP-6/", sep = "")
  )

master_schools <- rschools %>% 
  left_join(select(v1, c(institution, latest1 = latest_report)), by = "institution") %>% 
  mutate(latest_report = ifelse(is.na(latest1), latest_report, latest1)) %>% 
  left_join(select(v2, c(institution, latest2 = latest_report)), by = "institution") %>% 
  mutate(latest_report = ifelse(is.na(latest2), latest_report, latest2)) %>% 
  select(institution, stars_version, state, long, lat, URL, latest_report)

links <- master %>% 
  select(institution, latest_report) %>% 
  left_join(select(v1, c(institution, air1, wasted1, p1, ic1, water1)), by = "institution") %>% 
  left_join(select(v2, c(institution, air2, wasted2, p2, ic2, water2)), by = "institution") %>% 
  unite(water, c(water1, water2), sep = "") %>% 
  unite(air, c(air1, air2), sep = "") %>%
  unite(wasted, c(wasted1, wasted2), sep = "") %>% 
  unite(p, c(p1, p2), sep = "") %>% 
  unite(ic, c(ic1, ic2), sep = "") %>% 
  dplyr::mutate(across(air:water, ~ gsub("NA", "", .))) %>% 
  right_join(select(scraped, c(latest_report)), by = "latest_report") %>% 
  mutate(curriculum = paste(latest_report, "AC/curriculum/AC-1/", sep = ""),
         research = paste(latest_report, "AC/research/AC-9/", sep = ""),
         energy = paste(latest_report, "OP/energy/OP-6/", sep = ""),
         purchasing = paste(latest_report, "OP/food-dining/OP-7/", sep = ""),
         dining = paste(latest_report, "OP/food-dining/OP-8/", sep = ""))

###

d1 <- v1 %>% 
  select(institution)

d2 <- v2 %>% 
  select(institution)

df <- scraped %>% 
  select(institution)

l <- nrow(v1)
m <- nrow(v2)
n <- nrow(df)

##

elements <- c("/html/body/div/main/div/div/main/div/div/div/section/div/div[2]/div/div[2]/text()", 
              "/html/body/div/main/div/div/main/div/div/div/section/div/div[2]/div/div[3]", 
              "/html/body/div/main/div/div/main/div/div/div/section/div/div[2]/div/div[4]/text()",
              "/html/body/div/main/div/div/main/div/div/div/section/div/div[2]/div/div[5]/text()",
              "/html/body/div/main/div/div/main/div/div/div/section/div/div[2]/div/div[12]/text()",
              "/html/body/div/main/div/div/main/div/div/div/section/div/div[2]/div/div[2]/text()",
              "/html/body/div/main/div/div/main/div/div/div/section/div/div[2]/div/div[4]/text()",
              "/html/body/div/main/div/div/main/div/div/div/section/div/div[2]/div/div[15]",
              "/html/body/div/main/div/div/main/div/div/div/section/div/div[2]/div/table[5]/tbody/tr/td[2]",
              "/html/body/div/main/div/div/main/div/div/div/section/div/div[2]/div/table[4]/tbody/tr/td[2]",
              "/html/body/div/main/div/div/main/div/div/div/section/div/div[2]/div/div[5]/text()",
              "/html/body/div/main/div/div/main/div/div/div/section/div/div[2]/div/div[2]/text()")

df_cols <- c(5, 5, 5, 5, 4, 8, 9, 10, 7, 4, 4, 12)

for(i in seq_len(n)){
  #ERROR HANDLING
  skip_to_next <- FALSE
  
  tryCatch(
    for(k in 1:12){ 
      df[i, k+1] <- links[df_cols[k], i] %>%
        read_html() %>%
        html_element(xpath = elements[k]) %>%
        html_text()}, error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
}

##version 2
v2_elements <- c("/html/body/div/main/div/div/main/div/div/div/section/div/div[2]/div/div[9]/text()",
                 "/html/body/div/main/div/div/main/div/div/div/section/div/div[2]/div/table[6]/tbody/tr/td[2]",
                 "/html/body/div/main/div/div/main/div/div/div/section/div/div[2]/div/div[3]/text()",
                 "/html/body/div/main/div/div/main/div/div/div/section/div/div[2]/div/div[7]/text()",
                 "/html/body/div/main/div/div/main/div/div/div/section/div/div[2]/div/div[10]/div/p")

v2_cols <- c(11, 5, 4, 4, 10)

for(i in seq_len(m)){
  #ERROR HANDLING
  skip_to_next <- FALSE
  
  tryCatch(
    for(k in 1:5){ 
      ve2[i, k+1] <- links[v2_cols[k], i] %>%
        read_html() %>%
        html_element(xpath = v2_elements[k]) %>%
        html_text()}, error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
}

##version 1
v1_elements <- c("/html/body/div/main/div/div/main/div/div/div/section/div/div[2]/div/div[15]",
                 "/html/body/div/main/div/div/main/div/div/div/section/div/div[2]/div/table[8]/tbody/tr/td[2]/text()",
                 "/html/body/div/main/div/div/main/div/div/div/section/div/div[2]/div/div[11]/div/p",
                 "/html/body/div/main/div/div/main/div/div/div/section/div/div[2]/div/div[10]/text()")

v1_cols <- c(11, 5, 10, 4)

for(i in seq_len(m)){
  #ERROR HANDLING
  skip_to_next <- FALSE
  
  tryCatch(
    for(k in 1:4){ 
      ve1[i, k+1] <- links[v1_cols[k], i] %>%
        read_html() %>%
        html_element(xpath = v1_elements[k]) %>%
        html_text()}, error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
}

#rename columns
names(df)[1:13] <- c("endowment", "area", "locale", "climate", "size", "classes", "renewables", "water", "waste", "recycling")
names(v1)[2:6] <- c("renewables1", "gge1", "plant_based", "budget", "low1")
names(v2)[2:5] <- c("renewables2", "gge2", "low2", "monday")

###

df <- df %>% 
  left_join(select(ve1, c(institution, gge1, low1 = low, monday)), by = "institution") %>% 
  left_join(select(ve2, c(institution, renewables2 = renewables, gge2, low2 = low, plant_based, budget)), by = "institution") %>% 
  unite(renewables, c(renewables, renewables2), sep = "") %>% 
  unite(gge, c(gge1, gge2), sep = "") %>% 
  unite(low, c(low1, low2), sep = "") %>%
  dplyr::mutate(across(gge:budget, ~ gsub("NA", "", .)))

##

d <- df %>% 
  filter(!grepl("Acres", area)) %>% 
  mutate(gge = gsub("CO2", "", gge)) %>% 
  relocate(locale, .after = gge)

d[] <- lapply(d, function(x) gsub(",", "", x))
d[2:12] <- lapply(d[2:12], function(x) as.numeric(regmatches(x, gregexpr("[[:digit:]]+\\.*[[:digit:]]*", x))))
d[c("waste", "water")] <- lapply(d[c("waste", "water")], function(x) round(x/(d$size), digits = 3))
d <- d %>% 
  filter(institution != "The New School") %>% 
  mutate(area = round(area*2.47))

##

df2 <- df1 %>% 
  mutate(gge = gsub("CO2", "", gge)) %>% 
  relocate(locale, .after = gge) %>% 
  relocate(plant_based, .after = gge) %>% 
  mutate(area = ifelse(grepl("Acres", area), area, NA),
         waste = ifelse(grepl("Acres", area), waste, NA),
         water = ifelse(grepl("Acres", area), water, NA))

df2[] <- lapply(df2, function(x) gsub(",", "", x))
df2[2:14] <- lapply(df2[2:14], function(x) as.numeric(regmatches(x, gregexpr("[[:digit:]]+\\.*[[:digit:]]*", x))))
df2[c("locale", "monday")] <- lapply(df2[c("locale", "monday")], function(x) stringr::str_squish(x))

## unite
df3 <- df2 %>% 
  merge(select(d, c(institution, area, waste, water)), by = "institution", all = TRUE, no.dups = TRUE) %>% 
  unite(area, c(area.x, area.y), sep = "") %>% 
  unite(water, c(water.x, water.y), sep = "") %>%
  unite(waste, c(waste.x, waste.y), sep = "") 

df3[c("waste", "water", "area")] <- lapply(df3[c("waste", "water", "area")], function(x) gsub("NA", "", x))

df <- df3 %>% 
  filter(!grepl("Delta College|Eastern Mennonite University|Florida International University|Johnson County Community College|North Seattle College|Rice University|St Lawrence University|The University of Texas Rio Grande Valley|University of Notre Dame|Washington University in St Louis|West Chester University of Pennsylvania|Western University of Health Sciences|Whatcom Community College|University of Iowa|Wake Forest University|Wells College", institution))

##bind with report card scores
report <- school_scores[-c(1:4)] %>% 
  filter(!grepl("Delta College|Eastern Mennonite University|Florida International University|Johnson County Community College|North Seattle College|Rice University|St Lawrence University|The University of Texas Rio Grande Valley|University of Notre Dame|Washington University in St Louis|West Chester University of Pennsylvania|Western University of Health Sciences|Whatcom Community College|University of Iowa|Wake Forest University|Wells College", institution))

master <- df %>% 
  cbind(report) %>% 
  cbind(map[2:5]) 

#Join data from US News' list of Liberal Arts Colleges to allow users to filter for LACs (institutions with simalar charecteristics to Amherst)
news <- read.delim("/Users/sara/Desktop/STAT231/stat231-content/us_news.txt") 

map <- map %>% 
  left_join(select(news, institution= College.Name, IPEDS.ID), by= "institution")

write.csv(df, "map.csv", row.names = FALSE)
