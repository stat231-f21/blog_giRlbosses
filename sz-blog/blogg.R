library(tidyverse)
library(kableExtra)
library(robotstxt) 
library(rvest) 
library(purrr)
library(janitor)
library(DataCombine)

dfr <- read.delim(file = "/Users/sara/Desktop/STAT231/stat231-content/blog/data/map_df.txt", header=TRUE, sep="\t") 
schools <- read.delim(file = "/Users/sara/Desktop/STAT231/stat231-content/blog/data/school_dining.txt", header=TRUE, sep="\t") 

dfl <- schools %>% 
  mutate(latest = gsub("OP/food-dining/OP-8/", "", dining),
         curriculum)

df <- dfr

names(tb)[names(tb) == 'X1'] <- "institution"

td <- data.frame(matrix(nrow = 0, ncol = 16))

x = paste("df", y, sep = "")
y = 263

for (i in 271:279){

  y = y + 1
  x <- data.frame(a = 1:4, b = 1:4, c = 1:4, d = 1:4)
  paste(df$institution[i])
  
    for(k in 2:5){
      
      x[ , k-1] <- df$latest_report[i] %>%
        read_html() %>%
        html_elements(xpath = paste("/html/body/div/main/div/div/main/div/div/div/section
                                      /div/div[2]/div[3]/div[", k, 
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

# 73 Delta College, 85 Eastern Mennonite University, 93 Florida International University
# 121 Johnson County Community College, 150 North Seattle College, 171 Rice University
# 189 St Lawrence University, 203 University of Texas Rio Grande Valley, 254 University of Notre Dame
# 275 Washington University in St Louis, 280 West Chester University of Pennsylvania,
# 285 Western University of Health Sciences, 287 Whatcom Community College
# 230 University of Iowa, 272 Wake Forest University, 279 Wells College

# 73, 85, 93, 121, 150, 171, 189, 203, 230, 254, 264, 275, 280, 285, 287
td3 <- td2 %>% 
  select(-c, -d, -e, -f) %>% 
  relocate(c, .before = a)

w <- c(74, 85, 93, 121, 150, 171, 189, 203, 230, 254, 272, 275, 279, 280, 285, 287)

for (i in 1:16){
  td3 <- td3 %>% 
    add_row(a = 0, b = 0, g = 0, h = 0, i = 0, j = 0, k = 0, l = 0, m = 0, 
            n = 0, o = 0, p = 0,
            .before = w[i])
}

school_scores <- dfb %>% 
  cbind(td3) %>% 
  select(institution, state, rating, latest_report,
         curriculum = a, research = b, campus_engagement = g, 
         public_engagement = h, air = i, buildings = j, energy = k, food = l, 
         planning = m, diversity = n, investment = o, wellbeing = p) %>% 
  cbind(tz)

write.table(school_scores, "school_scores.txt", sep = "\t",
            row.names = FALSE)

write.table(df, "reporting.txt", sep = "\t",
            row.names = FALSE)

#9

tz <- data.frame(matrix(nrow = 0, ncol = 5))
names(tz) <- c("grounds", "purchasing", "transportation", "waste", "water")

z = paste("df", y, sep = "")
y = 8

for(i in 10:275){
    
    y = y + 1
    a <- c(1:5)
    z <- data.frame(a)
    
      for(k in 5:9){
        
        z[k-4, ] <- df$latest_report[i] %>%
          read_html() %>%
          html_elements(xpath = paste("/html/body/div/main/div/div/main/div/div/div/section/div/div[2]/div[3]/
                                      div[4]/div[2]/div/div/div[", k, "]/div[1]/a/div", sep = "")) %>%
          
          html_text() %>% 
          as.data.frame() %>% 
          select(score = ".") %>% 
          mutate(score = as.numeric(regmatches(score, regexpr("(?>-)*[[:digit:]]+\\.*[[:digit:]]*", 
                                                              score, perl=TRUE))))
        
      }   

    names(z) <- c("a")
    
    z <- z %>% 
      pivot_wider(names_from = a, values_from = a)
    
    if(ncol(z) == 5){
      names(z) <- c("grounds", "purchasing", "transportation", "waste", "water")
      tz <- tz %>% 
        rbind(z)
    }
    else{
      return("NA")
    }
    
    
}



### 11/12
all_schools <- read.delim(file = "/Users/sara/Desktop/STAT231/stat231-content/blog/data/all_schools.txt", header=TRUE, sep="\t") 
mapped <- read.delim(file = "/Users/sara/Desktop/STAT231/stat231-content/map_df.txt", header=TRUE, sep="\t")
scraped <- read.delim(file = "/Users/sara/Desktop/STAT231/stat231-content/scrapable.txt", header=TRUE, sep="\t")

all <- all_schools %>% 
  select(URL, stars_version, institution_rename = institution)

rschools <- mapped %>% 
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
         dining = paste(latest_report, "OP/food-dining/OP-8/", sep = "")
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
         dining = paste(latest_report, "OP/food-dining/OP-8/", sep = "")
  )

master_schools <- rschools %>% 
  left_join(select(v1, c(institution, latest1 = latest_report)), by = "institution") %>% 
  mutate(latest_report = ifelse(is.na(latest1), latest_report, latest1)) %>% 
  left_join(select(v2, c(institution, latest2 = latest_report)), by = "institution") %>% 
  mutate(latest_report = ifelse(is.na(latest2), latest_report, latest2)) %>% 
  select(institution, stars_version, state, long, lat, URL, latest_report)

vars <- c(curriculum, learning, research, purchasing, dining) 

links <- master %>% 
  select(institution, latest_report) %>% 
  left_join(select(v1, c(institution, air1, wasted1, p1, ic1, water1)), by = "institution") %>% 
  left_join(select(v2, c(institution, air2, wasted2, p2, ic2, water2)), by = "institution") %>% 
  unite(water, c(water1, water2), sep = "") %>% 
  unite(air, c(air1, air2), sep = "") %>%
  unite(wasted, c(wasted1, wasted2), sep = "") %>% 
  unite(p, c(p1, p2), sep = "") %>% 
  unite(ic, c(ic1, ic2), sep = "") %>% 
  dplyr::mutate(across(air:water, ~ gsub("NA", "", .)))

links <- links %>% 
  mutate(curriculum = paste(latest_report, "AC/curriculum/AC-1/", sep = ""),
         research = paste(latest_report, "AC/research/AC-9/", sep = ""),
         energy = paste(latest_report, "OP/energy/OP-6/", sep = ""),
         learning = paste(latest_report, "AC/curriculum/AC-2/", sep = ""),
         purchasing = paste(latest_report, "OP/food-dining/OP-7/", sep = ""),
         dining = paste(latest_report, "OP/food-dining/OP-8/", sep = ""))


write.table(df, "scraped.txt", sep = "\t",
            row.names = FALSE)
write.table(v1, "v1.txt", sep = "\t",
            row.names = FALSE)
write.table(v2, "v2.txt", sep = "\t",
            row.names = FALSE)
write.table(master_schools, "master_df.txt", sep = "\t",
            row.names = FALSE)
write.table(links, "page_links.txt", sep = "\t",
            row.names = FALSE)

ve1 <- v1 %>% 
  select(institution)

ve2 <- v2 %>% 
  select(institution)

sc <- scraped %>% 
  select(institution)

l <- nrow(v1)
m <- nrow(v2)
n <- nrow(df)

df <- sc

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

cols <- c(5, 5, 5, 5, 4, 8, 9, 10, 7, 4, 4, 12)

for(i in seq_len(n)){
  #ERROR HANDLING
  skip_to_next <- FALSE
  
  tryCatch(
    for(k in 1:12){ 
      df[i, k+1] <- links[cols[k], i] %>%
        read_html() %>%
        html_element(xpath = elements[k]) %>%
        html_text()}, error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }     
}

d2 <- ve2 %>% 
  select(institution)

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

names(df)[1:13] <- c("endowment", "area", "locale", "climate", "size", "classes", "renewables", "water", "waste", "recycling")
names(ve1)[2:6] <- c("renewables1", "gge1", "plant_based", "budget", "low1")
names(ve2)[2:5] <- c("renewables2", "gge2", "low2", "monday")

df1 <- df %>% 
  left_join(select(ve1, c(institution, gge1, low1 = low, monday)), by = "institution") %>% 
  left_join(select(ve2, c(institution, renewables2 = renewables, gge2, low2 = low, plant_based, budget)), by = "institution") %>% 
  unite(renewables, c(renewables, renewables2), sep = "") %>% 
  unite(gge, c(gge1, gge2), sep = "") %>% 
  unite(low, c(low1, low2), sep = "") %>%
  dplyr::mutate(across(gge:budget, ~ gsub("NA", "", .)))


##
rm(d)
d <- df1 %>% 
  filter(!grepl("Acres", area)) %>% 
  mutate(gge = gsub("CO2", "", gge)) %>% 
  relocate(locale, .after = gge)

d[] <- lapply(d, function(x) gsub(",", "", x))
d[2:12] <- lapply(d[2:12], function(x) as.numeric(regmatches(x, gregexpr("[[:digit:]]+\\.*[[:digit:]]*", x))))
d[c("waste", "water")] <- lapply(d[c("waste", "water")], function(x) round(x/(d$size), digits = 3))
d <- d %>% 
  filter(institution != "The New School") %>% 
  mutate(area = round(area*2.47))

rm(df2)
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

rm(df3)
df3 <- df3 %>% 
  merge(select(d, c(institution, area, waste, water)), by = "institution", all = TRUE, no.dups = TRUE) %>% 
  unite(area, c(area.x, area.y), sep = "") %>% 
  unite(water, c(water.x, water.y), sep = "") %>%
  unite(waste, c(waste.x, waste.y), sep = "") 

df3[c("waste", "water", "area")] <- lapply(df3[c("waste", "water", "area")], function(x) gsub("NA", "", x))


df2[2:14] <- lapply(df2[2:14], function(x) as.numeric(regmatches(x, gregexpr("[[:digit:]]+\\.*[[:digit:]]*", x))))

##
report_card <- read.delim(file = "/Users/sara/Desktop/STAT231/stat231-content/school_scores.txt", header=TRUE, sep="\t") 

map <- map %>% 
  filter(!grepl("Delta College|Eastern Mennonite University|Florida International University|Johnson County Community College|North Seattle College|Rice University|St Lawrence University|The University of Texas Rio Grande Valley|University of Notre Dame|Washington University in St Louis|West Chester University of Pennsylvania|Western University of Health Sciences|Whatcom Community College|University of Iowa|Wake Forest University|Wells College", institution))

rc <- report_card %>% 
  filter(!grepl("Delta College|Eastern Mennonite University|Florida International University|Johnson County Community College|North Seattle College|Rice University|St Lawrence University|The University of Texas Rio Grande Valley|University of Notre Dame|Washington University in St Louis|West Chester University of Pennsylvania|Western University of Health Sciences|Whatcom Community College|University of Iowa|Wake Forest University|Wells College", institution))

report <- rc[-c(1:4)]

rm(df)
df <- clean_answers %>% 
  mutate(total = "") 

for(i in 1:275){
  #ERROR HANDLING
  skip_to_next <- FALSE
  #
  tryCatch(p <- links$latest_report[i] %>%
             read_html() %>%
             html_element(xpath = "/html/body/div/main/div/div/main/div/div/div/section/div/div[2]/table") %>%
             html_table(), error = function(e) { skip_to_next <<- TRUE})
  df$total[i] <- p[2]
  if(skip_to_next) { next }     
}

df <- df %>% 
  cbind(report) %>% 
  cbind(map[2:5])

names(df)[21] <- c("researching")

df <- df %>% 
  select(-total) %>% 
  left_join(select(df2, c(institution, total)), by = "institution") %>%
  mutate(total = as.numeric(total))

write.csv(df, "master.csv", row.names = FALSE)








