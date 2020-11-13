library(rvest)
library(dplyr)
library(polite)

url <- paste0("https://www.numbeo.com/cost-of-living/prices_by_country.jsp?displayCurrency=USD&itemId=118")

session <- polite::bow(url,user_agent = "Richs tester")

webpage <- scrape(session)


table1 <- webpage %>%
  html_nodes(xpath='//*[@id="t2"]') %>% 
  html_table(fill=T) %>% {.[[1]]}


cleaner <- function(vec) {
  vec %>% tolower() %>%
    #str_remove("eifelkreis|regionalverband|stadtverband") %>%
    return()
}


fuzzy_matches <- function(clean_vec,dirty_vec) {
  control <- data.frame(original=dirty_vec)
  
  distmatrix <- stringdist::stringdistmatrix(cleaner(clean_vec),cleaner(dirty_vec),method='jw',p=0.1)
  best_fit <- apply(distmatrix,2,which.min) %>% as.integer()
  similarity <- apply(distmatrix,2,min)
  
  control$best_fit <- clean_vec[best_fit]
  control$similarity <- similarity 
  
  return(control)
}

library(tmap)

data("World")

dict <- fuzzy_matches(World$name,table1$Country) %>%
  filter(similarity<0.18,original!="Malta") %>% 
  mutate(best_fit=as.character(best_fit)) %>%
  mutate(best_fit=ifelse(original=="South Korea" & best_fit=="South Africa","Korea",best_fit))



bananas <- table1 %>%
  mutate(country_new=plyr::mapvalues(Country,from=dict$original,to=dict$best_fit)) %>%
  rename(price=`Banana (1kg)`)

palette_yellow <- c("yellow","gold1","orange","darkorange3")

World %>%
  full_join(bananas,by=c("name"="country_new")) %>%
tm_shape() + 
  tm_polygons(col="price",
              breaks = c(0, 1.2, 1.8, 2.5,5),
              labels = c("Less than $1.20", "$1.20 to $1.80", "$1.80 to $2.50","More than $2.50"),
              palette = palette_yellow) +
  tm_layout(panel.labels="How much does 1kg of bananas cost?",
            panel.label.bg.color = "gold1")+
  tm_credits("data: www.numbeo.com",position = c(0,0))

             