knitr::opts_chunk$set(echo = TRUE)


library(sf) # For shapefiles
library(readxl) # Read Excel file
library(ggplot2) # Visualization
library(dplyr) # Data manipulation
library(stringr) # String manipulation
library(stringdist) # String distances

theme_set(theme_void())

cases <- read_xlsx("C:/Richard/R and Python/Datasets/Fallzahlen_Kreise_04_11_2020.xlsx",sheet=5,skip=4)

knitr::kable(head(cases))

cases %>% filter(str_detect(LK,"Aschaff|Würzb")) %>% knitr::kable()

kreise <- read_sf("C:/Richard/R and Python/Datasets/Kreisgrenzen_2017_mit_Einwohnerzahl/Kreisgrenzen_2017_mit_Einwohnerzahl.shp")

kreise %>% st_drop_geometry() %>% select(GEN,BEZ) %>% head() %>% knitr::kable()

kreise <- kreise %>%
mutate(name=paste(ifelse(BEZ %in% c("Kreisfreie Stadt","Stadtkreis"),"SK","LK"),GEN))

kreise %>% st_drop_geometry() %>% select(name) %>% head() %>% knitr::kable()

cases %>% inner_join(kreise,by=c("LK"="name")) %>%
  ggplot() + geom_sf(aes(fill=Inzidenz,geometry=geometry))

cases %>% anti_join(kreise,by=c("LK"="name")) %>% head() %>% knitr::kable()

kreise %>% st_drop_geometry() %>%
  filter(str_detect(name,"Berlin|Offenbach|Mülheim|Lindau")) %>% 
  select(name) %>%
  knitr::kable()


cleaner <- function(vec) {
  vec %>% tolower() %>%
    str_remove("eifelkreis|regionalverband|stadtverband|saale") %>%
    return()
}


  
fuzzy_matches <- function(clean_vec,dirty_vec) {
  control <- data.frame(original=dirty_vec)
  
  distmatrix <- stringdist::stringdistmatrix(cleaner(clean_vec),cleaner(dirty_vec),method='jw',p=0.1)
  best_fit <- apply(distmatrix,2,which.min) %>% as.integer()
  similarity <- apply(distmatrix,2,min)
  
  control$best_fit <- clean_vec[best_fit]
  control$similarity <- similarity 
  
  return(control %>% arrange(desc(similarity)))
}

dict <- fuzzy_matches(kreise$name,cases$LK)

dict %>% filter(similarity>0) %>%
  slice(seq(1,39,2)) %>%
  knitr::kable()


cases %>%
  mutate(LK_new=plyr::mapvalues(LK,from=dict$original,to=dict$best_fit)) %>% 
  left_join(kreise,by=c("LK_new"="name")) %>%
  ggplot(aes(fill=Inzidenz))+geom_sf(aes(geometry=geometry))

