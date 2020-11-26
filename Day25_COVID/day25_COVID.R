#### COVID cases Germany

library(sf)
library(ggplot2)
library(dplyr)
library(stringr)
library(stringdist) # For Fuzzy merging
library(TTR) # Moving Averages/Sums

# Complete data from
# https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0?selectedAttribute=Refdatum


  
dates <- seq.Date(from=as.Date("2020-02-01"),to=as.Date("2020-11-25"),by="day")

kreise <- read_sf("C:/Richard/R and Python/Datasets/Kreisgrenzen_2017_mit_Einwohnerzahl/Kreisgrenzen_2017_mit_Einwohnerzahl.shp") %>%
  mutate(name=paste(ifelse(BEZ %in% c("Kreisfreie Stadt","Stadtkreis"),"SK","LK"),GEN)) %>%
  st_simplify(dTolerance=2000)

kreise_df <- st_drop_geometry(kreise)


df <- expand.grid(
  date=dates,name=kreise$name
) %>%
  inner_join(select(kreise_df,name,EWZ),by="name")

cases <- read.csv("C:/Richard/R and Python/Datasets/RKI_COVID19.csv") %>% 
  group_by(Landkreis,Meldedatum) %>%
  summarise(cases=sum(AnzahlFall)) %>%
  ungroup()

cleaner <- function(vec) {
  vec %>% tolower() %>%
    str_remove("eifelkreis|regionalverband|stadtverband") %>%
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

dict <- fuzzy_matches(kreise_df$name,unique(cases$Landkreis))


final <- cases %>%
  #fuzzy merging
  mutate(LK_new=plyr::mapvalues(Landkreis,from=dict$original,to=dict$best_fit)) %>% 
  group_by(LK_new,Meldedatum) %>%
  summarise(cases=sum(cases)) %>% 
  ungroup() %>%
  transmute(LK_new,date=as.Date(substr(Meldedatum,1,10)),cases) %>%
  #Fill all dates (even if there were no cases reported)
  right_join(df,by=c("LK_new"="name","date")) %>%
  mutate(cases=ifelse(is.na(cases),0,cases)) %>% 
  arrange(LK_new,date) %>%
  #Moving average 7 days
  group_by(LK_new) %>%
  mutate(avg_cases_7_day = runSum(cases, 7)) %>%
  ungroup() %>%
  mutate(incidence_per_100k = 100000*avg_cases_7_day/EWZ) %>%
  mutate(month=format(date,"%B")) %>%
  #Get the geometries from kreise
  left_join(select(kreise,name),by=c("LK_new"="name")) %>%
  mutate(cases7_per_bin=cut(incidence_per_100k,breaks=c(-1,4,25,50,100,250,1000),
                            labels=c("<5","5 to 25","25 to 50",
                                     "50 to 100","100 to 250","over 250"))) %>%
  st_as_sf(sf_column_name="geometry")
  
  

sel_dates <- seq.Date(from=as.Date("2020-02-25"),to=as.Date("2020-11-25"),by="month")

Sys.setlocale(locale = "English")

final %>%
  filter(date %in% sel_dates) %>%
  ggplot(aes(fill=cases7_per_bin))+geom_sf()+
  labs(title = 'COVID in Germany',
       fill = "7 day incidence values",
       caption = "Data from: NPGEO Corona, Hub RKI") +
  scale_fill_manual(values=c("grey60",
                             "grey40","gold2","orangered1","red3","red4"),drop=FALSE)+
  facet_wrap(~reorder(month,date))+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size=25,family="Crimson Text",colour="midnightblue"),
        plot.caption = element_text(size=10,colour="midnightblue"),
        legend.position=c(0.7,0.18))

#800x800

sel_dates <- seq.Date(from=as.Date("2020-02-15"),to=as.Date("2020-11-25"),by="day")
img_frames <- paste0("covid", seq_along(sel_dates), ".png")

for (i in seq_along(sel_dates)) {
  message(paste(" - image", i, "of", length(sel_dates)))
  map <- final %>%
    filter(date %in% sel_dates[i]) %>%
    ggplot(aes(fill=cases7_per_bin))+geom_sf()+
    labs(title = 'COVID in Germany',
         subtitle = format(sel_dates[i],"%B"),
         fill = "7 day incidence values",
         caption = "Data from: NPGEO Corona, Hub RKI") +
    scale_fill_manual(values=c("grey80","grey60",
                               "grey40","gold2","orangered1","red3","red4"),drop=FALSE)+
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(size=20))
  png(filename=paste0("covid",i,".png"),width = 800,height = 800)
  print(map)
  dev.off()
}
?magick::image_write_gif
# build gif
magick::image_write_gif(magick::image_read(img_frames),
                        path = "covid.gif",
                        delay = 1/10)
# build gif