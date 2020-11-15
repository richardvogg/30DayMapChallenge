library(raster)
library(dplyr)
library(ggplot2)
library(tmap)
library(sf)

#data from ftp://ftp.cdc.noaa.gov/Datasets/cpc_global_precip


#Long term mean
ltm <- brick("C:/Richard/R and Python/Environmental Data Science/GIS/ChileClimate/Data_raw/precip.day.1981-2010.ltm.nc") %>%
  rotate()

#Selected countries
data("World")
countries <- World %>%
  filter(continent%in%"South America") %>%
  st_transform(crs(ltm)@projargs)

crs(ltm) <- crs(countries)

summarise_precip <- function(df) {
  out <- df %>% 
    mask(countries) %>% 
    rasterToPoints() %>% 
    data.frame() %>% 
    tidyr::pivot_longer(cols=-c(x,y),names_to="date",values_to="value") %>% 
    group_by(x,y) %>% 
    summarise(precip=sum(value)) %>% 
    ungroup()
    
  return(out)
}

ltm_df <- summarise_precip(ltm)
rm(ltm)

for(year in 2011:2019) {
  dat <- brick(paste0("C:/Richard/R and Python/Environmental Data Science/GIS/ChileClimate/Data_raw/precip.",year,".nc")) %>%
    rotate()
  
  crs(dat) <- crs(countries)
  
  assign(paste0("df",year),summarise_precip(dat))
}


mean_10y <- rbind(df2010,df2011,df2012,df2013,df2014,df2015,df2016,df2017,df2018,df2019) %>%
  group_by(x,y) %>%
  summarise(prec2010_19=mean(precip))

final <- mean_10y %>%
  inner_join(ltm_df,by=c("x","y")) %>%
  mutate(diff=prec2010_19-precip,
         perc_change=(prec2010_19-precip)/(precip+0.1))

final %>%
  ggplot()+
  geom_tile(aes(x=x,y=y,fill=precip))+
  coord_quickmap()+
  scale_fill_gradient2(low="white",mid="darkblue",high="darkblue",midpoint=3000)+
  labs(title="Annual precipitation",
       subtitle="Average value for 1981-2010",
       caption="CPC Global Unified Precipitation data by NOAA/OAR/ESRL \nat ftp://ftp.cdc.noaa.gov/Datasets/cpc_global_precip",
       fill="Precipitation (mm)")+
  theme(panel.background = element_rect(fill="grey80"),
        plot.caption = element_text(size=7),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank())



final %>%
  ggplot()+
  geom_tile(aes(x=x,y=y,fill=perc_change))+
  coord_quickmap()+
  scale_fill_gradient2(labels = function(x) sprintf("%+f", x))+
  labs(title="Difference in Precipitation",
       subtitle="Comparing average annual precipitation 2010-2019 with 1981-2010",
       caption="CPC Global Unified Precipitation data by NOAA/OAR/ESRL \nat ftp://ftp.cdc.noaa.gov/Datasets/cpc_global_precip",
       fill="Difference (mm)")+
  theme(panel.background = element_rect(fill="grey80"),
        plot.caption = element_text(size=7),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank())

final_south_america <- final



#### Europe

ltm <- brick("C:/Richard/R and Python/Environmental Data Science/GIS/ChileClimate/Data_raw/precip.day.1981-2010.ltm.nc") %>%
  rotate()

#Selected countries
data("World")
countries <- World %>%
  filter(continent%in%"Europe") %>%
  st_transform(crs(ltm)@projargs)

crs(ltm) <- crs(countries)

summarise_precip <- function(df) {
  out <- df %>% 
    mask(countries) %>% 
    rasterToPoints() %>% 
    data.frame() %>% 
    tidyr::pivot_longer(cols=-c(x,y),names_to="date",values_to="value") %>% 
    group_by(x,y) %>% 
    summarise(precip=sum(value)) %>% 
    ungroup()
  
  return(out)
}

ltm_df <- summarise_precip(ltm)
rm(ltm)

for(year in 2011:2019) {
  dat <- brick(paste0("C:/Richard/R and Python/Environmental Data Science/GIS/ChileClimate/Data_raw/precip.",year,".nc")) %>%
    rotate()
  
  crs(dat) <- crs(countries)
  
  assign(paste0("df",year),summarise_precip(dat))
}


mean_10y <- rbind(df2010,df2011,df2012,df2013,df2014,df2015,df2016,df2017,df2018,df2019) %>%
  group_by(x,y) %>%
  summarise(prec2010_19=mean(precip))

final <- mean_10y %>%
  inner_join(ltm_df,by=c("x","y")) %>%
  mutate(diff=prec2010_19-precip,
         perc_diff=(prec2010_19-precip)/precip)

final %>%
  filter(x>(-25),x<36,y>35,y<72) %>%
  ggplot()+
  geom_tile(aes(x=x,y=y,fill=precip))+
  coord_quickmap()+
  scale_fill_gradient2(low="white",mid="darkblue",high="darkblue",midpoint=3000)+
  labs(title="Annual precipitation",
       subtitle="Average value for 1981-2010",
       caption="CPC Global Unified Precipitation data by NOAA/OAR/ESRL \nat ftp://ftp.cdc.noaa.gov/Datasets/cpc_global_precip",
       fill="Precipitation (mm)")+
  theme(panel.background = element_rect(fill="grey80"),
        plot.caption = element_text(size=7),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank())



final %>%
  filter(x>(-25),x<36,y>35,y<72) %>%
  ggplot()+
  geom_tile(aes(x=x,y=y,fill=perc_diff))+
  coord_quickmap()+
  scale_fill_gradient2(labels = function(x) sprintf("%+f", x))+
  labs(title="Difference in Precipitation",
       subtitle="Comparing average annual precipitation 2010-2019 with 1981-2010",
       caption="CPC Global Unified Precipitation data by NOAA/OAR/ESRL \nat ftp://ftp.cdc.noaa.gov/Datasets/cpc_global_precip",
       fill="Difference (mm)")+
  theme(panel.background = element_rect(fill="grey80"),
        plot.caption = element_text(size=7),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank())
