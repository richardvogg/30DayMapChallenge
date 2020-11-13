library(sf)
library(raster)
#library(tabularaster)
library(dplyr)
library(ggplot2)

options(scipen=10)

#from: https://psl.noaa.gov/data/gridded/data.cpc.globalprecip.html

#Load Rasters
prec <- brick("C:/Richard/R and Python/Environmental Data Science/GIS/ChileClimate/Data_raw/precip.2019.nc")
prec <- rotate(prec)


### The whole world

library(gganimate)
library(tmap)

data("World")

germany <- World %>% filter(name=="Germany") %>%
   st_transform(crs(prec)@projargs)

crs(prec) <- crs(germany)


rt_germany <- prec %>% 
  mask(germany) %>% 
  rasterToPoints() %>% 
  data.frame() %>% 
  tidyr::pivot_longer(cols=-c(x,y),names_to="date",values_to="value") %>% 
  mutate(date=gsub("X","",date),
         date=gsub("[.]","-",date) %>% as.Date("%Y-%m-%d"))

cities <- data.frame(
  city=c("Berlin","Munich","Hamburg","Cologne"),
  lon=c(52.516667,48.15,53.575323,50.933333),
  lat=c(13.4,11.583333,10.01534,6.95)
)

rt_germany %>%
  group_by(x,y) %>%
  summarise(total_precipitation=sum(value)) %>%
  ggplot()+
  geom_tile(aes(x=x,y=y,fill=total_precipitation))+
  geom_point(data=cities,aes(x=lat,y=lon))+
  geom_label(data=cities,aes(x=lat,y=lon,label=city),fill = alpha("white",0.5),nudge_y = -0.4)+
  borders(regions="Germany")+
  coord_quickmap()+
  scale_fill_gradient2(low = "lightblue", mid = "blue", high = "darkblue",midpoint = 900)+
  labs(title = 'Total precipitation 2019 in Germany',
       fill = "Precip (mm)",
       caption = "CPC Global Unified Precipitation data provided by the NOAA/OAR/ESRL from their Web site at https://psl.noaa.gov/") +
  theme_void()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),plot.caption = element_text(size=7))



anim <- rt_germany %>%
  mutate(value=ifelse(value==0,NA,value)) %>%
  ggplot()+
  geom_tile(aes(x=x,y=y,fill=value))+
  geom_point(data=cities,aes(x=lat,y=lon))+
  geom_label(data=cities,aes(x=lat,y=lon,label=city),fill = alpha("white",0.5),nudge_y = -0.3)+
  borders(regions="Germany")+
  coord_quickmap()+
  transition_states(date,state_length = 0.3)+
  enter_fade()+
  exit_shrink()+
  scale_fill_gradient2(low = "white", mid = "deepskyblue2", 
                       high = "darkblue",midpoint = 25,na.value = "white")+
  labs(title = 'Date: {closest_state}',
       fill = "Precipitation in mm",
       caption = "CPC Global Unified Precipitation data provided by the NOAA/OAR/ESRL from https://psl.noaa.gov/") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),plot.caption = element_text(size=7))


# Video output
an <- animate(anim,nframes=1000)

anim_save("precip.gif",an)



#### Other things and ideas


#Get the scale of measurement
ncin <- ncdf4::nc_open("C:/Richard/R and Python/Environmental Data Science/GIS/ChileClimate/Data_raw/precip.2019.nc")
ncdf4::ncatt_get(ncin,"precip","units")
ncdf4::nc_close(ncin)

#Load Shapefiles from Chilean regions
regiones <- read_sf("C:/Richard/R and Python/Environmental Data Science/Regiones/Regional.shp")


### Optional: preprocessing - removing Easter Island
## I am sure there are more elegant ways to solve this

#For Valparaiso remove Easter Island and Isla Juan Fernandez, then buffer to make it larger
valpo <- read_sf("C:/Richard/R and Python/Environmental Data Science/Comunas/comunas.shp") %>% 
  filter(!Comuna %in% c("Isla de Pascua","Juan Fernández"),Region=="Región de Valparaíso") %>% 
  st_simplify(dTolerance=500) %>% 
  st_buffer(5000) %>% 
  st_union()


#Plot Valparaiso Region without Easter Island
valpo %>% 
  ggplot()+
  geom_sf()+
  theme(legend.position = "none")

#Intersect Valparaiso with the new Valpo without Easter Island
test <- regiones %>% filter(Region=="Región de Valparaíso") %>% 
  st_buffer(dist=0) %>% 
  st_intersection(valpo)

#Remove old Valparaiso region from regions and add new Valpo
regiones <- regiones %>% 
  filter(!Region%in%c("Región de Valparaíso","Zona sin demarcar")) %>%
  rbind(test)

#Simplified version for faster visualization
reg_simp <- regiones %>% 
  st_simplify(dTolerance=8000)

reg_simp %>% 
  ggplot()+
  geom_sf()+
  theme(legend.position = "none")

regiones <- regiones %>% st_transform(crs(prec)@projargs)
reg_simp <- reg_simp %>% st_transform(crs(prec)@projargs)

crs(prec) <- crs(regiones)

#Convert to Dataframe and visualize

rt <- prec %>% 
  mask(reg_simp) %>% 
  rasterToPoints() %>% 
  data.frame() %>% 
  tidyr::pivot_longer(cols=-c(x,y),names_to="date",values_to="value") %>% 
  mutate(date=gsub("X","",date),
         date=gsub("[.]","-",date) %>% as.Date("%Y-%m-%d"))


rt %>% mutate(month=format(date,"%b")) %>%
  mutate(month=forcats::fct_reorder(month,as.numeric(format(date,"%m")))) %>%
  group_by(x,y,month) %>% 
  summarise(value=sum(value)) %>% 
  ggplot()+
  geom_tile(aes(x=x,y=y,fill=log(value)))+
  coord_quickmap()+
  facet_grid(~month)+
  labs(caption = "CPC Global Unified Precipitation data provided by the NOAA/OAR/ESRL PSL, Boulder, Colorado, USA, from their Web site at https://psl.noaa.gov/")+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),plot.caption = element_text(size=7))

