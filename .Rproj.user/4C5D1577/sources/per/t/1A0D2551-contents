library(ggplot2)
library(dplyr)
library(osmdata) #Open Street Map

sysfonts::font_add_google(name = "Crimson Text","Crimson Text")
sysfonts::font_add_google(name = "Permanent Marker","Permanent Marker")
showtext::showtext_auto()

# 1. Valparaíso 

# Unfortunately the bounding box of Valparaiso is not super good, so we adapt it
bbox <- getbb("Valparaíso,Valparaiso Chile")
bbox[1,] <- c(-71.6615, -71.5607)
bbox[2,] <- c(-33.08, -33.017)


coastline <- bbox %>%
  opq() %>%
  add_osm_feature(key = "natural",value="coastline") %>%
  osmdata_sf()

surface <- bbox %>%
  opq() %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()

valpo <- ggplot()+
  geom_sf(data = coastline$osm_lines,size=1,col="#846d63")+
  geom_sf(data = subset(surface$osm_lines,highway=="primary"),col="#bac0c4",size=0.8)+
  geom_sf(data = subset(surface$osm_lines,highway%in%c("secondary","tertiary")),col="#cbc4b8",size=0.5)+
  geom_sf(data = subset(surface$osm_lines,!highway%in%c("primary","secondary","tertiary")),col="#d2c1b6")+
  labs(title="Valparaíso",subtitle="33.05°S / 71.61°W")+
  ylim(c(-33.08,-33.02))+xlim(c(-71.67,-71.56))+
  theme_void()+
  theme(panel.background = element_rect(colour="#ededed"),
        plot.title = element_text(family = "Crimson Text", size = 30, colour = "#846d63"),
        plot.subtitle = element_text(family = "Crimson Text" ,size=20, colour = "#846d63"))


## Small Chile inset

coord_valpo <- data.frame(lat = -71.61,lon = -33.05)

loc <- ggplot()+
  borders("world",regions=c("Brazil", "Uruguay", "Argentina", "French Guiana", "Suriname", 
                            "Colombia", "Venezuela","Bolivia", "Ecuador", "Chile", 
                            "Paraguay", "Peru", "Guyana"))+
  geom_point(data=coord_valpo,aes(x=lat,y=lon),col="black",size=3)+
  xlim(c(-85,-34))+
  theme_void()


library(patchwork)




png("Day9_Monochrome/plot.png",width=600,height=500)
valpo + inset_element(loc,0.4,0.85,0.5,1,align_to = "full")
dev.off()
