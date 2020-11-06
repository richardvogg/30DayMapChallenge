library(sf)
library(ggplot2)
library(dplyr)

options(scipen = 10)

### Load and transform data

# data from https://www.bcn.cl/siit/mapas_vectoriales

aguas <- read_sf("C:/Richard/R and Python/Environmental Data Science/Masas_Lacustres/masas_lacustres.shp")
comunas <- read_sf("C:/Richard/R and Python/Environmental Data Science/Comunas/comunas.shp")

aguas_transformed <- st_transform(aguas,crs = 32719)
comunas <- st_transform(comunas,crs = 32719)


### Los Lagos

loslagos <- comunas %>% 
  filter(Region == "Región de Los Lagos")


#Remove duplicate names
lakes_in_loslagos <- st_join(aguas_transformed,loslagos,left=FALSE) %>%
  group_by(Nombre,st_area_sh.x) %>%
  mutate(rnk=rank(Nombre,ties.method="first")) %>% 
  mutate(Nombre=ifelse(rnk==1,Nombre,NA)) %>%
  ungroup() %>%
  mutate(include=st_area_sh.x>100000000)


subset(lakes_in_loslagos,include)$Nombre




textcol <- "midnightblue"

ggplot() + 
  geom_sf(data=loslagos,fill="grey70",col="grey70")+
  geom_sf(data=lakes_in_loslagos,fill="midnightblue",col="midnightblue")+
  ggrepel::geom_text_repel(
    data = subset(lakes_in_loslagos,include & Nombre!= "Lago Yelcho"),
    aes(label = Nombre, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0,
    family="sans",col=textcol,size=3,nudge_x= -60000,nudge_y=18000
  )+
  ggrepel::geom_text_repel(
    data = subset(lakes_in_loslagos,include & Nombre=="Lago Yelcho"),
    aes(label = Nombre, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0,
    family="sans",col=textcol,size=3,nudge_x= -15000,nudge_y=-20000
  )+
  labs(title="Lakes in Los Lagos region, Chile",x="",y="",
       caption="Data: Biblioteca del Congreso Nacional de Chile")+
  theme_light()+
  theme(plot.background = element_rect(fill = "aliceblue"),
        panel.background = element_rect(fill="aliceblue"),
        plot.title = element_text(family = "sans", face = "bold", size = 15, colour = textcol),
        plot.caption = element_text(family = "sans" ,size=10, colour = textcol))

