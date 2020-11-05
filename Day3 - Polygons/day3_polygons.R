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

### Valparaiso and Viña del Mar

valpo <- comunas %>% 
  filter(Comuna%in%c("Valparaíso","Viña del Mar"))

surroundings <- comunas %>%
  filter(Comuna%in%c("Valparaíso","Viña del Mar","Concón",
                     "Casablanca","Limache","Quilpué","Villa Alemana")) %>%
  st_crop(st_bbox(valpo))


surroundings %>%
ggplot()+geom_sf()+geom_sf_text(aes(label=Comuna))

lakes_in_valpo <- st_join(aguas_transformed,valpo,left=FALSE) %>%
  mutate(Nombre=ifelse(Nombre=="Estero Vina del Mar","Estero Viña del Mar",Nombre))

lakes_in_valpo$Nombre

lakes_in_valpo$include <- c(0,0,1,0,0,0,1,1,0,0,0,1,1,0,1,1,0)
subset(lakes_in_valpo,include==1)$Nombre

nudge_x_vals <- c(-2000,-500,-4000,2300,3000,0,-1000)
nudge_y_vals <- c(-700,-2800,-500,1000,-800,-500,-1000)

textcol <- "midnightblue"

ggplot() + 
  geom_sf(data=surroundings,fill="grey90")+
  geom_sf(data=valpo,fill="grey70")+
  geom_sf(data=lakes_in_valpo,aes(fill=Tipo,col=Tipo))+
  geom_sf_text(data=subset(lakes_in_valpo,include==1),
               aes(label=Nombre,),nudge_x=nudge_x_vals,nudge_y=nudge_y_vals,
               family="sans",col=textcol,size=4)+
  labs(title="Masas de agua en Valparaíso y Viña del Mar, Chile",x="",y="",
       caption="Data: Biblioteca del Congreso Nacional de Chile")+
  theme_light()+
  theme(plot.background = element_rect(fill = "aliceblue"),
        panel.background = element_rect(fill="aliceblue"),
        plot.title = element_text(family = "sans", face = "bold", size = 15, colour = textcol),
        plot.caption = element_text(family = "sans" ,size=10, colour = textcol))
