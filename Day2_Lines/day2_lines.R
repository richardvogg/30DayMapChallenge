library(sf)
library(ggplot2)
library(dplyr)
library(chilemapas)


ferroviaria <- read_sf("C:/Richard/R and Python/Datasets/Red ferroviaria/Red_ferroviaria.shp")

mapa_comunas %>% 
  filter(!codigo_comuna%in%c("05201","05104")) %>%
  generar_regiones() %>% 
ggplot() + 
  geom_sf(col="grey80",fill="grey80") +
  geom_sf(data=ferroviaria %>% filter(Activ_2016=="SI"),size=1,col="midnightblue")+
  labs(title="Active rails in Chile",subtitle="(Almost) not used for public transport.",
       caption="Data from Biblioteca del Congreso Nacional de Chile",
       x="",y="")+
  theme_light()+
  theme(legend.position="none",
        plot.background = element_rect(fill = "aliceblue"),
        panel.background = element_rect(fill="aliceblue"),
        plot.title = element_text(family = "sans", face = "bold", size = 13, colour = textcol),
        plot.subtitle = element_text(family = "sans" ,size=11, colour = textcol),
        plot.caption = element_text(family = "sans" ,size=10, colour = textcol),
        axis.text=element_blank(),
        axis.ticks=element_blank())+
  xlim(c(-85,-60))+
  ylim(c(-56,-17))
