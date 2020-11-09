library(maps)
library(dplyr)
library(ggplot2)
library(ggrepel)

cities <- maps::world.cities %>%
  filter(country.etc=="Germany")

green_cities <- c("Potsdam","Kassel","Bremen","Magdeburg","Munich","Leipzig",
                  "Gelsenkirchen","Osnabruck","Bielefeld","Oldenburg")
green_areas <- c(33.03,23.42,21.32,19.4,17.78,17.15,16.85,15.18,14.96,14.44)

df <- data.frame(green_cities,green_areas) %>%
  mutate(rnk=rank(desc(green_areas))) %>% 
  inner_join(cities,by=c("green_cities"="name"))

textcol <- "green4"
  
ggplot() + 
  borders(regions="Germany", fill="darkolivegreen2")+
  geom_point(data=df,aes(x=long,y=lat),col=textcol)+
  geom_label_repel(data=subset(df,rnk<=3),
                  aes(x=long,y=lat,label=paste0(rnk,". ",green_cities,"\n",green_areas)),
                  col=textcol,family="sans",size=4,nudge_x = 1,nudge_y=-0.2)+
  geom_label_repel(data=subset(df,rnk>3),
                  aes(x=long,y=lat,label=paste0(green_cities,"\n",green_areas)),
                  col=textcol,family="sans",size=3)+
  labs(title="Top 10 Greenest German Cities",
       subtitle=bquote("Measuring green areas per inhabitant in" ~ m^2 ~ "\n for cities over 100,000 inhabitants"),
       caption="Numbers from: https://www.holidu.de/magazine/10-grosstaedte-deutschlands-mit-der-meisten-gruenflaeche")+
  coord_quickmap()+
  theme(plot.background = element_rect(fill = "aliceblue"),
        panel.background = element_rect(fill="aliceblue"),
        plot.title = element_text(family = "sans", face = "bold", size = 15, colour = textcol),
        plot.subtitle = element_text(family = "sans", face = "bold", size = 10, colour = textcol),
        plot.caption = element_text(family = "sans" ,size=7, colour = textcol),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
  