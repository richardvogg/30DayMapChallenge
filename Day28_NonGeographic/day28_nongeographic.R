remotes::install_github("JoGall/soccermatics")


library(ggsoccer)
library(ggplot2)
library(dplyr)
library(soccermatics)
data("statsbomb")

to_opta <- rescale_coordinates(from = pitch_statsbomb, to = pitch_opta)

mbappe <- statsbomb %>% filter(player.name=="Kylian Mbappé Lottin") %>%
  select(location,type.name) %>%
  mutate(filter_null=lapply(location,is.null)) %>%
  filter(filter_null==FALSE) %>%
  mutate(x=lapply(location,function(x) x <- x[1]) %>% unlist(),
          y=lapply(location,function(x) x <- x[2]) %>% unlist())

ggplot(mbappe,aes(x=x,y=y)) +
  annotate_pitch(dimensions = pitch_statsbomb,fill = "darkgreen",col="white")+
  #stat_density2d()+
  stat_density_2d(aes(fill = ..density..,alpha=..density..), geom = "raster", contour = FALSE) +
  geom_point(data=subset(mbappe,type.name=="Shot"),col="red",size=5,shape=4,guide=TRUE)+
  theme_pitch()+
  theme(legend.position='none')+
  labs(title="Heatmap and goalshots of Kylian Mbappé during France 4 - 3 Argentine in 2018",
       subtitle="The 19-years-old superstar scored two goals in this quarter final of the World Cup",
       caption="Data from Statsbomb")
