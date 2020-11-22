library(sf)
library(dplyr)
library(ggplot2)

lines <- read_sf("C:/Richard/R and Python/Datasets/White Stork Bulgaria/lines.shp")
points <- read_sf("C:/Richard/R and Python/Datasets/White Stork Bulgaria/points.shp") %>%
  mutate(year=substr(timestamp,1,4),
         month=substr(timestamp,6,7),
         day = substr(timestamp,9,10),
         date=as.Date(substr(timestamp,1,10)),
         month_cat=case_when(
           date<'2016-03-21' | date > '2016-12-21' ~ "Northern Winter",
           date<'2016-06-20' ~ "Northern Spring",
           date<'2016-09-21' ~ "Northern Summer",
           TRUE ~ "Northern Autumn"),
         month_cat=as.factor(month_cat) %>% forcats::fct_relevel("Northern Autumn",after=2)) %>%
  filter(ind_ident!="Viktor",year==2016) %>%
  filter(long>0,lat<50)


ggplot()+
  borders(database = "world",fill="grey20",col=NA) +
  geom_point(data=points,aes(x=long,y=lat,col=month_cat),size=1)+
  xlim(c(-25,65))+ylim(c(-35,70))+
  facet_wrap(~ind_ident)+
  scale_color_manual(values=c("springgreen2","goldenrod","brown2","turquoise3"))+
  coord_quickmap()+
  labs(title="The movement of three white storks 2016",
      caption="Data: Movebank for animal tracking",
      col="")+
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        plot.title=element_text(size=20,family="Acme"),
        plot.caption=element_text(size=12,family="Acme"),
        strip.text = element_blank())
