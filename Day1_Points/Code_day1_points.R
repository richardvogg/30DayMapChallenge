library(ggplot2)
library(dplyr)
library(ggrepel)

#data can be downloaded here: https://earthquake.usgs.gov/earthquakes/search/

df <- read.csv("C:/Richard/R and Python/Datasets/Earthquakes Chile July 2019.csv") %>% 
  mutate(year=substr(time,1,4)) %>%
  filter(year>=2000) %>%
  mutate(country=place %>% 
            strsplit(split=", ") %>% 
            lapply(function(x) x <- x[2]) %>%
            unlist()
  ) %>%
  filter(country=="Chile") %>%
  mutate(region=place %>% 
           strsplit(split="of |, ") %>% 
           lapply(function(x) x <- x[length(x)-1]) %>%
           unlist()
  )

textcol <- "midnightblue"

ggplot() + 
  borders(regions="Chile", fill=NA)+
  geom_point(data=subset(df,mag<=7.5),
             aes(x=longitude,y=latitude,
                 size=mag),col=textcol,alpha=0.1)+
  geom_point(data=subset(df,mag>7.5),
             aes(x=longitude,y=latitude),col="goldenrod2",size=3)+
  geom_text_repel(data=subset(df,mag>7.5 & longitude<(-70) & latitude>=(-40)),
                  aes(x=longitude,y=latitude,label=paste(year,"\n",region,"\n",mag)),
                  nudge_x = -7,col=textcol,family="sans")+
  geom_text_repel(data=subset(df,mag>7.5 & (longitude>(-70)|latitude<(-40))),
                  aes(x=longitude,y=latitude,label=paste(year,"\n",region,"\n",mag)),
                  nudge_x = 7,col=textcol,family="sans")+
  scale_size_continuous(breaks = c(5,6,7,8.8),range = c(0.5,3))+
  labs(title="Earthquakes in Chile in the 21st century",subtitle="Events over 7.5 magnitude highlighted",x="",y="",
       caption="Data from: https://earthquake.usgs.gov/earthquakes/search/")+
  theme_light()+
  theme(legend.position="none",
        plot.background = element_rect(fill = "aliceblue"),
        panel.background = element_rect(fill="aliceblue"),
        plot.title = element_text(family = "sans", face = "bold", size = 13, colour = textcol),
        plot.subtitle = element_text(family = "sans" ,size=11, colour = textcol),
        plot.caption = element_text(family = "sans" ,size=10, colour = textcol),
        axis.text=element_blank(),
        axis.ticks=element_blank())+
  coord_quickmap()+
  xlim(c(-85,-60))+
  ylim(c(-56,-17))
