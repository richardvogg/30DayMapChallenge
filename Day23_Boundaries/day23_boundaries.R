library(dplyr)
library(ggplot2)
library(countrycode)
library(tmap)
library(sf)

df <- read.csv("C:/Richard/R and Python/Datasets/UN Women Data Hub map-export.csv") %>%
  mutate(iso_a3=countrycode(df$REF_AREA.Code, origin = 'iso3n', destination = 'iso3c')) %>%
  select(TIME_PERIOD,OBS_VALUE,SOURCE_DETAIL,COMMENT_OBS,iso_a3)

data("World")

europe <- World %>% 
  st_transform(crs=4326) %>%
  filter(continent=="Europe") %>%
  left_join(df,by="iso_a3") %>%
  mutate(value=cut(OBS_VALUE,breaks=c(0,4,8,12,20),labels=c("<4%","4-8%","8-12%",">12%")))

ggplot(europe)+geom_sf(aes(fill=value))+
  xlim(c(-11,37.5))+ylim(c(35,70))+
  scale_fill_manual(values=c("tomato1","firebrick2","red3","red4"),na.value="grey80")+
  labs(title="Domestic violence",
       subtitle="1 out of 16 women in Europe were subject to physical and/or sexual violence \nby a current or former intimate partner in the previous 12 months",
       caption="Data from UN Women",
       fill="")+
  theme_void()+
  theme(legend.position=c(0.1,0.9),
        plot.title=element_text(size=20,family="Sans"),
        plot.subtitle = element_text(size=12,family="Sans"),
        plot.caption=element_text(size=10,family="Sans"))
