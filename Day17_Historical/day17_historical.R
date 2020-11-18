#The Midpoint of the European Union
#remotes::install_github("coolbutuseless/ggpattern")

library(ggplot2)
library(dplyr)
library(tmap)
library(ggpattern)
library(patchwork)


sysfonts::font_add_google(name = "Crimson Text","Crimson Text")
showtext::showtext_auto()


europeanUnion <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                   "Czech Rep.","Denmark","Estonia","Finland","France",
                   "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                   "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                   "Portugal","Romania","Slovakia","Slovenia","Spain",
                   "Sweden","United Kingdom")

join <- c(1995,1957,2007,2013,2004,2004,1973,2004,
                1995,1957,1957,1981,2004,1973,1957,2004,2004,
                1957,2004,1957,2004,1986,2007,2004,2004,1986,
                1995,2020)

left <- c(rep(NA,27),2020)


country_df <- data.frame(
  name=europeanUnion,
  join,
  left
)

data("World")
EU <- World %>%
  st_transform(crs=4326) %>%
  filter(continent=="Europe"|name%in%europeanUnion) %>%
  left_join(country_df,by="name")

eu_hist <- EU %>%
  mutate(join=ifelse(join<=1995,1995,join)) %>%
ggplot()+
  geom_sf(aes(fill=factor(join)))+
  geom_sf_pattern(data=subset(EU,name=="United Kingdom"),
                  aes(pattern=left),
                  fill="yellowgreen",
                  pattern="stripe",pattern_density=0.1,
                  pattern_spacing=0.02,
                  show.legend = F)+
  geom_sf_text(data=subset(EU,join>1995),aes(label=name),size=3)+
  scale_fill_brewer(palette = "Set2",name="Joining/leaving date",
                    labels=c("Before 1995","2004","2007","2013","2020","Not part of EU"))+
  xlim(c(-11,34))+ylim(c(25,72))+
  theme_void()


midpt_places <- World %>%
  st_transform(crs=4326) %>%
  filter(name %in% c("Germany","Belgium","Netherlands","France",
                     "Luxembourg","Austria","Czech Rep.","Poland"))

time <- c("1995-2004","2004-2006","2007-2013","2013-2020","since 2020")
place <- c("Viroinval","Kleinmascheid","Meerholz","Westerngrund","Gadheim")
what <- c(NA,"EU Osterweiterung (10 countries)","Bulgaria and Romania","Croatia",
          "Brexit")
lat <- c(50.009167,50.525278,50.1725,50.113388,49.843056)
lon <- c(4.666389,7.597222,9.15,9.252536,9.901944)

nudgex <- c(0,0,0,2,1)
nudgey <- c(-1,1,1,0.5,-1)

midpoints <- data.frame(
  time,place,what,lat,lon,nudgex,nudgey
)

midpt <- ggplot(midpt_places)+geom_sf()+
  geom_sf_label(aes(label=name))+
  geom_point(data=midpoints,aes(x=lon,y=lat,fill=time),
             shape=21,size=5,
             show.legend = FALSE)+
  ggrepel::geom_text_repel(data=midpoints,aes(x=lon,y=lat,
                                              label=paste0(place,"\n",time)),
                           nudge_x=midpoints$nudgex,nudge_y=midpoints$nudgey)+
  scale_fill_brewer(palette="Set2")+
  xlim(c(3,14))+ylim(47,54)+
  theme_void()





midpt + eu_hist +
  plot_annotation(title="The historical centers of the European Union",
                  subtitle="After the Brexit in January 2020 Gadheim is the new center of the EU.",
                  caption="Information from Wikipedia") &
  theme(plot.title=element_text(size=25,family = "Crimson Text",colour="darkblue"),
        plot.subtitle=element_text(size=18,family = "Crimson Text"),
        plot.caption=element_text(size=12,family = "Crimson Text"))
