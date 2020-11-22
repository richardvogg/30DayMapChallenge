library(tmap)
library(dplyr)
library(ggplot2)
library(sf)


data("World")
Europe <- World %>%
  st_transform(crs=4326) %>%
  filter(continent=="Europe")

personalities <- data.frame(country=as.character(Europe$name))

#Extraverted vs Introverted
#Introverted individuals prefer solitary activities and get exhausted by social interaction.
#Extraverted individuals prefer group activities and get energized by social interaction.


#with autorization from https://www.16personalities.com/country-profiles/global/europe
personalities$ext_vs_int <- c(5.02,-3.48,-0.11,-0.16,-0.34,-5.66,3.6,-6.74,-6.17,-0.71,0.45, # Spain
                              -6.92,-9.3,-1,-0.33,0.03,-0.43,-5.07,-0.68,-1.39,-6.94,NA, #Kosovo
                              -11.19,-0.39,-7.98,0.54,3.25,4.23,2.07,-1.54,-9.66,-9.3, #Portugal
                              1.58,-7.8,2.2,-7.03,-2.49,-0.05,1.1)

sysfonts::font_add_google(name = "Acme","Acme")
showtext::showtext_auto()

Europe %>% 
  inner_join(personalities,by=c("name"="country")) %>%
  ggplot()+geom_sf(aes(fill=ext_vs_int))+
  scale_fill_gradient2(low = "#56B4E9",mid="white",high="#D55E00",breaks=c(-10,5),
                       labels=c("Higher % of \nintroverted people","Higher % of \nextraverted people"),
                       name="")+
  xlim(c(-23,37.5))+ylim(c(35,70))+
  labs(title="Extraversion vs Introversion in Europe",
       subtitle="Results from 16personalities surveys",
       caption="Data with friendly authorization from www.16personalities.com/country-profiles/global/europe")+
  theme_void()+
  theme(legend.position = c(0.1,0.5),
        plot.title=element_text(size=20,family="Acme"),
        plot.subtitle = element_text(size=16,family="Acme"),
        plot.caption=element_text(size=12,family="Acme"))
