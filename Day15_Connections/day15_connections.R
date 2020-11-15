library(tradestatistics)
library(maptools)
library(dplyr)
library(ggplot2)

options(scipen=15)

data("wrld_simpl") 


World <- data.frame(iso3=tolower(wrld_simpl$ISO3),
           lat=wrld_simpl$LAT,
           lon=wrld_simpl$LON)

products <- tradestatistics::ots_products
waste <- ots_product_code("waste")
#product code 3915 - Plastic waste


ots_country_code("USA")


test <- ots_create_tidy_data(years=c(1988,1998,2008,2018),reporters="usa",partners="all",
                             products="3915",table="yrpc") %>%
  left_join(World,by=c("partner_iso"="iso3"))



change <- test %>% select(partner_iso,export_value_usd,year,lon,lat) %>%
  filter(year %in% c(2008,2018)) %>%
  arrange(year) %>%
  tidyr::pivot_wider(id_cols=c(partner_iso,lon,lat),names_from=year,values_from=export_value_usd) %>% 
  filter(`2008`>1000000 | `2018`>1000000) %>% 
  mutate(perc_change=`2018`/`2008`-1,
         perc_change = cut(perc_change,breaks=c(-1,-0.5,0,0.5,100),labels=c("-50% or less","0 to -50 %",
                                                                          "0 to +50%","+50% or more"))) %>%
  mutate(year=2018)


ggplot()+
  borders("world")+
  geom_curve(data=subset(test,year%in%c(2008,2018) & partner_iso %in% change$partner_iso),
               aes(x=-98.606,y=39.622,xend=lon,yend=lat,size=export_value_usd),
             curvature = 0.1,alpha=0.8,col="mediumpurple4",lineend = "butt")+
  geom_point(data=change,shape=21,aes(x=lon,y=lat,fill=perc_change),size=3)+
  scale_size_continuous(breaks=c(1e6,1e7,1e8,1e11),range=c(0.5,3),
                        labels=c("$1MM","$10MM","$100MM","more"))+
  scale_alpha_continuous(breaks=c(1e6,1e7,1e8,1e11),range=c(0.1,1),guide = FALSE)+
  scale_fill_manual(values=c("blue","skyblue1","tomato1","red"))+
  labs(title="US plastic waste exports",subtitle="Changes after China's ban on importing waste (January 2018)",
    fill="Percentage change \ncompared to 2008",
    size="Export value (USD)",
    caption="Data: Open Trade Statistics")+
  facet_grid(year~.)+
  coord_quickmap()+
  theme_light()+
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(colour="black"))


test %>% 
  filter(partner_iso=="chn") %>%
  group_by(year) %>%
  summarise(total=sum(export_value_usd,na.rm=TRUE)) %>% 
  ggplot(aes(x=year,y=total,group=1))+geom_line()








test %>% filter(year %in% c(2008,2018)) %>% group_by(year,partner_fullname_english) %>%
  summarise(exported = max(export_value_usd),imported=max(import_value_usd)) %>% 
  top_n(10,exported) %>% arrange(year,desc(exported)) %>% 
  ggplot(aes(x=reorder(partner_fullname_english,exported),y=exported))+geom_col()+
  coord_flip()+
  facet_wrap(~year)



