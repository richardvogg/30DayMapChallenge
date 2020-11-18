library(ggplot2)
library(dplyr)
library(tmap)
library(stringr)
library(sf)

#Data from https://stats.oecd.org/Index.aspx?DataSetCode=LAND_COVER#

sysfonts::font_add_google(name = "Amatic SC","Amatic SC")
showtext::showtext_auto()


df <- read.csv("C:/Richard/R and Python/Datasets/LAND_COVER_18112020121654040.csv") %>%
  filter(MEAS=="PCNT")

data("World")




cleaner <- function(vec) {
  vec %>% tolower() %>%
    str_replace("democratic people's republic of","north") %>%
    str_replace("republic","rep.") %>%
    str_replace("^korea","south korea") %>%
    str_replace("democratic","dem.") %>%
    str_replace("south","s.") %>%
    return()
}



fuzzy_matches <- function(clean_vec,dirty_vec) {
  control <- data.frame(original=dirty_vec)
  
  distmatrix <- stringdist::stringdistmatrix(cleaner(clean_vec),cleaner(dirty_vec),method='jw',p=0.1)
  best_fit <- apply(distmatrix,2,which.min) %>% as.integer()
  similarity <- apply(distmatrix,2,min)
  
  control$best_fit <- clean_vec[best_fit]
  control$similarity <- similarity 
  
  
  return(control %>% arrange(desc(similarity)))
}

dict <- fuzzy_matches(World$name,unique(df$Country)) %>%
  mutate(best_fit=as.character(best_fit)) %>%
  filter(similarity<0.159,!original%in%c("Malta","United States Virgin Islands",
                                         "Macau, China","Hong Kong, China",
                                         "Samoa","Chinese Taipei","Guam"))


final <- df %>% 
  select(Country,Land.cover.class,Year,Value) %>%
  tidyr::pivot_wider(id_cols=c(Country,Year),
                     values_from=Value,
                     names_from=Land.cover.class) %>%
  transmute(Country,Year,
            `Natural Land Cover`=`Inland water`+`Tree cover`+`Shrubland`+
              `Sparse vegetation`+`Wetland`+`Grassland`,
            Cropland,`Bare area`,`Artificial surfaces`) %>% 
  tidyr::pivot_longer(-c(Country,Year),names_to="Land.cover.class",values_to="Value") %>% 
  mutate(name_new=plyr::mapvalues(Country,from=dict$original,to=dict$best_fit)) %>% 
  full_join(World,by=c("name_new"="name")) %>%
  select(Country,Land.cover.class,Year,Value,iso_a3,geometry) %>%
  st_as_sf(sf_column_name="geometry")

top2 <- final %>%
  filter(!is.na(iso_a3),Year==2018) %>%
  group_by(Land.cover.class) %>%
  top_n(2,Value) %>%
  ungroup()


final %>%
  filter(!is.na(Land.cover.class),!is.na(iso_a3)) %>%
  filter(Year==2018) %>%
  group_by(Land.cover.class) %>%
  mutate(min_val=min(Value),
         max_val=max(Value)) %>%
  mutate(norm_value=(Value-min_val)/(max_val-min_val)) %>% 
  ungroup() %>% 
  ggplot()+geom_sf(aes(fill=Land.cover.class,alpha=norm_value),col=NA)+
  geom_sf_text(data=top2,aes(label=paste0(Country,"\n",round(Value,1),"%")))+
  facet_wrap(~Land.cover.class)+
  labs(title="Percentage of land cover by type",
       caption="Data from: OECD.Stat - Land Cover")+
  theme(legend.position = "none",
        axis.title=element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(family="Amatic SC",size=35),
        strip.text = element_text(family="Amatic SC",size=20),
        plot.caption = element_text(family="Amatic SC",size=15))


