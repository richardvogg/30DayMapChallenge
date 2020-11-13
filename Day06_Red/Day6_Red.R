#source: http://www.conaf.cl/incendios-forestales/incendios-forestales-en-chile/estadisticas-historicas/

library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(readxl)
library(chilemapas)

#######
#Load Data and clean
#######



change_colnames <- function(df) {
  colnames(df) <- c("month",colnames(df)[2:length(colnames(df))])
  return(df)
}


df_2019 <- read_xlsx("C:/Richard/R and Python/Datasets/Conaf_Waldbraende.xlsx",
                     sheet=3,range="A10:Q22")
df_2019 <- change_colnames(df_2019)
df_2019$year <- 2019


df_fires <- pivot_longer(df_2019,cols=2:(length(df_2019)-1),names_to="region",values_to="forest_fire_cnt")


#Replace missing values with 0
df_fires <- df_fires %>% mutate(forest_fire_cnt=ifelse(is.na(forest_fire_cnt),0,forest_fire_cnt))


#Adjust the years (period is going from July to June)

df_fires <- df_fires %>% mutate(month_num = case_when(
  month=="ENERO" ~ 1,
  month=="FEBRERO" ~ 2,
  month=="MARZO" ~ 3,
  month=="ABRIL" ~ 4,
  month=="MAYO" ~ 5,
  month=="JUNIO" ~ 6,
  month=="JULIO" ~ 7,
  month=="AGOSTO" ~ 8,
  month=="SEPTIEMBRE" ~ 9,
  month=="OCTUBRE" ~ 10,
  month=="NOVIEMBRE" ~ 11,
  month=="DICIEMBRE" ~ 12
)) %>%
  mutate(region_num = case_when(
    region=="RM" ~ "13",
    region=="I" ~ "01",
    region=="II" ~ "02",
    region=="III" ~ "03",
    region=="IV" ~ "04",
    region=="V" ~ "05",
    region=="VI" ~ "06",
    region=="VII" ~ "07",
    region=="VIII" ~ "08",
    region=="IX" ~ "09",
    region=="X" ~ "10",
    region=="XI" ~ "11",
    region=="XII" ~ "12",
    region=="XIV" ~ "14",
    region=="XV" ~ "15",
    region=="XVI" ~ "16")
  )


regions <- mapa_comunas %>% 
  filter(!codigo_comuna %in% c("05201","05104")) %>%
  generar_regiones()



fires <- df_fires %>%
  filter(year%in% 2016:2019) %>%
  group_by(region_num) %>%
  summarise(fires=round(sum(forest_fire_cnt))) %>%
  mutate(fire_bins=cut(fires,breaks=c(-1,500,900,1500,2500),
                       labels=c("0 to 500","500 to 900","900 to 1,500","1,500+"))) %>% 
  ungroup() %>%
 filter(region_num %in% c("04","05","06","07","08","09","14","13","16")) %>%
  left_join(regions,by=c("region_num"="codigo_region")) %>% 
  st_as_sf(sf_column_name = "geometry") %>%
  st_crop(xmin=-74,xmax = -69,ymin= -41,ymax = -29)

plot1 <- fires %>%
  ggplot()+
  geom_sf(aes(fill=fire_bins))+
  geom_sf_text(data=subset(fires,region_num=="05"),aes(label=fires),nudge_x=-0.5,nudge_y=0.3)+
  geom_sf_text(data=subset(fires,region_num!="05"),aes(label=fires))+
  scale_fill_brewer(palette="Reds")+
  labs(fill="Fires")+
  theme_minimal()+
  theme(axis.text = element_blank(),
        axis.title = element_blank())+
  xlim(c(-74,-68))


plot2 <- regions %>% 
  mutate(fill_regions = codigo_region %in% c("04","05","06","07","08","09","14","13","16")) %>%
  ggplot()+geom_sf(aes(fill=fill_regions,col=fill_regions))+
  scale_fill_manual(values=c("grey50","red3"))+
  scale_color_manual(values=c("grey50","red3"))+
  theme_minimal()+
  xlim(c(-76,-66))+
  theme(legend.position = "none",
        axis.text = element_blank(),panel.grid.major = element_blank(),
        panel.background = element_rect())

png("Day6_Red/chile.png",width = 150,height=800)
print(plot2)
dev.off()

library(patchwork)

textcol <- "red3"

plot1 + inset_element(png::readPNG("Day6_Red/chile.png",native=TRUE), 0.64, 0.01, 0.9, 0.48) +
  plot_annotation(
  title = 'Forest fires in Chile',
  subtitle = 'Registered between July 2018 and \nJune 2019 in the central regions',
  caption = 'Data: CONAF Chile'
) &
  theme(plot.title = element_text(family = "sans", face = "bold", size = 16, colour = textcol),
        plot.subtitle = element_text(family = "sans" ,size=12, colour = textcol),
        plot.caption = element_text(family = "sans" ,size=11, colour = textcol))
