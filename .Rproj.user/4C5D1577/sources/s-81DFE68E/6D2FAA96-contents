#### COVID cases Germany

library(sf)
library(ggplot2)
library(dplyr)


#Datasource COVID CASES RKI:
#https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/917fc37a709542548cc3be077a786c17_0

cases <- read_sf("C:/Richard/R and Python/Datasets/RKI_Corona_Landkreise-shp/Landkreise.shp")

#Original map
cases %>%
  mutate(cases7_per_bin=cut(cases7_per,breaks=c(-1,0,4,25,50,100,1000),
                     labels=c("0","unter 5","5 bis 25","25 bis 50",
                              "50 bis 100","ab 100 (Hotspot)"))) %>%
  ggplot(aes(fill=cases7_per_bin))+geom_sf()+
  scale_fill_manual(values=c("grey80","grey60",
                             "grey40","gold2","orangered1","red3"),drop=FALSE)

#Create grid

cases <- st_transform(cases, 3857)
initial <- cases
initial$index_target <- 1:nrow(initial)
target <- st_geometry(initial)

grid <- st_make_grid(target,
                     50 * 1000, #kms
                     crs = st_crs(initial),
                     what = "polygons",
                     square = TRUE
)

grid <- st_sf(index = 1:length(lengths(grid)), grid) # Add index

cent_grid <- st_centroid(grid)
cent_merge <- st_join(cent_grid, initial["index_target"], left = F)
grid_new <- inner_join(grid, st_drop_geometry(cent_merge))

Fishgeom <- aggregate(grid_new,
                      by = list(grid_new$index_target),
                      FUN = min,
                      do_union = FALSE
)

# Lets add the df
Fishnet <- left_join(
  Fishgeom %>% select(index_target),
  st_drop_geometry(initial)
) %>%
  select(-index_target)


cities <- data.frame(
  city=c("Berlin","Munich","Hamburg","Cologne"),
  lat=c(1492232.65,1288648.23,1112491.59,774995.05),
  lon=c(6894701.26,6129702.78,7085591.67,6609555.67)
  )



png("Day10_Grid/plot.png",width=600,height=700)

Fishnet %>%
  mutate(cases7_per_bin=cut(cases7_per,breaks=c(-1,0,4,25,50,100,1000),
                            labels=c("0","under 5","5 to 25","25 to 50",
                                     "50 to 100","over 100 (Hotspot)"))) %>%
  ggplot()+
  geom_sf(aes(fill=cases7_per_bin))+
  geom_point(data=cities,aes(x=lat,y=lon))+
  ggrepel::geom_label_repel(data=cities,aes(x=lat,y=lon,label=city),fill = alpha("white",0.5))+
  scale_fill_manual(values=c("grey80","grey60",
                             "grey40","gold2","orangered1","red3"),drop=FALSE)+
  labs(title="7 day COVID incidence values per 100,000 inhabitants",subtitle="Germany, Nov 10, 2020",
       fill="7-day-incidence",caption="Data: NPGEO Corona (RKI)")+
  theme_void()

dev.off()

#Test

library(tmap) # for World dataset
library(dplyr)
library(sf)
library(ggplot2)

data("World")
Europe <- World %>% filter(continent=="Europe")

Europe <- st_transform(Europe, 3857)

initial <- Europe
initial$index_target <- 1:nrow(initial)
target <- st_geometry(initial)

grid <- st_make_grid(target,
                     100 * 1000,
                     # Kms
                     crs = st_crs(initial),
                     what = "polygons",
                     square = TRUE
) %>%
  st_crop(xmin = -2800000, xmax= 3500000,ymin = 3900000, ymax = 11500000)

grid <- st_sf(index = 1:length(lengths(grid)), grid) # Add index

cent_grid <- st_centroid(grid)
cent_merge <- st_join(cent_grid, initial["index_target"], left = F)
grid_new <- inner_join(grid, st_drop_geometry(cent_merge))

Fishgeom <- aggregate(grid_new,
                      by = list(grid_new$index_target),
                      FUN = min,
                      do_union = FALSE
)

# Lets add the df
Fishnet <- left_join(
  Fishgeom %>% select(index_target),
  st_drop_geometry(initial)
) %>%
  select(-index_target)


ggplot(Fishnet) + geom_sf(aes(fill=name)) +
  xlim(c(-2500000,3500000))+ylim(c(3900000,11500000)) + 
  theme_void()

