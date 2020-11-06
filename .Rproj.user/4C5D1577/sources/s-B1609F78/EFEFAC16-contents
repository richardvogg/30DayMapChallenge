library(geogrid)
library(tmap)

kreise <- read_sf("C:/Richard/R and Python/Datasets/Kreisgrenzen_2017_mit_Einwohnerzahl/Kreisgrenzen_2017_mit_Einwohnerzahl.shp")


kreise %>% st_drop_geometry() %>% View()
  

unterfranken <- kreise %>%
  mutate(name=paste(ifelse(BEZ=="Kreisfreie Stadt","SK","LK"),GEN)) %>%
  filter(SN_L=='09',SN_R=='6')

unterfranken %>%
  tm_shape() + 
  tm_polygons("EWZ", palette = "viridis") +
  tm_text("name",size=0.7)


new_cells_hex <- calculate_grid(shape = unterfranken, grid_type = "hexagonal", seed = 3)
resulthex <- assign_polygons(unterfranken, new_cells_hex)

tm_shape(resulthex) + 
  tm_polygons("EWZ", palette = "viridis") +
  tm_text("name",size = 0.7)



####################

library(chilemapas) # Comunas and Regions of Chile
library(sf) # Maps
library(ggsflabel)
library(stringdist) # Fuzzy merging

### Load data

df <- read.csv("C:/Richard/R and Python/Datasets/plebiscito_2020_resultados_por_mesa.csv") %>%
  mutate(comuna_low=tolower(comuna))


comunas_list <- codigos_territoriales %>%
  mutate(comuna_low=tolower(nombre_comuna)) %>%
  select(comuna_low,nombre_comuna,codigo_comuna)

### Fuzzy merging

distmatrix <- stringdist::stringdistmatrix(comunas_list$comuna_low,df$comuna_low,method="lv")
best_fit <- apply(distmatrix,2,which.min) %>% as.integer()
similarity <- apply(distmatrix,2,min)

df$best_fit <- comunas_list$codigo_comuna[best_fit]
df$similarity <- similarity

#### Make sure to check if your fuzzy merging did the correct thing

final <- df %>%
  group_by(region,comuna,best_fit) %>%
  summarise(mesas=n_distinct(mesa),
            votantes=sum(votantes),
            apruebo=sum(apruebo),
            rechazo=sum(rechazo)) %>%
  mutate(perc_apruebo=apruebo/(apruebo+rechazo)) %>% 
  left_join(mapa_comunas,by=c("best_fit"="codigo_comuna")) %>%
  left_join(comunas_list,by=c("best_fit"="codigo_comuna")) %>%
  ungroup()

### Create the map

santiago <- final %>%
  filter(region=="METROPOLITANA DE SANTIAGO") %>%
  mutate(nombre_comuna=case_when(
    nombre_comuna=="San Jose de Maipo" ~ "Sn.J. de Maipo",
    nombre_comuna=="Pedro Aguirre Cerda" ~ "P.Ag. Cerda",
    nombre_comuna=="Calera de Tango" ~ "C. de Tango",
    nombre_comuna=="Estacion Central" ~ "Est. Central",
    TRUE ~ nombre_comuna)) %>%
  st_as_sf(sf_column_name = "geometry")



map <- santiago %>%
  ggplot()+geom_sf(aes(fill=perc_apruebo))+
  scale_fill_gradient2(low="red",high="blue",mid = "white",midpoint=0.5)+
  geom_sf_text_repel(aes(label=nombre_comuna),force=10,size=3,segment.size=1)

tm_shape(santiago) + 
  tm_polygons("perc_apruebo",palette="Blues",title="Percentage `Apruebo`") +
  tm_legend(legend.position=c("left","top"))+
  tm_text("nombre_comuna",size = 0.7)+
  tm_layout(panel.labels="Santiago's results of elections for a new constitution")

#Make hexplot

new_cells_hex <- calculate_grid(shape = santiago, grid_type = "hexagonal", seed = 3)
resulthex <- assign_polygons(santiago, new_cells_hex)


hex <- resulthex %>%
  ggplot()+geom_sf(aes(fill=perc_apruebo))+
  scale_fill_gradient2(low="red",high="blue",mid = "white",midpoint=0.5)+
  geom_sf_text(aes(label=nombre_comuna),size=3)

tm_shape(resulthex) + 
  tm_polygons("perc_apruebo",palette="Blues",title="Percentage `Apruebo`") +
  tm_legend(legend.position=c("left","top"))+
  tm_text("nombre_comuna",size = 0.7)+
  tm_layout(panel.labels="Santiago's results of elections for a new constitution")

## save with 700x700 to see the whole names