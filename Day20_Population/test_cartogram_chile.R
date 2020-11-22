library(chilemapas)
library(cartogram)
library(dplyr)
library(sf)
library(ggplot2)

poblacion <- chilemapas::censo_2017_comunas %>%
  group_by(codigo_provincia=substr(codigo_comuna,1,3)) %>%
  summarise(poblacion=sum(poblacion)) %>%
  ungroup()



chile <- chilemapas::mapa_comunas %>% 
  filter(!codigo_comuna %in% c("05201","05104")) %>%
  generar_provincias() %>%
  left_join(poblacion,by="codigo_provincia") %>%
  st_as_sf(sf_column_name="geometry") %>%
  st_transform("+init=epsg:3395") %>%
  st_simplify(dTolerance=500)

chile_cont <- cartogram_cont(chile,"poblacion",itermax=5)

chile_2 <- ggplot(chile_cont)+geom_sf(aes(fill=poblacion))
chile <- ggplot(chile)+geom_sf(aes(fill=poblacion))

library(tmap)

tm_shape(chile_cont) + tm_polygons("poblacion", style = "jenks")

library(patchwork)
options(scipen=10)

chile+chile_2+plot_layout(guides="collect")
