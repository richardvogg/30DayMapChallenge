library(dplyr)
library(ggplot2)
library(raster)


aral73 <- brick("C:/Richard/R and Python/Datasets/northaral_mss_09aug73_321_geo.tif")
aral89 <- brick("C:/Richard/R and Python/Datasets/northaral_l4_17sep89_432_geo.tif")
aral00 <- brick("C:/Richard/R and Python/Datasets/northaral_l7_29jul00_432_geo.tif")

crs(aral73) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
crs(aral89) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
crs(aral00) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

aral73 <- aral73 %>% crop(extent(225000,330000,5050000,5170000))
aral89 <- aral89 %>% crop(extent(685000,800000,5050000,5170000))
aral00 <- aral00 %>% crop(extent(685000,800000,5050000,5170000))

prepare_df <- function(img) {
  out <- img %>%
    aggregate(fact=4) %>% 
    raster(layer=1) %>%
    as.data.frame(xy=TRUE,na.rm=TRUE)
  
  names(out) <- c("x","y","value")
  
  return(out)
}

aral73df <- prepare_df(aral73)
aral89df <- prepare_df(aral89)
aral00df <- prepare_df(aral00)



plot73 <- aral73df %>%
  ggplot()+geom_raster(aes(x,y,fill=value))+
  scale_fill_gradient(low="blue",high="yellow",guide=FALSE) +
  labs(subtitle = "1973")+
  theme_void()

plot89 <- aral89df %>%
  ggplot()+geom_raster(aes(x,y,fill=value))+
  scale_fill_gradient(low="blue",high="yellow",guide=FALSE) +
  labs(subtitle="1989")+
  theme_void()

plot00 <- aral00df %>%
  ggplot()+geom_raster(aes(x,y,fill=value))+
  scale_fill_gradient(low="blue",high="yellow",guide=FALSE) +
  labs(subtitle="2000")+
  theme_void()

library(patchwork)

sysfonts::font_add_google(name = "Averia Libre","Averia Libre")
showtext::showtext_auto()

plot73 + plot89 + plot00 +
  plot_annotation(title="The Northern Aral Sea",
                  caption="Satellite images from NASA Visible Earth") &
  theme(plot.title = element_text(size=30,family="Averia Libre"),
        plot.subtitle = element_text(size=20,family="Averia Libre"),
        plot.caption = element_text(size=14,family="Averia Libre"))

