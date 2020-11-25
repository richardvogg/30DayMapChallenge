library(sf)
library(rayshader)
library(elevatr)
library(raster)

bbox <- osmdata::getbb("Tenerife")

xmin <- bbox[1,1]
xmax <- bbox[1,2]
ymin <- bbox[2,1]
ymax <- bbox[2,2]
p1 <- rbind(c(xmin,ymin),c(xmin,ymax),c(xmax,ymax),c(xmax,ymin),c(xmin,ymin))

tenerife <- st_polygon(list(p1)) %>%
  st_sfc(crs=4326)


a <- get_elev_raster(as_Spatial(tenerife), z = 9, clip = "locations")
r3 <- focal(a, w= matrix(1,3,3), mean)

elmat = raster_to_matrix(r3)

elmat[elmat<0] <- -1

elmat %>%
  sphere_shade(texture = "desert",sunangle = 180) %>%
  add_water(detect_water(elmat,cutoff = 0,max_height=50), color = "desert") %>%
  #add_shadow(ray_shade(elmat,sunaltitude = 20,sunangle = 150,zscale=20)) %>%
  plot_3d(elmat, zscale = 30, fov = 0, theta = 45, zoom = 0.75, phi = 45, windowsize = c(1000, 800))

  
  Sys.sleep(0.2)
render_snapshot("Day24_Elevation/plot.png",title_text="Tenerife")
