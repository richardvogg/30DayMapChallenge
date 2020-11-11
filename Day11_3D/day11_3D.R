library(rayshader)
library(leaflet)
library(raster) #load .hgt file

bbox <- osmdata::getbb("Alpamayo, Peru")

peru <- raster("C:/Richard/R and Python/Datasets/Peru Elevation/S09W078.hgt")

#bigger area
bbox <- extent(-77.72,-77.59,-8.950,-8.810)

#smaller area
bbox <- extent(-77.680,-77.630,-8.910,-8.850)


alpamayo <- crop(peru, bbox)
plot(alpamayo)

elmat <- raster_to_matrix(alpamayo)


elmat %>%
  sphere_shade(texture = "desert",sunangle = 270) %>%
  add_shadow(ray_shade(elmat, zscale = 30), 0.5) %>%
  plot_3d(elmat, zscale = 30, fov = 0, theta = 45, zoom = 0.75, phi = 20, windowsize = c(1000, 800),
          baseshape = "circle")
Sys.sleep(0.2)
render_snapshot("Day11_3D/plot.png")


#### Animation as GIF

zscale <- 30
elev_matrix <- elmat
n_frames <- 90
# frame transition variables
thetavalues <- seq(0, 356,by = 360/90)
# shadow layers
ambmat <- ambient_shade(elev_matrix, zscale = zscale)
raymat <- ray_shade(elev_matrix, zscale = zscale, lambert = TRUE)

# generate .png frame images
img_frames <- paste0("drain", seq_len(n_frames), ".png")
for (i in seq_len(n_frames)) {
  message(paste(" - image", i, "of", n_frames))
  elev_matrix %>%
  sphere_shade(texture = "desert",sunangle = 270) %>%
    add_shadow(ambmat, 0.5) %>%
    add_shadow(raymat, 0.5) %>%
    plot_3d(elmat, zscale = 30, fov = 0, theta = thetavalues[i], 
            zoom = 0.75, phi = 20, windowsize = c(1000, 800),
            baseshape = "circle")
  render_snapshot(img_frames[i])
  rgl::clear3d()
}

# build gif
magick::image_write_gif(magick::image_read(img_frames), 
                        path = "alpamayo1.gif", 
                        delay = 12/n_frames)
