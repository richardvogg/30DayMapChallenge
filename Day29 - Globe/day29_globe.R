library(webglobe)
library(ggplot2)
library(dplyr)

sweden <- map_data(map="world",region="Sweden")
max(sweden$lat)
median(sweden$long)


chile <- map_data(map = "world",region="Chile")

chile_shift <- chile %>%
  mutate(lat=lat+(max(sweden$lat)-max(chile$lat)),
         long=long+(median(sweden$long)-median(chile$long)-5))


webglobe(immediate=FALSE) + 
  wgpolygondf(chile_shift,fill="purple",stroke_width = 2) +
  wgcamcenter(50, 20, 8000)                    #Move camera
