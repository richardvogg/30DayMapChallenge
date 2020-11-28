library(webglobe)              #Load the library

wg <- webglobe(immediate=FALSE) #Make a webglobe
#wg + wgpoints(quakes$lat, quakes$lon, size=5*quakes$mag) #Plot quakes
wg + wgcamcenter(-24, 178.0650, 8000)                    #Move camera
