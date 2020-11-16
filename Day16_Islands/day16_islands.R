library(leaflet)

# Check all leaflet providers here: https://leaflet-extras.github.io/leaflet-providers/preview/

leaflet() %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  setView(-16.5,28.3,zoom=10)

