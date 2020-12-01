library(leaflet)

leaflet() %>%
  addProviderTiles(provider = providers$Stamen.Watercolor) %>%
  addRectangles(-160,-60,160,80) %>%
  addRectangles(-25,-35,65,70,label="Europe/Africa: 22",
                labelOptions = labelOptions(noHide = T,direction="right")) %>%
  addRectangles(-82,-58,-30,15,label="South America: 14",
                labelOptions = labelOptions(noHide = T,direction="right")) %>%
  addRectangles(-26,35,37.5,70) %>%
  addRectangles(-75.64, -55.61, -66.96, -17.58,label="Chile: 1,2,3,4,5,6,9,29",
                labelOptions = labelOptions(noHide = T,direction="right")) %>%
  addRectangles(5.99, 47.30, 15.02, 54.98,label="Germany: 7,10,13,19,25,27",
                labelOptions = labelOptions(noHide = T,direction="right")) %>%
  addRectangles(-81.41, -18.35, -68.66, -0.057,label="Peru: 11",
                labelOptions = labelOptions(noHide = T,direction="right")) %>%
  addRectangles(-16.9,27.9,-16.1,28.7,label="Tenerife: 16,24",
                labelOptions = labelOptions(noHide = T,direction="left")) %>%
  addRectangles(58,44,61,47,label="Aral Sea: 21",
                labelOptions = labelOptions(noHide = T,direction="right")) %>%
  addLabelOnlyMarkers(80,-50,label="World: 8,12,15,18,26,30",
                      labelOptions = labelOptions(noHide = T,direction="right")) %>%
  addLabelOnlyMarkers(5,70,label="Europe: 17,20,23,29",
                      labelOptions = labelOptions(noHide = T,direction="top"))
  
  
  
#Chile: 1,2,3,4,5,6,9,29
#Germany: 7,10,13,19,25,27
#SouthAmerica: 14
#World: 8,12,15,18,26
#Peru: 11
#Tenerife: 16,24
#Europe: 17,20,23,29
#Aral Sea: 21
#Europe+Africa: 22
