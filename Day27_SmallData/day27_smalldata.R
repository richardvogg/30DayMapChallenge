# Hefeweizen-Test

library(ggplot2)
library(dplyr)
library(readxl)
library(ggrepel)


df <- read_xlsx("C:/Richard/R and Python/Datasets/Alkoholfreier_Hefe_Test_Nov_2020.xlsx")

library(extrafont)
font_import() # You just have to run this once to import all the fonts present in your system
loadfonts()

ggplot(data=df,aes(x=lon,y=lat))+
  borders(regions="Germany",fill="grey30")+
  geom_text(label="\U0001F37A",family="EmojiOne",size=10,aes(col=Durchschnitt))+
  geom_text_repel(aes(label=paste0(Name," (",Durchschnitt,")")),col="white",min.segment.length = 0,
                  nudge_y = 0.2,family="Malgun Gothic")+
  scale_color_gradient(low="red",high="yellow")+
  labs(title="Blind testing alcohol-free wheat beer",
       subtitles="12 breweries, 6 participants, rating between 1 (lowest) and 9 (highest) per person \n",
       col="Family rating")+
  coord_quickmap()+
  ylim(c(47,51.5))+
  theme_void()+
  theme(panel.background = element_rect(fill="grey60"),
        legend.position=c(0.93,0.5),
        plot.title=element_text(size=20,family="Malgun Gothic"),
        plot.subtitle=element_text(size=12,family="Malgun Gothic"),
        legend.text = element_text(colour="white"),
        legend.title = element_text(colour="white",family="Malgun Gothic"))
