# Testing with SA4 data
require("rgdal") # requires sp, will use proj.4 if installed
require("maptools")
require("ggplot2")
require("plyr")
require("rgeos")
require("plotly")

#I upped my memory limit as the file we are going to map is pretty large
memory.limit(6000)

aus = readOGR(dsn="data", layer="SA4_2011_AUST")
aus@data$id = rownames(aus@data)

#aus.buffered = gBuffer(aus, width=0, byid=TRUE)
aus.points = fortify(aus, region="id") #replaced aus.buffered with aus
aus.df = join(aus.points, aus@data, by="id")

graph =
ggplot(aus.df) +
  aes(long,lat,group=group,fill=SA4_NAME11)+
  #Don't want a legend with 150 variables so suppress the legend
  geom_polygon(show_guide = FALSE ) +
  geom_path(color="white") +
  #for some reason it maps too much ocean so limit coords (EDIT: due to Christmas Island)
  coord_equal(xlim=c(110,155))
