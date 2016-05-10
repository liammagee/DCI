# Testing with SA4 data
require("rgdal") # requires sp, will use proj.4 if installed
require("maptools")
require("ggplot2")
require("plyr")
require("rgeos")
require("plotly")
require("dplyr")

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
  coord_equal(xlim=c(110,155)) +
  scale_fill_hue(c=45, l=80)

#To add plotting points, merge data with aus@data$SA4_NAME11

sa4s = read.csv("data/1270055006_CG_POSTCODE_2011_SA4_2011.csv")
sa4s = sa4s[5:3115, 1:6]
names = c("POSTCODE", 
          "POSTCODE", 
          "SA4_CODE_2011", 
          "SA4_NAME_2011", 
          "RATIO", 
          "PERCENTAGE")
names(sa4s) = names
sa4s = sa4s[3:3107,]

sa4s = 
  sa4s[,2:5] %>%
  mutate( RATIO = as.numeric(RATIO),
          POSTCODE = as.numeric(POSTCODE) ) %>%
  filter( RATIO > 0.5 ) %>%
  .[,c(1, 3)]

# Original survey data needs SA4 column from postcodes
DCI_DATA = 
  read.csv("data/DCI_DATA.csv") %>%
  rename( POSTCODE = Q2_197_OTHER ) %>%
  
data = left_join( DCI_DATA, sa4s )

# This says no one in Sydney was surveyed.
View(table(data$SA4_NAME_2011))

# This says 1487 (66%) is not in an SA4 area
table(is.na(data$SA4_NAME_2011))

#Manually check
View(sa4s)
View(data[, c("POSTCODE", "SA4_NAME_2011")])

