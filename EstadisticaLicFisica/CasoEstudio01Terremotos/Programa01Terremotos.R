#Datos: http://earthquake.usgs.gov/earthquakes/feed/v1.0/csv.php
DatosTerremotos<-read.csv("all_month.csv")
DatosTerremotos[1,]
Xobs<-DatosTerremotos$mag
hist(Xobs)
