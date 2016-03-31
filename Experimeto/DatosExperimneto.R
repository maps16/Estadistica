
#Librerias utilizadas
library (gdata)
#Leyendo los datos obtenidos
datos=read.table("Data.txt")
datos
summary(datos)
var(datos)
d=datos$V1
hist(d)
