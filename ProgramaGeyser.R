#EJERCICIO SOBRE ANALISIS DESCRIPTIVO DE DATOS Y AJUSTE DE MODELOS
#MONTOYA 17 DE FEBRERO DEL 2016

#LIBRERIAS
library(MASS)
library(marg)

#######################CURIOSIDAD######################################
#DATOS
geyser
TiempoEntreEventos<-geyser$waiting
DuracionEvento<-geyser$duration

#ESTADISTICA DESCRIPTIVA: HISTOGRAMAS
hist(TiempoEntreEventos)
hist(DuracionEvento)
help(hist)
#######################INICIAMOS######################################

#DATOS
venice


