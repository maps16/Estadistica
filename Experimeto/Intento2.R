################################################################
#VARIABLE ALEATORIA CONTINUA: CASO GEOMETRICA                      #
################################################################
#Librerias utilizadas
library (gdata)
#Leyendo los datos obtenidos
dat=read.table("Data.txt")
#DATOS SIMULADOS
Xobs = dat$V1
Xobs
n=100

#ESTADISTICA DESCRIPTIVA
summary(Xobs)
var(Xobs)
r = var(Xobs)
sd = r**(1/2)
sd

#GRAFICA DE LOS DATOS: FRECUENCIA ABSOLUTA
ExtremoIntervalos<-seq(3.5,8.5,1)
TablaFrecuencia<-table(cut(Xobs,ExtremoIntervalos))
ValoresX<-seq(4,8,1)
names(TablaFrecuencia)<-ValoresX
barplot(TablaFrecuencia,xlab="X",ylab="Frecuencia absoluta")

#ESTIMADOR DE MAXIMA VEROSIMILITUD
EMV =((1/n)*sum(Xobs))**(-1)
EMV

#GRAFICA: DENSIDAD DE PROBABILIDAD VERSUS LA ESTIMADA
FrecuenciaRelativa<-as.numeric(prop.table(TablaFrecuencia))
DensidadProbabilidadEMV<-dgeom(ValoresX,EMV)
plot(ValoresX,FrecuenciaRelativa,pch=19,col=1,xlab="X",ylab="Frecuencia relativa",
     main="Densidad de Probabilidad")
points(ValoresX,DensidadProbabilidadEMV,pch=17,col=4)
legend("topright",c("Observada","Estimada"),pch=c(19,17),col=c(1,4),bty="n")

#GRAFICA: DISTRIBUCION EMPIRICA VERSUS ESTIMADA
Fempirica<-ecdf(Xobs)
DistribucionEMV<-pgeom(ValoresX,EMV)
plot(Fempirica,main="Funcion de probabilidad acumulada")
points(ValoresX,DistribucionEMV,type="p",pch=17,col=4)
legend("topleft",c("Observada","Estimada"),pch=c(19,17),col=c(1,4),bty="n")

#GRAFICA: CUANTIL EMPIRICO VERSUS TEORICO ESTIMADO
valpha<-(seq(1,n,1)-0.5)/n
QTE<-qgeom(valpha,EMV)
QE<-sort(Xobs)
plot(QTE,QE,type="p",pch=19,cex=1,xlab="Cuantil teorico estimado",ylab="Cuantil empirico",
     main="Garfica cuantil-cuantil")
segments(4,4,8,8,lwd=2,col=4)
