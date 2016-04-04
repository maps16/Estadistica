################################################################
#VARIABLE ALEATORIA DISCRETA: CASO BINOMIAL                    #
################################################################
#DATOS SIMULADOS

#Librerias utilizadas
library (gdata)
#Leyendo los datos obtenidos
dat=read.table("Data.txt")
#DATOS SIMULADOS
Xobs = dat$V1
Xobs
N<-8
p<-0.5
k<-100

#ESTADISTICA DESCRIPTIVA
summary(Xobs)

#GRAFICA DE LOS DATOS: FRECUENCIA ABSOLUTA
ExtremoIntervalos<-seq(3.5,8.5,1)
TablaFrecuencia<-table(cut(Xobs,ExtremoIntervalos))
ValoresX<-seq(4,8,1)
names(TablaFrecuencia)<-ValoresX
barplot(TablaFrecuencia,xlab="X",ylab="Frecuencia absoluta")

#ESTIMADOR DE MAXIMA VEROSIMILITUD
EMV<-sum(Xobs)/(N*k)
EMV

#GRAFICA: DENSIDAD DE PROBABILIDAD VERSUS LA ESTIMADA
FrecuenciaRelativa<-as.numeric(prop.table(TablaFrecuencia))
DensidadProbabilidadEMV<-dbinom(ValoresX,size=N,prob=EMV)
plot(ValoresX,FrecuenciaRelativa,pch=19,col=1,xlab="X",ylab="Frecuencia relativa",
     main="Densidad de Probabilidad")
points(ValoresX,DensidadProbabilidadEMV,pch=17,col=4)
legend("topright",c("Observada","Estimada"),pch=c(19,17),col=c(1,4),bty="n")

#GRAFICA: DISTRIBUCION EMPIRICA VERSUS ESTIMADA
Fempirica<-ecdf(Xobs)
DistribucionEMV<-pbinom(ValoresX,size=N,prob=EMV)
plot(Fempirica,main="Funcion de probabilidad acumulada")
points(ValoresX,DistribucionEMV,type="p",pch=17,col=4)
legend("topleft",c("Observada","Estimada"),pch=c(19,17),col=c(1,4),bty="n")

#GRAFICA: CUANTIL EMPIRICO VERSUS TEORICO ESTIMADO
valpha<-(seq(1,k,1)-0.5)/k
QTE<-qbinom(valpha,size=N,prob=EMV)
QE<-sort(Xobs)
plot(QTE,QE,type="p",pch=19,cex=1,xlab="Cuantil teorico estimado",ylab="Cuantil empirico",
     main="Garfica cuantil-cuantil")
segments(4,4,8,8,lwd=2,col=4)
