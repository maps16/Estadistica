################################################################
#LECTURA DE DATOS                                              #
################################################################
#Librerias utilizadas
library (gdata)
#Leyendo los datos obtenidos
dat=read.table("Data.txt")


#IntenatarPoisson
################################################################
#VARIABLE ALEATORIA DISCRETA: CASO BINOMIAL                    #
################################################################
#DATOS 
Xobs = dat$V1
Xobs
N = length(Xobs)
k = 100
barplot(table(Xobs))


#ESTADISTICA DESCRIPTIVA
summary(Xobs)
var(Xobs)

#GRAFICA DE LOS DATOS: FRECUENCIA ABSOLUTA
ExtremoIntervalos<-seq(-0.5,N+1,1)
TablaFrecuencia<-table(cut(Xobs,ExtremoIntervalos))
ValoresX<-seq(0,N,1)
names(TablaFrecuencia)<-ValoresX
barplot(TablaFrecuencia,xlab="X",ylab="Frecuencia absoluta")
#barplot(TablaFrecuencia,xlab="X",ylab="Frecuencia absoluta", xlim =c(4,11))

#ESTIMADOR DE MAXIMA VEROSIMILITUD
EMV<-sum(Xobs)/(k*N)
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
segments(0,0,100,100,lwd=2,col=4)














#DATOS SIMULADOS
n<-100
lambda0<-5

#ESTADISTICA DESCRIPTIVA
summary(Xobs)

#GRAFICA DE LOS DATOS: FRECUENCIA ABSOLUTA
ExtremoIntervalos<-seq(-0.5,4*lambda0,1)
TablaFrecuencia<-table(cut(Xobs,ExtremoIntervalos))
ValoresX<-seq(0,length(TablaFrecuencia)-1,1)
names(TablaFrecuencia)<-ValoresX
barplot(TablaFrecuencia,xlab="X",ylab="Frecuencia absoluta")

#ESTIMADOR DE MAXIMA VEROSIMILITUD
EMV<-mean(Xobs)
EMV

#GRAFICA: DENSIDAD DE PROBABILIDAD VERSUS LA ESTIMADA
FrecuenciaRelativa<-as.numeric(prop.table(TablaFrecuencia))
DensidadProbabilidadEMV<-dpois(ValoresX,EMV)
plot(ValoresX,FrecuenciaRelativa,pch=19,col=1,xlab="X",ylab="Frecuencia relativa",
     main="Densidad de Probabilidad")
points(ValoresX,DensidadProbabilidadEMV,pch=17,col=4)
legend("topright",c("Observada","Estimada"),pch=c(19,17),col=c(1,4),bty="n")

#GRAFICA: DISTRIBUCION EMPIRICA VERSUS ESTIMADA
Fempirica<-ecdf(Xobs)
DistribucionEMV<-ppois(ValoresX,EMV)
plot(Fempirica,main="Funcion de probabilidad acumulada")
points(ValoresX,DistribucionEMV,type="p",pch=17,col=4)
legend("topleft",c("Observada","Estimada"),pch=c(19,17),col=c(1,4),bty="n")

#GRAFICA: CUANTIL EMPIRICO VERSUS TEORICO ESTIMADO
valpha<-(seq(1,n,1)-0.5)/n
QTE<-qpois(valpha,EMV)
QE<-sort(Xobs)
plot(QTE,QE,type="p",pch=19,cex=1,xlab="Cuantil teorico estimado",ylab="Cuantil empirico",
     main="Garfica cuantil-cuantil")
segments(0,0,100,100,lwd=2,col=4)











