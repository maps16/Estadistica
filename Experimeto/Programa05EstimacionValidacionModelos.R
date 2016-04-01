###############################################################################
#PROGRAMA: ESTIMACION Y VALIDACION DE MODELOS                                 #
#MONTOYA: 2016-03-07                                                          #
###############################################################################

################################################################
#VARIABLE ALEATORIA DISCRETA: CASO POISSON                     #
################################################################
#DATOS SIMULADOS
n<-100
lambda0<-5
Xobs<-rpois(n,lambda0)
Xobs

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

################################################################
#VARIABLE ALEATORIA DISCRETA: CASO BINOMIAL                    #
################################################################
#DATOS SIMULADOS
N<-10
p<-0.5
k<-100
Xobs<-rbinom(k,size=N,prob=p)
Xobs

#ESTADISTICA DESCRIPTIVA
summary(Xobs)

#GRAFICA DE LOS DATOS: FRECUENCIA ABSOLUTA
ExtremoIntervalos<-seq(-0.5,N+1,1)
TablaFrecuencia<-table(cut(Xobs,ExtremoIntervalos))
ValoresX<-seq(0,N,1)
names(TablaFrecuencia)<-ValoresX
barplot(TablaFrecuencia,xlab="X",ylab="Frecuencia absoluta")

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


################################################################
#VARIABLE ALEATORIA CONTINUA: CASO NORMAL                      #
################################################################
#DATOS SIMULADOS
n<-100
mu<-20
sigma<-2
Xobs<-rnorm(n,mean=mu,sd=sigma)
Xobs

#ESTADISTICA DESCRIPTIVA
summary(Xobs)

#GRAFICA DE LOS DATOS: FRECUENCIA ABSOLUTA
hist(Xobs,breaks=10,xlab="X",ylab="Frecuencia",main="")

#ESTIMADOR DE MAXIMA VEROSIMILITUD
EMVmu<-mean(Xobs)
EMVsigma<-sqrt(sum((Xobs-EMVmu)^2)/n)

#GRAFICA: DENSIDAD DE PROBABILIDAD VERSUS LA ESTIMADA
x<-Xobs
hist(x,breaks=10,xlab="X",ylab="Frecuencia",main="",
     xlim=c(EMVmu-3.5*EMVsigma,EMVmu+3.5*EMVsigma),freq = FALSE)
curve(dnorm(x,mean=EMVmu,sd=EMVsigma), col = 2, lty = 2, lwd = 2, add = TRUE)

#GRAFICA: DISTRIBUCION EMPIRICA VERSUS ESTIMADA
ValoresX<-seq(0,max(Xobs)+5,0.1)
Festimada<-pnorm(ValoresX,mean=EMVmu,sd=EMVsigma)
Fempirica<-ecdf(Xobs)
plot(Fempirica,main="Funcion de probabilidad acumulada")
points(ValoresX,Festimada,type="l",lwd=2,lty=1,col=4)

#GRAFICA: CUANTIL EMPIRICO VERSUS TEORICO ESTIMADO
valpha<-(seq(1,n,1)-0.5)/n
QTE<-qnorm(valpha,mean=EMVmu,sd=EMVsigma)
QE<-sort(Xobs)
plot(QTE,QE,type="p",pch=19,cex=1,xlab="Cuantil teorico estimado",ylab="Cuantil empirico",
     main="Garfica cuantil-cuantil")
segments(0,0,100,100,lwd=2,col=4)


################################################################
#VARIABLE ALEATORIA CONTINUA: WEIBULL                          #
################################################################
#DATOS SIMULADOS
n<-100
ParForma<-2
ParEscala<-4
Xobs<-rweibull(n, shape=ParForma, scale=ParEscala)
Xobs

#ESTADISTICA DESCRIPTIVA
summary(Xobs)

#GRAFICA DE LOS DATOS: FRECUENCIA ABSOLUTA
hist(Xobs,breaks=10,xlab="X",ylab="Frecuencia",main="")

#ESTIMADOR DE MAXIMA VEROSIMILITUD
library(MASS)
OptEMV<-fitdistr(Xobs,"weibull")
EMVForma<-as.numeric(OptEMV$estimate[1])
EMVEscala<-as.numeric(OptEMV$estimate[2])

#GRAFICA: DENSIDAD DE PROBABILIDAD VERSUS LA ESTIMADA
x<-Xobs
hist(x,breaks=10,xlab="X",ylab="Frecuencia",main="",freq = FALSE)
curve(dweibull(x,shape=EMVForma, scale=EMVEscala), col = 2, lty = 2, lwd = 2, add = TRUE)

#GRAFICA: DISTRIBUCION EMPIRICA VERSUS ESTIMADA
ValoresX<-seq(0,max(Xobs)+5,0.1)
Festimada<-pweibull(ValoresX,shape=EMVForma, scale=EMVEscala)
Fempirica<-ecdf(Xobs)
plot(Fempirica,main="Funcion de probabilidad acumulada")
points(ValoresX,Festimada,type="l",lwd=2,lty=1,col=4)

#GRAFICA: CUANTIL EMPIRICO VERSUS TEORICO ESTIMADO
valpha<-(seq(1,n,1)-0.5)/n
QTE<-qweibull(valpha,shape=EMVForma, scale=EMVEscala)
QE<-sort(Xobs)
plot(QTE,QE,type="p",pch=19,cex=1,xlab="Cuantil teorico estimado",ylab="Cuantil empirico",
     main="Garfica cuantil-cuantil")
segments(0,0,100,100,lwd=2,col=4)


################################################################
#VARIABLE ALEATORIA CONTINUA: LOGNORMAL                        #
################################################################
#DATOS SIMULADOS
n<-100
ParLocalizacion<-1.6
ParEscala<-0.5
Xobs<-rlnorm(n, meanlog=ParLocalizacion, sdlog=ParEscala)
Xobs

#ESTADISTICA DESCRIPTIVA
summary(Xobs)

#GRAFICA DE LOS DATOS: FRECUENCIA ABSOLUTA
hist(Xobs,breaks=10,xlab="X",ylab="Frecuencia",main="")

#ESTIMADOR DE MAXIMA VEROSIMILITUD
library(MASS)
OptEMV<-fitdistr(Xobs,"log-normal")
EMVLocalizacion<-as.numeric(OptEMV$estimate[1])
EMVEscala<-as.numeric(OptEMV$estimate[2])

#GRAFICA: DENSIDAD DE PROBABILIDAD VERSUS LA ESTIMADA
x<-Xobs
hist(x,breaks=10,xlab="X",ylab="Frecuencia",main="",freq = FALSE)
curve(dlnorm(x,meanlog=EMVLocalizacion, sdlog=EMVEscala), col = 2, lty = 2, lwd = 2, add = TRUE)

#GRAFICA: DISTRIBUCION EMPIRICA VERSUS ESTIMADA
ValoresX<-seq(0,max(Xobs)+5,0.1)
Festimada<-plnorm(ValoresX,meanlog=EMVLocalizacion, sdlog=EMVEscala)
Fempirica<-ecdf(Xobs)
plot(Fempirica,main="Funcion de probabilidad acumulada")
points(ValoresX,Festimada,type="l",lwd=2,lty=1,col=4)

#GRAFICA: CUANTIL EMPIRICO VERSUS TEORICO ESTIMADO
valpha<-(seq(1,n,1)-0.5)/n
QTE<-qlnorm(valpha,meanlog=EMVLocalizacion, sdlog=EMVEscala)
QE<-sort(Xobs)
plot(QTE,QE,type="p",pch=19,cex=1,xlab="Cuantil teorico estimado",ylab="Cuantil empirico",
     main="Garfica cuantil-cuantil")
segments(0,0,100,100,lwd=2,col=4)


################################################################
#VARIABLE ALEATORIA CONTINUA: DISTRIBUCION DE VALORES EXTREMOS #
################################################################
#DATOS SIMULADOS
library(extRemes)
n<-100
ParLocalizacion<-20
ParEscala<-0.5
ParForma<-0.2
Xobs<-revd(n, loc=ParLocalizacion, scale=ParEscala, shape=ParForma)
Xobs

#ESTADISTICA DESCRIPTIVA
summary(Xobs)

#GRAFICA DE LOS DATOS: FRECUENCIA ABSOLUTA
hist(Xobs,breaks=10,xlab="X",ylab="Frecuencia",main="")

#ESTIMADOR DE MAXIMA VEROSIMILITUD
OptEMV<-fevd(Xobs)
EMVLocalizacion<-as.numeric(OptEMV$results$par[1])
EMVEscala<-as.numeric(OptEMV$results$par[2])
EMVForma<-as.numeric(OptEMV$results$par[3])

#GRAFICA: DENSIDAD DE PROBABILIDAD VERSUS LA ESTIMADA
x<-Xobs
hist(x,breaks=10,xlab="X",ylab="Frecuencia",main="",freq = FALSE)
curve(devd(x, loc=EMVLocalizacion, scale=EMVEscala, shape=EMVForma), col = 2, lty = 2, lwd = 2, add = TRUE)

#GRAFICA: DISTRIBUCION EMPIRICA VERSUS ESTIMADA
ValoresX<-seq(0,max(Xobs)+5,0.1)
Festimada<-pevd(ValoresX, loc=EMVLocalizacion, scale=EMVEscala, shape=EMVForma)
Fempirica<-ecdf(Xobs)
plot(Fempirica,main="Funcion de probabilidad acumulada")
points(ValoresX,Festimada,type="l",lwd=2,lty=1,col=4)

#GRAFICA: CUANTIL EMPIRICO VERSUS TEORICO ESTIMADO
valpha<-(seq(1,n,1)-0.5)/n
QTE<-qevd(valpha, loc=EMVLocalizacion, scale=EMVEscala, shape=EMVForma)
QE<-sort(Xobs)
plot(QTE,QE,type="p",pch=19,cex=1,xlab="Cuantil teorico estimado",ylab="Cuantil empirico",
     main="Garfica cuantil-cuantil")
segments(0,0,100,100,lwd=2,col=4)


