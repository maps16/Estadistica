###############################################################################
#PROGRAMA: CASO NORMAL                                                        #
#ESTE PROGRAMA PERMITE GRAFICAR LA FUNCION DE DENSIDAD, LA FUNCION DE         #
#DISTRIBUCION Y LA FUNCION CUANTIL DE UNA VARIABLE ALEATORIA NORMAL (X) CON   #
#PARAMETROS MU Y SIGMA, LA MEDIA Y LA VARIANZA, RESPECTIVAMENTE. ADEMAS,      #
#PERMITE SIMULAR UNA MUESTRA DE X DE TAMAÑO ESPECIFICADO                      #
#MONTOYA: 2015-02-03                                                          #
###############################################################################

#ESCENARIO DE ESTUDIO: MEDIA Y DESVIACION DE UNA VARIABLE ALEATORIA NORMAL X
Parmu<-0
ParSigma<-1

#GRAFICA DE LA DENSIDAD
x<-seq(Parmu-5*ParSigma,Parmu+5*ParSigma,0.1)
fx<-dnorm(x, mean = Parmu, sd = ParSigma)
plot(x,fx,type="l",lty=1,lwd=1,col=1,xlim=c(Parmu-5*ParSigma,Parmu+5*ParSigma),
     xlab="Valores de x",ylab="f(x)", main="Densidad Normal", cex.main=1.5,
     cex.lab=1.25,cex.axis=1.25)


#par(mfrow = c(1,2))
#GRAFICA DE LA DISTRIBUCION
x<-seq(Parmu-5*ParSigma,Parmu+5*ParSigma,0.1)
Fx<-pnorm(x, mean = Parmu, sd = ParSigma)
plot(x,Fx,type="l",lty=1,lwd=1,col=1,xlim=c(Parmu-5*ParSigma,Parmu+5*ParSigma),
     xlab="Valores de x",ylab="F(x)", main="Distribucion Normal", cex.main=1.5,
     cex.lab=1.25,cex.axis=1.25)


#GRAFICA DEL CUANTIL
ValAlpha<-seq(0.001,0.999,0.001)
Qalpha<-qnorm(ValAlpha, mean = Parmu, sd = ParSigma)
plot(ValAlpha,Qalpha,type="l",lty=1,lwd=1,col=1,xlim=c(0,1),
     xlab="Probabilidad",ylab="Cuantil", main="Distribucion Normal",cex.main=1.5,
     cex.lab=1.25,cex.axis=1.25)


#SIMULACION DE OBSERVACIONES
n<-1000
x<-rnorm(n, mean = Parmu, sd = ParSigma)
hist(x,freq=FALSE,main="Histograma: Caso Normal",xlab="Valores de x",ylab="Frecuencia")
curve(dnorm(x, mean = Parmu, sd = ParSigma),col=4,add=TRUE)


