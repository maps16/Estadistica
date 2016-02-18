###############################################################################
#PROGRAMA: VALIDACION DE MODELOS                                              #
#MONTOYA: 2016-02-15                                                          #
###############################################################################

#ESCENARIO DE SIMULACION: DATOS EXPONENCIALES - SE AJUSTA MODELO EXPONENCIAL
Lambda0<-10
n<-50
#DATOS SIMULADOS
x<-rexp(n,1/Lambda0)

#DENSIDAD ESTIMADA Y EL HISTOGRAMA
EMVlambda<-mean(x)
hist(x, freq = FALSE)
curve(dexp(x,1/EMVlambda), col = 2, lty = 2, lwd = 2, add = TRUE)

#FUNCION DE DISTRIBUCION ESTIMADA Y LA DISTRIBUCIÓN EMPIRICA
z<-seq(0,max(x)+5,0.1)
Festimada<-pexp(z,1/EMVlambda)
Fempirica<-ecdf(x)
plot(Fempirica,main="Funciones de probabilidad acumuladas")
points(z,Festimada,type="l",lwd=2,lty=1,col=4)



#ESCENARIO DE SIMULACION: DATOS NORMALES - SE AJUSTA MODELO EXPONENCIAL
Media0<-2
Sd0<-0.5
n<-100

#DATOS SIMULADOS
x<-rnorm(n,Media0,Sd0)

#DENSIDAD ESTIMADA Y EL HISTOGRAMA
EMVlambda<-mean(x)
hist(x, freq = FALSE)
curve(dexp(x,1/EMVlambda), col = 2, lty = 2, lwd = 2, add = TRUE)

#FUNCION DE DISTRIBUCION ESTIMADA Y LA DISTRIBUCIÓN EMPIRICA
z<-seq(0,max(x)+5,0.1)
Festimada<-pexp(z,1/EMVlambda)
Fempirica<-ecdf(x)
plot(Fempirica,main="Funciones de probabilidad acumuladas")
points(z,Festimada,type="l",lwd=2,lty=1,col=4)


#ESCENARIO DE SIMULACION: DATOS EXPONENCIALES - SE AJUSTA MODELO EXPONENCIAL
Lambda0<-10
n<-50

#DATOS SIMULADOS
x<-rexp(n,1/Lambda0)

#GRAFICA Q-Q-PLOT
EMVlambda<-mean(x)
valpha<-(seq(1,n,1)-0.5)/n
QTE<-qexp(valpha,1/EMVlambda)
QE<-sort(x)
plot(QTE,QE,type="p",pch=19,cex=1.25,xlim=c(0,60),ylim=c(0,60))
segments(0,0,100,100,lwd=2,col=4)


#ESCENARIO DE SIMULACION: DATOS NORMALES - SE AJUSTA MODELO EXPONENCIAL
Media0<-2
Sd0<-0.5
n<-100

#DATOS SIMULADOS
x<-rnorm(n,Media0,Sd0)

#GRAFICA Q-Q-PLOT
EMVlambda<-mean(x)
valpha<-(seq(1,n,1)-0.5)/n
QTE<-qexp(valpha,1/EMVlambda)
QE<-sort(x)
plot(QTE,QE,type="p",pch=19,cex=1.25)
segments(0,0,100,100,lwd=2,col=4)








#DATOS TERREMOTOS
DatosTerremotos<-read.csv("all_month.csv")
Xobs<-DatosTerremotos$mag
FEmpTERREMOTOS<-ecdf(Xobs)
plot(FEmpTERREMOTOS)




