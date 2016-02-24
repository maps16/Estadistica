#Libreria a utilizar
library(ocedata)

#Base de datos
data(soi)

time = soi$year
x = soi$index

plot(time, x, type = "l"  ,col = "deepskyblue")

#Calculo de EMV
n = length(x)
sumIx = sum(x)
  #CASO NORMAL
mIx = sumIx/n
sdI2x = (1/n)*sum((x-mIx)**2)
sdIx = sdI2x**(1/2)

#Histograma vs Modelo
hist(x, freq = FALSE)
curve(dnorm(x, mean = mIx, sd = sdIx), col = "royalblue2", lty = 2, lwd = 2, add = TRUE)

#Grafica de Distribucion
j = seq(min(x)-5, max(x)+5, 1)
dj = pnorm(j , mIx, sdIx)
dJ=ecdf(x)
plot(dJ , col = "deepskyblue" , main="Funciones de probabilidad acumuladas")
points(j, dj, type="l",lwd=2,lty=1,col="black")

#Q-Qplot
valpha<-(seq(1,n,1)-0.5)/n
QTE<-qnorm(valpha,mIx, sdIx)
QE<-sort(x)
plot(QTE,QE,type="p",pch=16,cex=1.0, col="deepskyblue", main = "Q-QPlot")
segments(-200,-200,200,200,lwd=2,col="black")




