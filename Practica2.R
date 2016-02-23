#Libreria a utilizar
library(ocedata)

#Cargar datos
data(buoy)
#Extraer datos
wh = buoy$height
x = wh[!is.na(wh)] #Eliminando los NA

n = length(x)
sumx = sum(x)

#Calculabdo EMV
mx = sumx/n
sd2x = (1/n)*sum((x-mx)**2)
sdx = sd2x**(1/2)

#Histograma vs Modelo
hist(x, freq = FALSE)
curve(dnorm(x, mean = mx, sd = sdx), col = "royalblue2", lty = 2, lwd = 2, add = TRUE)

#Grafica de Distribucion
t = seq(0, max(x)+5, 1)
dT = pnorm(t , mx, sdx)
dE=ecdf(x)
plot(dE , main="Funciones de probabilidad acumuladas")
points(t, dT, type="l",lwd=2,lty=1,col="royalblue4")

#Q-Qplot
valpha<-(seq(1,n,1)-0.5)/n
QTE<-qnorm(valpha,mx, sdx)
QE<-sort(x)
plot(QTE,QE,type="p",pch=16,cex=1.0, main = "Q-QPlot")
segments(0,0,200,200,lwd=2,col=4)

