library(MASS)
library(marg)

#DATOS
venice
vAno = venice$year
vMar = venice$sea
x = vMar

plot(vAno , vMar , "o" )

n=length(vMar)
sumvMar = sum(vMar)

#EMV
mMar = sumvMar/n
sd2Mar = (1/n)*sum((vMar-mMar)**2)
sdMar = sd2Mar**(1/2)

#MODELO Comparación con histogramas
hist(vMar , freq = FALSE)
curve(dnorm(x, mean = mMar, sd = sdMar), col = "royalblue2", lty = 2, lwd = 2, add = TRUE)

#Grafica de Distribucion
t = seq(0, max(x)+5, 1)
dT = pnorm(t , mMar, sdMar)
dE=ecdf(vMar)
plot(dE , main="Funciones de probabilidad acumuladas")
points(t, dT, type="l",lwd=2,lty=1,col="royalblue4")

#Q-Qplot
valpha<-(seq(1,n,1)-0.5)/n
QTE<-qnorm(valpha,mMar, sdMar)
QE<-sort(x)
plot(QTE,QE,type="p",pch=16,cex=1.25, main = "Q-QPlot")
segments(0,0,200,200,lwd=2,col=4)




