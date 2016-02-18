###############################################################################
#PROGRAMA: COMPORTAMIENTO DEL ESTIMADOR DE MAXIMA VEROSIMILITUD PARA EL CASO  #
#EXPONENCIAL. SE SIMULAN M DATOS DE UNA EXPONENCIAL CON MEDIA LAMBDA0 Y SE    #
#CALCULA EL EMV DE LAMBDA0 PARA n=1,...,M. LUEGO, SE MUESTRA COMO EL ESTIMADOR#
#SE ACERCA A LA VERDAD CUANDO n CRECE                                         #
#MONTOYA: 2016-02-08                                                          #
###############################################################################

#ESCENARIO DE SIMULACION
Lambda0<-10
M<-10000

#DATOS SIMULADOS
x<-rexp(M,1/Lambda0)

#CLACULO DEL EMV PARA n=1,2,...,M
EMVlambda<-c()
for(i in 1:M)
{
  n<-i
  xobs<-x[1:n]
  EMVlambda[i]<-mean(xobs)
}

plot(seq(1,M,1),EMVlambda,type="b",pch=19,cex=0.5,xlab="n",ylab="Media",cex.lab=1.25,
     main="Comportamiento del EMV: Caso Exponencial",cex.main=1.5)
segments(0,Lambda0,M,Lambda0,lwd=2,col=4,lty=1)




#ESCENARIO DE SIMULACION: DATOS EXPONENCIALES - MODELO EXPONENCIAL
Lambda0<-10
n<-250
#DATOS SIMULADOS
x<-rexp(n,1/Lambda0)
#hist(x)

EMVlambda<-mean(x)
hist(x, freq = FALSE)
curve(dexp(x,1/EMVlambda), col = 2, lty = 2, lwd = 2, add = TRUE)

#ESCENARIO DE SIMULACION: DATOS NORMALES - MODELO EXPONENCIAL
Media0<-2
Sd0<-0.5
n<-100
#DATOS SIMULADOS
x<-rnorm(n,Media0,Sd0)
hist(x)

EMVlambda<-mean(x)
hist(x, freq = FALSE)
curve(dexp(x,1/EMVlambda), col = 2, lty = 2, lwd = 2, add = TRUE)

