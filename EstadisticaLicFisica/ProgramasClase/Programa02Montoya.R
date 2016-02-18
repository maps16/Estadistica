###############################################################################
#PROGRAMA: SIMULACION DE DATOS NORMALES EN UN CONTEXTO FISICO                 #
#SE DISPARA UN PROYECTIL DESDE UNA ALTURA h SOBRE UN PLANO HORIZONTAL CON     #
#VELOCIDAD INICIAL v0, HACIENDO UN ANGULO THETA CON LA HORIZONTAL.            #
#MONTOYA: 2015-02-03                                                          #
###############################################################################

####################COMPORTAMIENTO DETERMINISTA DEL FENOMENO###################
#ESCENARIO DE SIMULACION
v0<-60
h<-200
g<-9.81
vtheta<-45

#TIEMPO DE VUELO
z<-g*h/(v0^2)
w<-sin(vtheta)+sqrt((sin(vtheta))^2+2*z)
vT<-(v0/g)*w

#ALCANCE
vR<-((v0^2)/g)*w*cos(vtheta)

#TRAYECTORIA DEL PROYECTIL
vt<-seq(0,vT,0.1)
x<-v0*cos(vtheta)*vt
y<-h+v0*sin(vtheta)*vt-g*(vt^2)/2
plot(x,y,type="l",lty=1,lwd=1,col=1,ylim=c(0,max(y)+10),
     xlab="X",ylab="Y", main="Trayectoria del proyectil", cex.main=1.5,
     cex.lab=1.25,cex.axis=1.25)
text(vR,0,"*",cex=2,col=2)


####################COMPORTAMIENTO ALEATORIO DEL FENOMENO###################
####################VELOCIDAD SE COMPORTA COMO UNA NORMAL###################
#ESCENARIO DE SIMULACION
MEDIAv0<-60
DESVIv0<-0.75
h<-200
g<-9.81
vtheta<-45
n<-1000

#DATOS SIMULADOS DE LA VELOCIDAD: CASO NORMAL
v0Normal<-rnorm(n,mean=MEDIAv0,sd=DESVIv0)

#TIEMPOS DE VUELO
z<-g*h/(v0Normal^2)
w<-sin(vtheta)+sqrt((sin(vtheta))^2+2*z)
vT<-(v0Normal/g)*w

#ALCANCES
vRnormal<-((v0Normal^2)/g)*w*cos(vtheta)

#TRAYECTORIAS DEL PROYECTIL
vt<-seq(0,max(vT),0.1)
x<-v0*cos(vtheta)*vt
y<-h+v0*sin(vtheta)*vt-g*(vt^2)/2
plot(x,y,type="l",lty=1,lwd=2,col=1,ylim=c(0,max(y)+10),xlim=c(0,max(x)+10),
     xlab="X",ylab="Y", main="Trayectoria del proyectil", cex.main=1.5,
     cex.lab=1.25,cex.axis=1.25)
for(i in 1:n)
{
  vx<-v0Normal[i]*cos(vtheta)*vt
  vy<-h+v0Normal[i]*sin(vtheta)*vt-g*(vt^2)/2
  points(vx,vy,type="l",lty=3,lwd=1,col=4)
}
text(vR,0,"*",cex=2,col=2)

#GRAFICA DE ALCANCES
summary(vRnormal)
sd(vRnormal)
var(vRnormal)
min(vRnormal)
max(vRnormal)

hist(vRnormal)
text(vR,0,"*",cex=2,col=2)

boxplot(vRnormal)

