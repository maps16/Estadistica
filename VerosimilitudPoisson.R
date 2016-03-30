#==================================================================================
# Verosimilitud Caso Poisson
#==================================================================================
#Data
n = 10
lambda0 = 5
Xobs = rpois(n, lambda0)
Xobs

#Funcion Utilizadas
source("FuncionVeroPoisson.R", local = FALSE)

#Grafica
ValLambda = seq(1,10,0.01)
N = length(ValLambda)
Verosimilitud = c()
for(i in 1:N){
  Verosimilitud[i] = FuncionVeroPoisson(ValLambda[i], Xobs)
}
plot(ValLambda, Verosimilitud, type = "l", lty =1, lwd=2, col=1, cex.lab=1.35, cex.axis=1.25,
     xlab = expression(lambda), ylab = "Verosimilitud",
     main = paste("Datos simulados Poisson: n =",n," y Lambda = ",lambda0))
segments(lambda0, 0,lambda0, FuncionVeroPoisson(lambda0,Xobs), lty = 1, lwd = 2, col = 4)

