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
source("FuncionLogVeroPoisson.R", local = FALSE)
source("FuncionMenosLogVeroPoisson.R", local = FALSE)

#Grafica VeroSimilitud
ValLambda = seq(1,10,0.01)
N = length(ValLambda)
Verosimilitud = c()
for(i in 1:N){
  Verosimilitud[i] = FuncionVeroPoisson(ValLambda[i], Xobs)
}


#Grafica LogVeroSimilitud
ValLambda = seq(1,10,0.01)
N = length(ValLambda)
LogVerosimilitud = c()
for(i in 1:N){
  LogVerosimilitud[i] = FuncionLogVeroPoisson(ValLambda[i], Xobs)
}


#Grafica MenosLogVeroSimilitud
ValLambda = seq(1,10,0.01)
N = length(ValLambda)
MenosLogVerosimilitud = c()
for(i in 1:N){
  MenosLogVerosimilitud[i] = FuncionMenosLogVeroPoisson(ValLambda[i], Xobs)
}



..


