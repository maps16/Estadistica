#Escenario de Simulacion: Normal
n = 30
media0 = 10
sigma0 = 1
M = 20000

#Calculo de la frecuencia de Cobertura

z = c()
for(i in 1:M){
  y = rnorm(n, media0, sigma0)
  z[i] = (sqrt(n)*(mean(y)-media0))/sigma0
}

x = z
hist(x, freq = FALSE, col = blues9)
curve(dnorm(x,0,1),col=2, add=TRUE)






#Escenario de Simulacion: Exp
n = 30
lamda=100
media0 = lamda
sigma0 = lamda
M = 10000

#Calculo de la frecuencia de Cobertura

z = c()
for(i in 1:M){
  y = rexp(n, 1/lamda)
  z[i] = (sqrt(n)*(mean(y)-media0))/sigma0
}

x = z
hist(x, freq = FALSE, col = blues9)
curve(dnorm(x,0,1),col=2, add=TRUE)




