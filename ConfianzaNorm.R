#Escenario
n = 500
mediaNorm = 20
varianza = 0.2
M =10000
valpha = 0.15

#Frecuencia de Cobertura
contSI = 0
contNO = 0

z = qnorm(1 - (valpha/2), 0, 1)

for(i in 1:n){
  x = rnorm(n, mediaNorm,varianza)
  media = mean(x)
  A = media - z*(sd(x)/sqrt(n))
  B = media + z*(sd(x)/sqrt(n))
  
  if(mediaNorm>A & B>mediaNorm){
    contSI = contSI + 1
  }
  else{
    contNO = contNO + 1
  }
}

FrecCo = contSI/n
FrecCo