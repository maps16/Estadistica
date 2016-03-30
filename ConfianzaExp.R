#Escenario
n = 50
vlamda = 20
M =10000
valpha = 0.15

#Frecuencia de Cobertura
contSI = 0
contNO = 0

z = qnorm(1 - (valpha/2), 0, 1)

for(i in 1:n){
  x = rexp(n, 1/vlamda)
  media = mean(x)
  A = media - z*(media/sqrt(n))
  B = media + z*(media/sqrt(n))
  
  if(vlamda>A & B>vlamda){
    contSI = contSI + 1
  }
  else{
    contNO = contNO + 1
  }
}

FrecCo = contSI/n
FrecCo