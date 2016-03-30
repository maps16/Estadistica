#Escenario de Simulacion
n = 30
PC = 0.95
media0 = 100
sigma0 = 1
M = 10000

#Calculo de la frecuencia de Cobertura
contSI = 0
contNO = 0
#Calculo de Cuantil
z = qnorm(1-((1-PC)/2),0,1)

for(i in 1:M){
  x = rnorm(n, media0, sigma0)
  A = mean(x)-z*(sigma0/(sqrt(n)))
  B = mean(x)+z*(sigma0/(sqrt(n)))
  
  if(media0>A & media0<B){
    contSI = contSI+1
  }
  else{
    contNO = contNO+1
  }
}

frecCobertura = contSI/M
frecErrorCobertura = contNO/M
frecCobertura
frecErrorCobertura