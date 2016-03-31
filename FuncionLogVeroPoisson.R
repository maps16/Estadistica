FuncionLogVeroPoisson = function(vecpar, datos){
  Lambda = vecpar[1]
  S =  sum(datos)
  n = length(datos)
  Constante = prod(factorial(datos))
  LVero = log( (Lambda^S)*exp(-n*Lambda)/Constante, exp(1))
  return(LVero)
}