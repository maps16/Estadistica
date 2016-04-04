FuncionMenosLogVeroPoisson =  function(vecpar, datos){
  Lambda = vecpar[1]
  S =  sum(datos)
  n = length(datos)
  Constante = prod(factorial(datos))
  LMVero = -log( (Lambda^S)*exp(-n*Lambda)/Constante, exp(1))
  return(LMVero)
}