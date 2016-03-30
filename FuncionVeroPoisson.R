FuncionVeroPoisson = function(vecpar, datos){
  Lambda = vecpar[1]
  S =  sum(datos)
  n = length(datos)
  Constante = prod(factorial(datos))
  Vero = (Lambda^S)*exp(-n*Lambda)/Constante
  return(Vero)
}