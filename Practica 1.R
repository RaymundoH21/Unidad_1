# Hirales Lazareno Raymundo - 17212339
# Galaviz Lona Oscar Eduardo - 17212993
#
# Se inicializa el contador
counter <- 0
#
# Se establece el maximo numero de veces que se repetira el numero random
randNum <- 1000000
#
# Se inicia el ciclo
for(i in rnorm(randNum)){
# Condicional si los numeros que aparecen esta entre el -1 y el 1 el contador aumentara
  if(i > -1& i<1){
    counter <- counter + 1
  }
}
#
# El contador sera dividido entre el numero que establecimos como el maximo numero de 
# veces que se repetira y se multiplicara por 100
answer<-(counter / randNum) * 100
# Se imprime el resultado en porcentaje de veces que los numeros estuvieron entre -1 y 1
print(answer)