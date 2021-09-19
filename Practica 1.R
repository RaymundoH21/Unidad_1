# Hirales Lazareno Raymundo - 17212339
# Galaviz Lona Oscar Eduardo - 17212993
#
counter <- 0
#
randNum <- 1000000
#
for(i in rnorm(randNum)){
  if(i > -1& i<1){
    counter <- counter + 1
  }
}
#
answer<-(counter / randNum) * 100
print(answer)