# Hirales Lazareno Raymundo - 17212339
# Galaviz Lona Oscar Eduardo - 17212993
#
#1 suma
suma <- function(x, y) {
  resultado <- x + y
  return(resultado)
}
suma (x=4, y=8)
#
#2 numeros aleatorios
fun1 <- function() {
  num <- runif(1)
  veces <- 1
  while (sum(num) < 3) {
    veces <- veces + 1
    num[veces] <- runif(1)
  }
  return(veces)
}

fun1()  # primera prueba
#
#3 punto medio
medio <- function(a, b) {
  medio <- (a + b) / 2
  cat("El punto medio de los valores", a, "y", b,
      "ingresados es", medio)
}

medio(a=-3, b=-1)  # Probando la función
#
#4 temperatura
fahr_to_kelvin<- function(temp){
  kelvin <-((temp-32)*(5/9)) +273.15
  return(kelvin)
}
fahr_to_kelvin(temp = 15)
#
#5 stop if
fahr_to_kelvin <- function(temp) {
  if (!is.numeric(temp)) {
    stop("temp must be a numeric vector.")
  }
  kelvin <- ((temp - 32) * (5 / 9)) + 273.15
  return(kelvin)
}
fahr_to_kelvin(temp =120)
#
#6 suma progresiva
an <- function(a1, r, n){
  a1 * r ** (n - 1)
}
an(a1 = 1, r = 2, n = 5)  # 16
an(a1 = 4, r = -2, n = 6) # -128
#
#7 funcion return
asn <- function(a1 = 1, r = 2, n = 5) {
  A  <- an(a1, r, n)
  S  <- sn(a1, r, n)
  ii <- 1:n
  AA <- an(a1, r, ii)
  SS <- sn(a1, r, ii)
  return(list(an = A, sn = S,
              salida = data.frame(valores = AA,
                                  sum = SS)))
}
asn(2)
#
#8 variables locales y globales
fun <- function() {
  print(x)
}

x <- 1

fun() # 1

x <- 1
y <- 3
fun3 <- function() {
  x <- 2
  y <<- 5
  print(paste(x, y))
}

fun3() # 2 5
x # 1 (el valor no cambió)
y # 5 (el valor cambió)
#
#9 Porcentaje
DNI <- function(numero) {
letras <- c("T", "R", "W", "A", "G", "M", "Y", "F", "P", "D", "X", "B",
            "N", "J", "Z", "S", "Q", "V", "H", "L", "C", "K", "E")
letras <- letras[numero %% 23 + 1]
return(letras)
}
DNI(50247828) # G
#
#10 probabilidad con graficas
dado <- function(n = 100){
  lanzamientos <- sample(1:6, n, rep = T)
  frecuencias <- table(lanzamientos)/n
  barplot(frecuencias, main = "")
  abline(h = 1/6, col = "red", lwd = 2)
  return(frecuencias)
}
par(mfcol = c(1, 3))

dado(100)
dado(500)
dado(100000)
#
#11 rnorm numero aleatorios
rnorm(3:15, 10)
#
#12 slappy
cuadrado.raro <- function(x) if(x < 5) x^2 else -x^2
sapply(1:10, cuadrado.raro)
#
#13 raiz cuadrada
sqrt(1:10)
#
#14 reduce
Reduce(function(a, b) a + b, 1:10)
#
#15 filtro
x <- 1:20
x[x %% 3 == 0]
Filter(function(i) i %%3 == 0, x)
#
#16 concatenacion
x <- c(1,2,3)
t <- c("uno","dos","tres")
x; t
#
#17 CAT 
x <- 2
y <- 4
cat(x,"elevado a",y,"es",x ^ y,"\n")
#
#18 Invisible
suma <- function(a, b) {
  s <- a + b
  return(invisible(s))
}

suma(5,4)
x <- suma(5,4)
x
#
#19 Trunc elimina decimales
x <- 56.13
trunc(x)
#
#20 Round redondeo
round(6.78,1)
