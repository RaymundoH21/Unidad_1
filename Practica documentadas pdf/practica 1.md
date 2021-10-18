
<p align="center">
    <img alt="Logo" src="https://www.tijuana.tecnm.mx/wp-content/uploads/2021/08/liston-de-logos-oficiales-educacion-tecnm-FEB-2021.jpg" width=850 height=250>
</p>

<H2><p align="Center">TECNOLÓGICO NACIONAL DE MÉXICO</p></H2>

<H2><p align="Center">INSTITUTO TECNOLÓGICO DE TIJUANA</p></H2>

<H2><p align="Center">SUBDIRECCIÓN ACADÉMICA</p></H2>

<H2><p align="Center">DEPARTAMENTO DE SISTEMAS Y COMPUTACIÓN</p></H2>

<H2><p align="Center">NOMBRE DE LOS ALUMNOS: </p></H2>

<H2><p align="Center">RAYMUNDO HIRALES LAZARENO (N. CONTROL: 17212339)</p></H2>

<H2><p align="Center">GALAVIZ LONA OSCAR EDUARDO (N.CONTROL: 17212993)</p></H2>

<H2><p align="Center">Carrera: Ingeniería Informática</p></H2>

<H2><p align="Center">Semestre: 9no </p></H2>

<H2><p align="Center">MATERIA: Minería de datos</p></H2>

<H2><p align="Center">PROFESOR: JOSE CHRISTIAN ROMERO HERNANDEZ</p></H2>

<H2><p align="Center">TRABAJOS: Practica 1</p></H2>

<H2><p align="Center">FECHA: 21/09/21</p></H2>

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

```R
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

```