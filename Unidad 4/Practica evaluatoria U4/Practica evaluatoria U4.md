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

<H2><p align="Center">TRABAJOS: Practica evaluatoria U4</p></H2>

<H2><p align="Center">FECHA: 1/11/21</p></H2>

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>




### Introduccion
Para esta practica se utilizo el modelo kmeas, el modelo es un metodo de agrupamiento que tiene como objetivo la particion de un conjunto de n observaciones en k grupos en el que la observacion pertene al grupo con un valor medio mas cercano, ademas de ser de forma agil y controlada, esta utiliza operaciones estadisticas que para minimizar los errores y mostrar diferentes puntos aleatorios de k llamados centroides y cada punto de k se le asigna una observacion en estos casos busca el centroide con menos errores y por ultimo este modelo nos mostrara los datos de una forma diferentes a otros en este caso en forma de puntos y a diferencia de los demas este busca que tengan menos errores posibles en cada punto

### Desarrollo

```R

dataset = read.csv('iris.csv')
dataset = dataset[1:4]
install.packages("cluster")
library(cluster)
set.seed(101)
irisCluster <- kmeans(dataset[,1:4], center=3, nstart=20)
irisCluster
```
<img alt="Evidence1" src="./../../Unidad 4/Practica evaluatoria U4/IMG/Dataset Acomodado.PNG">
<img alt="Evidence1" src="./../../Unidad 4/Practica evaluatoria U4/IMG/IrisCluster.PNG">
<img alt="Evidence1" src="./../../Unidad 4/Practica evaluatoria U4/IMG/IrisCluster 2.PNG">

```R
library(cluster)
clusplot(iris, irisCluster$cluster, color=T, shade=T, labels=0, lines=0)
```
<img alt="Evidence1" src="./../../Unidad 4/Practica evaluatoria U4/IMG/ClusterPlot(Iris).png">

```R
tot.withinss <- vector(mode="character", length=10)
for (i in 1:10){
  irisCluster <- kmeans(dataset[,1:4], center=i, nstart=20)
  tot.withinss[i] <- irisCluster$tot.withinss
}

plot(1:10, tot.withinss, type="b", pch=19)
```
<img alt="Evidence1" src="./../../Unidad 4/Practica evaluatoria U4/IMG/Plot Grafica.png">

### Conclusion

Esta practica quiza no fue tan larga pero demuestra que tiene cosas que enseñar tratandose de un modelo que nos permite agrupar datos y buscar los puntos con menos errores significa que es un modelo que se preocupa por su exactitud, este modelo funciona para el agrupamiento de datos, asi como identificar los diferentes puntos intermedios del grupo de datos, en conclusion es un modelo que tenie un exactitud bastante buena y que para la clasificacion es de un de los modelos a utilizar