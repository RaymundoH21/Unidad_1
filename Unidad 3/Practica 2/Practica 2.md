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

<H2><p align="Center">FECHA: 1/11/21</p></H2>

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

### Desarrollo 
Aqui comenzamos con la carga del archivo csv que se utilizara para el analisis de datos de esta practica, una vez cargados procedemos a que los estados sean convertidos a datos categoricos en este caso numeros, despues dividimos el dataframe en dos con un semilla de aleatoriedad para que los datos se repartan de manera aleatoria

```R
getwd()
setwd("/home/chris/Documents/itt/Enero_Junio_2020/Mineria_de_datos/DataMining/MachineLearning/MultipleLinearRegression")
getwd()

# Importing the dataset
dataset <- read.csv('50_Startups.csv')

# Encoding categorical data 
dataset$State = factor(dataset$State,
                       levels = c('New York', 'California', 'Florida'),
                       labels = c(1,2,3))

dataset
# Splitting the dataset into the Training set and Test set
# Install.packages('caTools')
library(caTools)
set.seed(123)
split <- sample.split(dataset$Profit, SplitRatio = 0.8)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

# Fitting Multiple Linear Regression to the Training set
#regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State)
regressor = lm(formula = Profit ~ .,
               data = training_set )

summary(regressor)
```
Por ultimo los resultado del modelo que mediante la regresion  

<img alt="Evidence1" src="./../../Unidad 3/Practica 2/IMG/summary.PGN">

# predicciones
Aqui nos muestran las predicciones que tendria cada uno de los campos del dataframe

```R
# Prediction the Test set results
y_pred = predict(regressor, newdata = test_set)
y_pred
```
<img alt="Evidence1" src="./../../Unidad 3/Practica 2/IMG/prediccion en y.PGN">

# Optimizacion del modelo para usar backward elimanation
En este apartado empezamos a optimizar el dataframe para la utilizacion del backward elimanation reduciendo el dataframe a solo unos cuantos campos claves del dataframe que se utilizaran para este analisis

```R
# Assigment: visualize the siple liner regression model with R.D.Spend 

# Building the optimal model using Backward Elimination
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
               data = dataset )
summary(regressor)
```
<img alt="Evidence1" src="./../../Unidad 3/Practica 2/IMG/summary regresor.PGN">

```R
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend,
               data = dataset )
summary(regressor)
```
<img alt="Evidence1" src="./../../Unidad 3/Practica 2/IMG/no state.PGN">

```R
regressor = lm(formula = Profit ~ R.D.Spend + Marketing.Spend,
               data = dataset )
summary(regressor)

regressor = lm(formula = Profit ~ R.D.Spend + Marketing.Spend,
               data = dataset )
summary(regressor)
```
<img alt="Evidence1" src="./../../Unidad 3/Practica 2/IMG/no administration.PGN">

```R
y_pred = predict(regressor, newdata = test_set)
y_pred
```
<img alt="Evidence1" src="./../../Unidad 3/Practica 2/IMG/new prediccion en y.PGN">

# uso de backwardelimination
una vez reducido el dataframe procedemos a realizar el uso de la funcion de backwardelimination. creamos la funcion para que se realice en el dataframe que reducimos

```R
# Homework analise the follow atomation backwardElimination function 
backwardElimination <- function(x, sl) {
  numVars = length(x)
  for (i in c(1:numVars)){
    regressor = lm(formula = Profit ~ ., data = x)
    maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
    if (maxVar > sl){
      j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
      x = x[, -j]
    }
    numVars = numVars - 1
  }
  return(summary(regressor))
}

SL = 0.05
#dataset = dataset[, c(1,2,3,4,5)]
training_set
```
<img alt="Evidence1" src="./../../Unidad 3/Practica 2/IMG/training set.PGN">

# resultados del backwardelimination
Aqui se nos muestran los resultados como el analisis anterior pero con la funcion del analisis de backwardeliminatio y nos mostrara una serie de resultados estadisticos como la media, la mediana, el cuartil, el error estandar entre otros que me muestra

```R
backwardElimination(training_set, SL)
```

<img alt="Evidence1" src="./../../Unidad 3/Practica 2/IMG/backward.PGN">