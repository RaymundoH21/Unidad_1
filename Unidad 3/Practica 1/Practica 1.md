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

<H2><p align="Center">FECHA: 26/11/21</p></H2>

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

# Visualización de datos de modelo de machine learning regresión lineal:
Usando una fuente de datos de los salarios y la experiencia de los trabajadores con los cuales visualizamos los modelos básicos de regresión lineal.

# Código:
```R
# importar el repositorio
dataset <- read.csv('Salary_Data.csv')

# separar el datasets dentro del training set y el test set
# instalar.packages('caTools')
library(caTools)
set.seed(123)
split <- sample.split(dataset$Salary, SplitRatio = 2/3)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

# acomodando la regresion linear simple al Training set
regressor = lm(formula = Salary ~ YearsExperience,
               data = dataset)
summary(regressor)

# Prediccion de reusltados del Test set
y_pred = predict(regressor, newdata = test_set)

# Visualizando los resultados del Training set
library(ggplot2)
ggplot() +
  geom_point(aes(x=training_set$YearsExperience, y=training_set$Salary),
             color = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            color = 'blue') +
  ggtitle('Salary vs Experience (Training Set)') +
  xlab('Years of experience') +
  ylab('Salary')
```
En esta imagen podemos denotar la linea de progresion lineal del training set
<img alt="Evidence1" src="./../../Unidad 3/Practica 1/IMG/Salarios vs Experiencia (Test Set).JPG">

```R
# Visualizando los resultados del Test set
ggplot() +
  geom_point(aes(x=test_set$YearsExperience, y=test_set$Salary),
             color = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            color = 'blue') +
  ggtitle('Salary vs Experience (Test Set)') +
  xlab('Years of experience') +
  ylab('Salary')
```
<img alt="Evidence1" src="./../../Unidad 3/Practica 1/IMG/Salarios vs Experiencia (Training Set).JPG">

# Analisis y conclusion
Se puede denotar la regresion lineal en el ejercicio a partir de las imagenes presentadas lo que demuestra el uso de este tema para el analisis de mineria de datos.
