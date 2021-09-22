
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

<H2><p align="Center">TRABAJOS: Practica 3</p></H2>

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
#Data 
revenue <- c(14574.49, 7606.46, 8611.41, 9175.41, 8058.65, 8105.44, 11496.28, 9766.09, 10305.32, 14379.96, 10713.97, 15433.50)
expenses <- c(12051.82, 5695.07, 12319.20, 12089.72, 8658.57, 840.20, 3285.73, 5821.12, 6976.93, 16618.61, 10054.37, 3803.96)


#solucion

#Ganancias por mes
profit <- revenue - expenses
profit

#30% del valor de impuestos
tax_30_per <- round(profit * 0.30, 0)
tax_30_per

#Ganancias despues de impuestos
profit_after_tax <- profit - tax_30_per
profit_after_tax

#Margen de ganancia en %
profit.margin <- round(profit_after_tax/revenue, 2)*100
profit.margin <- paste(profit.margin,"%")
profit.margin

#Mejor mes
best_month <- max(profit_after_tax) 

#Peor mes
worst_month <- min(profit_after_tax) 

best_month
#
worst_month

mean_for_year <- mean(profit_after_tax)
mean_for_year

#ordenar vector en orden ascendente
profit_sort_asc <- sort(profit_after_tax, decreasing = F)  

for(i in profit_sort_asc){
  if(i>mean_for_year){
    good_month = i
    break
  }else{
    bad_month = i
  }
}

#Mejor mes
good_month

#Peor mes
bad_month

```