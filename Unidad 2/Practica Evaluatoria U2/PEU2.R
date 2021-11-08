# Hirales Lazareno Raymundo - 17212339
# Galaviz Lona Oscar Eduardo - 17212993
#
#Practica Evaluatoria U2

#Librerias Necearias
library(ggplot2)

# Importar Dataset
ourdata <- read.csv(file.choose())

#Filtrar el genero del dataframe
#filtro1
filtro <- (ourdata$Genre == "action") | (ourdata$Genre == "adventure") |(ourdata$Genre   ==  "animation")   |   (ourdata$Genre   ==  "comedy") |(ourdata$Genre == "drama")
filtro

#Filtrar los estudios del dataframes
#filtro2
filtro2 <- ourdata$Studio %in% c("Buena Vista Studios", "WB", "Fox", "Universal", "Sony", "Paramount Pictures")
filtro2

#creacion de Dataframe Filtrado
ourdata2 <- ourdata[filtro & filtro2,]
ourdata2

# Creacion de la grafica
grafica <- ggplot(data=ourdata2,aes(x=Genre, y=Gross...US))+geom_jitter(aes(size=Budget...mill.,colour=Studio))+ geom_boxplot(alpha = 0.8, outlier.color = NA)+ scale_size_continuous(range = c(1, 3))+xlab("Genre")+ ylab("Gross % US")+ ggtitle("Domestic Gross % by Genre")+ theme(axis.title.x = element_text(colour = "Blue", size=15), axis.title.y = element_text(colour = "Blue", size=15), axis.text.x = element_text(size=10), axis.text.y = element_text(size=10), plot.title = element_text(size=20), legend.title = element_text(size=10), text = element_text(family = "sans")  )                      
grafica