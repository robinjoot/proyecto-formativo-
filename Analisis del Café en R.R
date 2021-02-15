############################################
# FUNDAMENTOS DE ANALÍTICA DE DATOS EN R
# APRENDIENDO R CON DATAFRAMES
# Diseñó: Ing. Luis Armando Amaya Q.
############################################
# importante crear un nuevo projecto
#cargar libreria para manejo de dataframes
library(readr)
library(dplyr)
library(ggplot2)
############################################
# Instalar librerias - ejemplo
########## ya las tengo instaladas
#install.packages("dplyr") # se instala 1 sola vez
#install.packages("ggplot2") # se instala 1 sola vez
#############################################
#############################################
### librerias para crear Modelo de regresion lineal en R
install.packages("tidyverse")
library(tidyverse)
install.packages("car")
library(car)
install.packages("boot")
library(boot)
install.packages("QuantPsyc")
library(QuantPsyc)
install.packages("ggplot2")
library(ggplot2)
############################################
# conocer el directorio o carpeta de trabajo actual
dir()
# conoces la ubicacion del archivo seleccionado en el dispositivo
file.choose()

################################
##################
# Recordar que para leer un dataframe ya creado se utiliza
#X_df <- read.table(url, header=T)
# También puede indicarle la ruta y parámetros de la siguiente forma
#datos<-read.csv(files="./carpeta/archivo.csv", header = TRUE, sep=",", dec=".")
# también, puedes leer el dataframe del directorio así:
#datos_df<-read.csv("./carpeta/archivo.csv")
#mi projecto esta en el directorio mis documentos/PROYECTO_R_ANALISIS_CAFE
#cafe_df<-read.csv("./produccioncafe.csv", header = TRUE, sep=",", dec=".")
#DATOS<-read_excel("./PRODUCCIONe.xlsx", header =TRUE)
cafe_df<-read.csv("C:\\Users\\Janus\\Desktop\\Archivos para subir a Github\\Produccion Café Dataframe.csv", header = TRUE, sep=",", dec=".")
cafe_df
class(cafe_df) # indica si es o no es un dataframe
class(cafe_df$Departamento)

class(cafe_df$Producto)
class(cafe_df$Rendimiento..ha.ton.)
class(cafe_df$Produccion..ton.)
class(cafe_df$ï..Anio)
###########################################################
#conocer la dimensión del dataframe
dim(cafe_df)  # Para conocer la dimensión del dataframe - Nro filas y columnas
###########################################################
# Ahora, podemos ver la información resumen de todo el dataframe
# de las variables cuantitativas se tienen valores estadisticos
summary(cafe_df)
# podemos tambien realizar el estudio por variables
summary(cafe_df$Departamento)
summary(cafe_df$Area..ha.)

# información del dataframe con varias variables
tapply(cafe_df$Area..ha.,cafe_df$Produccion..ton.,summary)
###################################################
# tambien algunos valores estadísticos 
mean(cafe_df$Rendimiento..ha.ton.)
max(cafe_df$Rendimiento..ha.ton.)
min(cafe_df$Rendimiento..ha.ton.)
sd(cafe_df$Rendimiento..ha.ton.) 

################
#Las medidas de dispersion nos dicen 
#que tan parecidos o diferentes pueden llegar a ser
# nuestros dataset
max(cafe_df$Rendimiento..ha.ton.)-min(cafe_df$Rendimiento..ha.ton.)
##############################################################

###########################################
# Estructura de los datos del dataframe
str(cafe_df)
#La función str() nos muestra la estructura de este objeto, 
###############################################
# Nombre de las variables o columnas del dataframe
names(cafe_df)
colnames(cafe_df)  # hace lo mismo que names
###############################################
#Seleccion de variables o columnas
#Listar contenido del dataframe, parte inicial, final y por selección especifica
head(cafe_df)
tail(cafe_df)
cafe_df[1:3,]
cafe_df[3:6,]
cafe_df[2:4,]
# asi podemos ir inspeccionando de forma detallada el dataframe
# Seleccionar solamente la columna 1 del dataframe cafe_df
cafe_df
cafe_df[,1]
cafe_df[,1]

#seleccion de la columna 3
cafe_df[3]
cafe_df[1]
peliculas_df
peliculas_df[3]
peliculas_df[,3] # lista los valores de la columna 3 pero en una fila
cafe_df[,4]
peliculas_df[1:3,]
#seleccionar los datos de la segunda columna del Dataframe
cafe_df
cafe_df[,2] # lista los valores de la columna 2
peliculas_df
peliculas_df[,3]
#seleccionar los datos de la segunda columna del Dataframe
cafe_df
cafe_df[,2] #lista los valores de la segunda columna
cafe_df[2,]  #lista los valores de la segunda fila del datafram
cafe_df[,2] #lista los valores de la columna 2
cafe_df[2,] #selección de los valores de la segunda fila
cafe_df[100,] #selección de los valores de la  fila 100
cafe_df
cafe_df[,2] #lista los valores de la columna 2
cafe_df[2,] #selección de los valores de la segunda fila
cafe_df[,2]
peliculas_df[,2]
peliculas_df
#seleccionar los datos de la tercera columna del Dataframe
cafe_df
cafe_df[,3]
cafe_df
cafe_df[,3]
peliculas_df[,3]
peliculas_df
# Si se desea seleccionar algunas columnas
cafe_df
# seleccion de las columnas 1,3,4
cafe_df[c(1, 3, 4)]
peliculas_df[c(1,2,3)]
cafe_df[c(1,2,3)]
cafe_df[c(2,4,6)]
####################################################################
#podemos ELIMINAR filas del dataframe que se consideren no necesarias 
#Para eliminar filas definiremos un vector de tipo lógico en el que 
# indicaremos para cada fila si será eliminada o no:
# Definimos las filas a conservar, en este ejemplo las TRUE
# Creamos vectores con los valores, en este caso CENSO1

################################################################
#Para AÑADIR filas a un `dataframe` existente, definiremos un nuevo vector respetando las variables
#de las columnas que han sido definidas con anterioridad y pegando esta fila al `dataframe` original
#mediante la función `rbind()` (acrónimo de _rowbind_, pegar por filas):
# Creamos vectores con los valores y el tipo de dato deseado
# Podemos Añadir filas en el dataframe
fila_nueva<- c(2007, 'Antioquia','Cafe', 14500, 13400, 0.98,1.90,2.30)

#AÃ±adimos la nueva fila a 'Cafe2' con 'rbind()'
cafe2_df<- rbind(cafe_df,fila_nueva)

# Ahora mostramos el dataframe con la nueva fila
cafe2_df

####################################################################
####################################################################
# continuamos con las seleccion de variables, para lo que necesite en el 
# analisis de los datos

# seleccion de datos por el nombre de las variables
cafe_df$Departamento
names(cafe_df)
cafe_df$Area..ha.
cafe_df$Produccion.Nacional..ton.
cafe_df$Anio
# tambien podemos utilizar la funcion table para listar el total(frecuencias) de cada columna
cafe_df
table(cafe_df$Area..ha.)
table(cafe_df$Produccion..ton.)
table(cafe_df$Rendimiento..ha.ton.)
#Seleccionando datos del Dataframe por fila y columna
# seleccion del dato de la fila 3 y columna 2
cafe_df #listamos el dataframe
#seleccionar dato de la fila 3 y la columna 2
cafe_df
#dataframe[fila,columna]
cafe_df[3, 2]
datox32<-cafe_df[3,2]
datox32

#seleccion de las fila 1,2,3 las columnas 1 y 2

cafe_df[ c(1,2,3), c(1,2)]
cafe_df[ c(10,20,30,40,50,60), c(1,2,3)]
# otro ejemplo similar
#seleccion de las fila 1,2,3 las columnas 1,2,3
cafe_df
cafe_df[ c(1,2,3), c(1,2,3)]
# otro ejemplo similar
#seleccion de la fila 4 las columnas 1,2,3,4
cafe_df
cafe_df[ c(4), c(1,2,3,4)]
##################################################
# Extraer subconjuntos de datos de un Dataframe
# se puede utilizar la función subset(x, subset, select)
# El parámetro x sirve para indicar el marco de datos o dataframe original
# el parámetro subset sirve para colocar la condición 
# el parámetro select sirve para quedarnos sólo con algunas de las variables
cafe_df
# podemos pedir ayuda y comprender sobre los argumentos de esta funcion
help (subset)
###########################
names(cafe_df)
subset(cafe_df, subset = Departamento == 'HUILA')
subset(cafe_df, subset = Departamento == 'META')
subset(cafe_df, subset = Anio == '2009')
subset(cafe_df, subset = Anio == '2012')
subset(cafe_df, subset = Departamento =='HUILA')
subset(cafe_df, subset = Departamento =='VALLE DEL CAUCA')
subset(cafe_df, subset = ï...Anio == 2007)
subset(cafe_df, subset = ï...Anio == 2008)
subset(cafe_df, subset = ï...Anio == 2007)
subset(cafe_df,subset = Departamento == 'ANTIOQUIA',
       select = c('Producto','Area..ha.','Produccion..ton.','Rendimiento..ha.ton.'))

subset(cafe_df,subset = ï...Anio >= 2011 & Departamento == 'VALLE DEL CAUCA')
subset(cafe_df,subset = ï...Anio <= 2011 & Departamento == 'VALLE DEL CAUCA')
subset(cafe_df,subset = ï...Anio >= 2012 & Departamento == 'ANTIOQUIA')
subset(cafe_df,subset = ï...Anio <= 2012 & Departamento == 'ANTIOQUIA')
subset(cafe_df,subset = ï...Anio <= 2013 & Departamento == 'LA GUAJIRA')
subset(cafe_df,subset = ï...Anio >= 2013 & Departamento == 'LA GUAJIRA')
subset(cafe_df,subset = ï...Anio <= 2011 & Departamento == 'LA GUAJIRA',
       select = c('Rendimiento..ha.ton.','Area.Nacional..ha.'))
subset(cafe_df,subset = ï...Anio >= 2011 & Departamento == 'LA GUAJIRA',
       select = c('Rendimiento..ha.ton.','Area.Nacional..ha.'))


###############################
names(cafe_df)
subset(cafe_df, subset=Anio == '2009', 
       select=c('Rendimiento..ha.ton.', 'Produccion.Nacional..ton.'))
#############################
subset(cafe_df, subset=Departamento == 'CUNDINAMARCA', 
       select=c('Rendimiento..ha.ton.', 'Produccion.Nacional..ton.'))
##############
subset(cafe_df, subset=Departamento == 'CUNDINAMARCA' & Anio <= '2012', 
       select=c( 'Produccion.Nacional..ton.', 'Area.Nacional..ha.', 'Rendimiento..ha.ton.'))
#####################
##############
Grupo_Antioquia<-subset(cafe_df, subset=Departamento == 'ANTIOQUIA' & Anio <= '2012', 
                        select=c( 'Produccion.Nacional..ton.', 'Area.Nacional..ha.', 'Rendimiento..ha.ton.'))
Grupo_Antioquia
plot(Grupo_Antioquia)
#################################################
#Podemos crear objetos nuevos (variables)
Produccion_2013<- subset(cafe_df,subset = ï...Anio == 2013 ,
                         select = c('Produccion..ton.',
                                    'Produccion..ton.'))

Produccion_2013
plot(Produccion_2013)

Produccion_2008_Mayor<- subset(cafe_df,subset = 
                                 ï...Anio >=2008,
                               select = c('Produccion..ton.',
                                          'Produccion..ton.'))
Produccion_2008_Mayor
plot(Produccion_2008_Mayor)
########################################
# Ordenacion de dataframes de forma creciente o decaente
cafe_df
cafe_df[order(cafe_df$Departamento, decreasing = FALSE),]
cafe_df[order(cafe_df$Departamento, decreasing = TRUE),]
#########################################################

#######################################################
# podemos ver si existen datos NULOS en todo el dataframe censo
# si existen datos NULOS (NA), se tendrá la respuesta TRUE (TRUE equivale a 1)

is.na(cafe_df$Departamento)
is.na(cafe_df$Producto)
is.na(cafe_df$Area..ha.)
is.na(cafe_df$Produccion..ton.)
is.na(cafe_df$Rendimiento..ha.ton.)
is.na(cafe_df$Produccion.Nacional..ton.)
is.na(cafe_df$Area.Nacional..ha.)
is.na(cafe_df$ï...Anio)

# conocer por filas la suma de valores perdidos o nulos (NA)
rowSums(is.na(cafe_df)) # si el valor es diferente de cero(0) hay datos nulos

# ahora vamos a totalizar la cantidad de valores nulos o perdidos
sum(rowSums(is.na(cafe_df)))


################################################
################################################
#Visualizacion de histogramas

hist(cafe_df$Rendimiento..ha.ton.)
hist(cafe_df$Rendimiento..ha.ton.,
     col = 'red',
     main = 'Histograma',
     xlab = 'Frecuencia',
     ylab = 'Rendimiento (ha/ton)')


cafe_df
plot(cafe_df$Rendimiento..ha.ton.)
# ahora el mismo grafico pero con detalles
plot(cafe_df$Rendimiento..ha.ton., main ="Gráfico Dispersion", 
     sub ="CANTIDAD",
     type ="p", # p indica puntos de dispersion
     col ="red",
     xlab ="cantidad",
     ylab = "Rendimiento (ha/ton)")
#############
plot(cafe_df$Rendimiento..ha.ton., main ="Gráfico Dispersion", 
     sub ="Indices",
     type ="b", # b indica lineas uniendo los puntos de dispersion
     col ="red",
     xlab ="cantidad",
     ylab = "Rendimiento (ha/ton)")
##########
plot(cafe_df$Rendimiento..ha.ton., main ="Gráfico Dispersion", 
     sub ="Indices",
     type ="o", # b indica lineas sobreindicadas sobre los puntos de dispersion
     col ="blue",
     xlab ="cantidad",
     ylab = "Rendimiento (ha/ton)")
###########################
plot(cafe_df$Rendimiento..ha.ton., main ="Gráfico Dispersion", 
     sub ="Indices",
     type ="h", # b indica lineas tipo histograma sobre los puntos de dispersion
     col ="blue",
     xlab ="cantidad",
     ylab = "Rendimiento (ha/ton)")
##############
plot(cafe_df$Rendimiento..ha.ton., main ="Gráfico Dispersion", 
     sub ="Indices",
     type ="l", # l indica lineas 
     col ="blue",
     xlab ="cantidad",
     ylab = "Rendimiento (ha/ton)")
###############
plot(cafe_df$Rendimiento..ha.ton., main ="Gráfico Dispersion", 
     sub ="Indices",
     type ="s", # s funcion escalera (horizontal a vertical)
     col ="blue",
     xlab ="cantidad",
     ylab = "Rendimiento (ha/ton)")
##############
# graficos por variables
plot(x = cafe_df$Area..ha.,  y = cafe_df$Rendimiento..ha.ton.)
plot(x = cafe_df$Area..ha.,  y = cafe_df$Area.Nacional..ha.)
plot(x = cafe_df$ï..Anio,  y = cafe_df$Rendimiento..ha.ton.)
plot(x = cafe_df$ï..Anio,  y = cafe_df$Produccion..ton.)
plot(x = cafe_df$Area..ha.,  y = cafe_df$Produccion.Nacional..ton.)
plot(cafe_df$Area..ha.)
#########################
# hallamos las frecuencias de los datos de la variable Rendimiento
table(cafe_df$Rendimiento..ha.ton.)
# diagrama de barras variable Rendimiento (ha/ton)
barplot(table(cafe_df$Rendimiento..ha.ton.))
#################################################
# datos estadisticos
cafe_df
summary(cafe_df$Rendimiento..ha.ton.)
summary(cafe_df$Produccion.Nacional..ton.)
summary(cafe_df$Area.Nacional..ha.)
#vamos a realizar el estudio de manera grafica la relacion entre dos variables
#obasevando su comportamiento en el diagrama de dispersion
plot(cafe_df$Produccion..ton.,cafe_df$Rendimiento..ha.ton.)
plot(cafe_df$Area..ha.,cafe_df$Rendimiento..ha.ton.)
plot(cafe_df$Area.Nacional..ha.,cafe_df$Rendimiento..ha.ton.)
#################
# Para ver como se distribuyen los datos, 
# podemos usar una funcion de densidad
#La densidad es una version suavizada del histograma
# Permite observar si los datos observados se comportan
# como una densidad conocida ejemplo: la distrib. normal.
plot(density(cafe_df$Rendimiento..ha.ton.), main="Densidad para el Rendimiento (ha/ton)")
# comparada con su el histograma que se da a continuacion

################3
plot(density(cafe_df$Produccion.Nacional..ton.), main="Densidad para la produccion Nacional (ton)")
plot(density(cafe_df$Area.Nacional..ha.), main="Densidad para el Area Nacional (ha)")
###############
# Para  graficos tipo pie o pastel
pie(table(cafe_df$Departamento[1:10]))
pie(table(cafe_df$Rendimiento..ha.ton.[1:10]))
#######################
#Los Boxplots o diagramas de caja
#se construyen a partir de los percentiles.
# Se construye una rectangulo usando entre
#el primer y el tercer cuartil (Q1 y Q3)
#La altura del rectangulo es el rango intercuartil 
#La mediana es una linea que divide el rectangulo.
#Los valores mas extremos que el largo 
# de los brazos son considerados atipicos
# El boxplot nos entrega informacion sobre la simetria de la distribucion de los datos
# Si la mediana no esta en el centro del rectangulo, la distribucion no es simetrica
# Son utiles para ver la presencia de valores atipicos u outliers
boxplot(cafe_df$Rendimiento..ha.ton.,ylab="Rendimiento (ha/ton)")
boxplot(cafe_df$Produccion.Nacional..ton.,ylab="Produccion Nacional (ton)")
##### por rangos
boxplot(x=cafe_df$Rendimiento..ha.ton.[1:10],main="Rendimiento (ha/ton)")
####################################################
##################################################
# para mejorar la presentación y lectura de los graficos
### install.packages("hexbin") # solo se instala UNA VEZ
# cargar las funciones de la libreria
require(hexbin)
# tambien puedes usar:  library(hexbin)
# se crea el objeto bin con la relación de las dos variables
bin1<-hexbin(cafe_df$Produccion..ton.,cafe_df$Rendimiento..ha.ton., xbins=10)
plot(bin1)
########
bin2<-hexbin(cafe_df$Area.Nacional..ha.,cafe_df$Rendimiento..ha.ton., xbins=10)
plot(bin2)
##################
# hallar la correlacion entre las variables por correlacion de pearson
# indicamos que utiliza las variables con valores descartando los valores nulos que existan 
cor(cafe_df$Area.Nacional..ha.,cafe_df$Rendimiento..ha.ton., use="complete.obs")
#####################
# Una forma de calcular la moda es utilizando funciones
# calcular la moda para la variable Rendimiento 
# en el dataframe Cafe
cafe_df
table(cafe_df$Rendimiento..ha.ton.)
moda=function(var){
  frec.var<-table(var)
  valor=which(frec.var==max(frec.var)) #Elemento con el valor
  names(valor)}
# ahora se llama a la función para que entregue el valor
moda(cafe_df$Rendimiento..ha.ton.)
#######################################
# graficos en 3D
###install.packages("scatterplot3d",dependencies=T)
#Luego se carga la libreria de la siguiente manera
library(scatterplot3d)
# desactivadas las siguientes 6 lineas, faltan AJUSTES
#plot(cafe_df$ï..Year, cafe_df$Rendimiento..ha.ton.,col=cafe_df$ï..Year,
#     pch=as.numeric(cafe_df$ï..Year)),
# Le agregamos una leyenda
# legend("topright", levels(cafe_df$Rendimiento..ha.ton.),
# lty=1, col=1:3, bty="n", cex=.75)
#########################################
# seguimos estudiando el comportamiento o distribución
# de los datos o la información con gráficos de dispersion
# Grafico de dispersion del comportamiento 
# de la producción versus Rendimiento
# permite entender o descubrir patrones en los datos
# install.packages("qplot")
# install.packages("ggplot2")
qplot(Produccion..ton., Rendimiento..ha.ton., data = cafe_df,
      main ="Grafico de Dispersion",
      col = "Red",
      xlab ="Produccion (miles ton)",
      ylab = "Rendimiento (ha/ton)")
#################     
# Grafico de dispersion del comportamiento 
# de la producción nacional versus Rendimiento
# permite entender o descubrir patrones en los datos
qplot(Produccion.Nacional..ton., Rendimiento..ha.ton., data = cafe_df,
      main ="Grafico de Dispersion",
      col = "Red",
      xlab ="Produccion Nacional (miles ton)",
      ylab = "Rendimiento (ha/ton)")
################## coeficiente de correlacion de Pearson ######
# coeficiente de correlación entre La produc nacional y el rendimiento
# hallar la correlacion entre las variables por correlacion de pearson
# indicamos que utiliza las variables con valores descartando los valores nulos que existan 
cor(cafe_df$Produccion.Nacional..ton.,cafe_df$Rendimiento..ha.ton., use="complete.obs")
#####################
######################
# Grafico de dispersion del comportamiento 
# entre el Area nacional versus Rendimiento
# permite entender o descubrir patrones en los datos
qplot(Area.Nacional..ha., Rendimiento..ha.ton., data = cafe_df,
      main ="Grafico de Dispersion",
      col = "Red",
      xlab ="Area Nacional ha",
      ylab = "Rendimiento (ha/ton)")
################## 
# coeficiente de correlacion de Pearson
# coeficiente de correlación entre el area nacional y el rendimiento
# hallar la correlacion entre las variables por metodo correlacion de pearson
# indicamos que utiliza las variables con valores descartando los valores nulos que existan 
cor(cafe_df$Area.Nacional..ha.,cafe_df$Rendimiento..ha.ton., use="complete.obs")
#####################
qplot(Anio, Rendimiento..ha.ton., data = cafe_df,
      main ="Grafico de Dispersion",
      col = "Red",
      xlab ="Area Nacional ha",
      ylab = "Rendimiento (ha/ton)")
cor(cafe_df$Area.Nacional..ha.,cafe_df$Rendimiento..ha.ton., use="complete.obs")
##################################
#####  Graficando con la libreria ggplot #####
#install.packages("ggplot2") #Se instala 1 sola vez
library(tidyverse)
# Especificar el dataframe es el primer argumento en la función ggplot
# dentro de aes() escribimos las variables (x,y) que queremos graficar 
# permite entender o descubrir patrones en los datos
# se puede utilizar el parametro color para representar una tercera variable
# Gráfico de dispersion: Area Nacional vs Rendimiento
ggplot(cafe_df) + 
  geom_point(aes(x = Area.Nacional..ha., y = Rendimiento..ha.ton., color = "Anio"))
################
# Gráfico de lineas: Area Nacional vs Rendimiento
ggplot(cafe_df) + 
  geom_line(aes(x = Area.Nacional..ha., y = Rendimiento..ha.ton., color = "Anio"))
###################
# Gráfico de dispersion: Produccion Nacional vs Rendimiento
ggplot(cafe_df) + 
  geom_point(aes(x = Produccion.Nacional..ton., y = Rendimiento..ha.ton., color = "Anio"))
################
###################
# Gráfico de lineas: Produccion Nacional vs Rendimiento
ggplot(cafe_df) + 
  geom_line(aes(x = Produccion.Nacional..ton., y = Rendimiento..ha.ton., color = "Anio"))
################
# otra forma con grafico de lineas sin representar los años
#data(cafe_df)
ggplot(cafe_df, aes(x = Produccion.Nacional..ton., y = Rendimiento..ha.ton., color ="red")) + 
  geom_line()
##################
# Histograma: Area Nacional 
ggplot(cafe_df) + 
  geom_histogram(aes(x = Area.Nacional..ha., color="red"))
################
# Histograma: Produccion Nacional
ggplot(cafe_df) + 
  geom_histogram(aes(x = Produccion.Nacional..ton., color="red"))
################
# Histograma: Rendimiento
ggplot(cafe_df) + 
  geom_histogram(aes(x = Rendimiento..ha.ton., color="red"))
################
# Gráfico de lineas: Produccion Nacional vs Rendimiento
ggplot(cafe_df) + 
  geom_line(aes(x = Produccion.Nacional..ton., y = Rendimiento..ha.ton., color = 'Anio'))
################
################
# Gráfico de cajas: Produccion Nacional vs Rendimiento
ggplot(cafe_df) + 
  geom_boxplot(aes(x = Produccion.Nacional..ton., y = Rendimiento..ha.ton., color = 'Anio'))
################
# Gráfico de cajas: Area Nacional vs Rendimiento
ggplot(cafe_df) + 
  geom_boxplot(aes(x = Area.Nacional..ha., y = Rendimiento..ha.ton., color = 'Anio'))
#################
###################
################
# Gráfico de dispersion -jitter: Area Nacional vs Rendimiento
# análisis del comportamiento, dispersion mediante colores, agrupados por años
ggplot(cafe_df) + 
  geom_jitter(aes(x = Area.Nacional..ha., y = Rendimiento..ha.ton., color = 'Anio'))
###################
# Gráfico de dispersion: Produccion Nacional vs Rendimiento
ggplot(cafe_df) + 
  geom_point(aes(x = Produccion.Nacional..ton., y = Rendimiento..ha.ton., color = 'Anio'))
################
# Gráfico de dispersion con jitter: Produccion Nacional vs Rendimiento
ggplot(cafe_df) + 
  geom_jitter(aes(x = Produccion.Nacional..ton., y = Rendimiento..ha.ton., color = 'Anio'))
#####################
# otra forma con grafico de dispersion sin representar los años
#data(cafe_df)
ggplot(cafe_df, aes(x = Produccion.Nacional..ton., y = Rendimiento..ha.ton., color ="red")) + 
  geom_point()
# otra forma con grafico de lineas sin representar los años
#data(cafe_df)
ggplot(cafe_df, aes(x = Produccion.Nacional..ton., y = Rendimiento..ha.ton., color ="red")) + 
  geom_line()
#comparando cuando tiene representado los años a continuacion
#data(cafe_df)
ggplot(cafe_df, aes(x = Produccion.Nacional..ton., y = Rendimiento..ha.ton., color ='Anio')) + 
  geom_line()
###############################
# tambien se puede realizar dispersion en PANELES por años
# permite entender o descubrir patrones en los datos
ggplot(cafe_df, aes(x = Produccion.Nacional..ton., y = Rendimiento..ha.ton.)) + 
  geom_jitter() +
  facet_grid(.~ 'Anio')

###################
# permite entender o descubrir patrones en los datos
ggplot(cafe_df, aes(x = Area.Nacional..ha., y = Rendimiento..ha.ton.)) + 
  geom_point() +
  facet_wrap(.~ 'Anio')
############
# GRAFICA DE DISPERSION - Produc Nacional versus Rendimiento
#Comparando el anterior con el grafico de dispersion sin años
# otra forma con grafico de dispersion sin representar los años
#data(cafe_df)
ggplot(cafe_df, aes(x = Produccion.Nacional..ton., y = Rendimiento..ha.ton., color ="red")) + 
  geom_point()
#############################
# comparar con el Gráfico de dispersion  Produccion Nacional vs Rendimiento con años
ggplot(cafe_df) + 
  geom_jitter(aes(x = Produccion.Nacional..ton., y = Rendimiento..ha.ton., color = 'Anio'))

##############################################
###############################################
#Grafica de dispersion del Área Nacional versus Producción Nacional
ggplot(cafe_df, aes(x = Area.Nacional..ha., y = Produccion.Nacional..ton., color ="red")) + 
  geom_point()
############################################
###############################################
#Grafica de dispersion del Área Nacional versus Producción Nacional
# mostrando comportamiento por años en tonalidades de azul
ggplot(cafe_df) + 
  geom_jitter(aes(x = Area.Nacional..ha., y = Produccion.Nacional..ton., color = 'Anio'))
Grafica_AreaNal_ProdNal_2=ggplot(cafe_df) + 
  geom_jitter(aes(x = Area.Nacional..ha., y = Produccion.Nacional..ton., color = 'Anio'))
Grafica_AreaNal_ProdNal_2
##############################################################
###############   MODELO DE REGRESION LINEAL EN R ############
#############################################################
#############################################################
#############################################################
### LIBRERIAS para crear Modelo de regresion lineal en R
# REcuerde, que se instalan una (1) vez (INSTALL), luego solo requiere llamar(LIBRARY)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("car")
library(car)
#install.packages("boot")
library(boot)
#install.packages("QuantPsyc")
library(QuantPsyc)
#install.packages("ggplot2")
library(ggplot2)
########################################################
# construccion del modelo de regresion lineal, variable de entrada Area Nacional, Variable Salida Produc Nal
# CREAMOS EL MODELO DE REGRESIÓN LINEAL - PRODUC. NACIONAL VERSUS AREA NACIONAL
modelo_regresion_lineal=lm( Produccion.Nacional..ton.~ Area.Nacional..ha.,data=cafe_df, na.action=na.exclude)
#####################################
#####################################
# A continuación se describen los parámetros del modelo anterior
summary(modelo_regresion_lineal)
######################################
# Construcción de la gráfica Area Nal. versus Prod. Nacional 
Grafica_AreaNal_ProdNal_1=ggplot(cafe_df, aes(x = Area.Nacional..ha., y = Produccion.Nacional..ton., color ="red")) + 
  geom_point()
# a continuacion se despliega la grafica dispersion Area Nacional Versus Prod. Nac. 
Grafica_AreaNal_ProdNal_1
##########################################
##########################################
# MODELO DE REGRESION LINEAL - Area Nacional versus Producción Nacional
# Ahora se representa de dispersion con el modelo de regresión lineal en color azul
# la linea recta de color azul representa el modelo.
Grafica_AreaNal_ProdNal_1 + geom_point()+geom_smooth(method="lm", colour ="Blue")
########################################################
################################################################
###############################################
#Grafica de dispersion del Área Nacional versus Rendimiento
Grafica_AreaNal_Rendim_1=ggplot(cafe_df, aes(x = Area.Nacional..ha., y = Rendimiento..ha.ton., color ="red")) + 
  geom_point()
# a continuacion desplegamos la anterior grafica
Grafica_AreaNal_Rendim_1
############################################
## CREAMOS EL MODELO DE REGRESIÓN LINEAL DE RENDIMIENTO VERSUS AREA NACIONAL
modelo_regresion_lineal=lm( Rendimiento..ha.ton.~ Area.Nacional..ha.,data=cafe_df, na.action=na.exclude)
###############################################
#####################################
# A continuación se describen los parámetros del modelo anterior
summary(modelo_regresion_lineal)
######################################
###################################
###################################
# MODELO DE REGRESION LINEAL - Area Nacional versus Rendimiento
# Ahora se representa GRAFICA 2 con el modelo de regresión lineal en color azul
# la linea recta de color azul representa el modelo.
Grafica_AreaNal_Rendim_1 + geom_point()+geom_smooth(method="lm", colour ="Blue")
########################################################
################################################################
# Analisis de los datos mediante otros tipos de gráficas
# Ahora se utilizará un suavizador 
# la grafica lineal suavizada
# permite entender o descubrir patrones en los datos por años
ggplot(cafe_df, aes(x = Area.Nacional..ha., y = Rendimiento..ha.ton.)) + 
  geom_point() +
  facet_wrap(.~ 'Anio') +
  geom_smooth(span = 2)
############
# grafica general del dataframe cafe
ggplot(cafe_df)
########
ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy, color = class))
ggplot(mpg) + 
  geom_line(aes(x = displ, y = hwy, color = class))
#####################################################
### LIBRERIAS para crear Modelo de regresion lineal en R
# REcuerde, que se instalan una vez
install.packages("tidyverse")
library(tidyverse)
#install.packages("car")
library(car)
#install.packages("boot")
library(boot)
#install.packages("QuantPsyc")
library(QuantPsyc)
#install.packages("ggplot2")
library(ggplot2)
cafe_df
##########################
########################
## LIBRERIAS PARA RMARKDOWN
install.packages("rmarkdown")
library(rmarkdown)
