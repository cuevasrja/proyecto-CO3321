# CO3321 Estadística para Ingenieros
# Proyecto(15%).R
# Baudilio Velasquez - Carnet: 18-10665
# Juan Cuevas        - Carnet: 19-10056
# Anya Marcano       - Carnet: 19-10336

source("linear_reg.R")
# Cargamos la base de datos suministrada
library(readxl)
Datos_car <- read_excel("D:/Downloads/Datos_car.xlsx")
View(Datos_car)
attach(Datos_car)
names(Datos_car)

#1. Realice un análisis descriptivo de las variables.

# VARAIBLE: Brand
# Como Brand es una variable cualitativa, procedemos a visualizar la frecuencia
# de las marcas de los carros usando la funcion table y un grafico de barras.

table(Brand)

# Ademas, para facilitar el analisis de la variable Brand procedemos a generar 
# dos funciones que nos permitan visualizar la frecuencia de las marcas de los 
# carros en un grafico de barras con colores iguales para las marcas que tengan
# la misma frecuencia:

frecuencias <- table(Brand)
colores_unicos <- rainbow(length(unique(frecuencias)))
mapeo_colores <- colores_unicos[as.factor(frecuencias)]

barplot(table(Brand), col = mapeo_colores, 
        main = "Frecuencia de las marcas de los carros", ylab = "Frecuencia", 
        las=2, ylim=c(0, 12))

# Analisis de la variable Brand:

# A partir del grafico de barras y del resumen proporcionado por la funcion table
# podemos apreciar que de las 11 opciones posibles para la variable Brand, las
# marcas con mayor frecuencia son: Ford, Hyundai y Tata, cada una de ellas con 
# una frecuencia de 11, seguidas por Audi, BMW, Mercedes, Toyota y Wolksvagen 
# con una frecuencia de 10, mientras que por otro lado, la marca con menor 
# frecuencia corresponde con Mahindra con una frecuencia de 5, seguida por las 
# marcas Maruti y Honda con una frecuencia de 6.

# VARIABLE: Model
# Como Model es una variable cualitativa, procedemos a visualizar la frecuencia
# de los modelos de los carros usando la funcion table y un grafico de barras.

table(Model)

# Igual que como hicimos en el analisis anterior, vamos a generar dos funciones
# que nos permitan visualizar la frecuencia de los modelos de los carros en un
# grafico de barras con colores iguales para los modelos que tengan la misma
# frecuencia:

frecuencias <- table(Model)
length(frecuencias)
colores_unicos <- rainbow(length(unique(frecuencias)))
mapeo_colores <- colores_unicos[as.factor(frecuencias)]

barplot(table(Model), col = mapeo_colores, 
        main = "Frecuencia de los modelos de los carros", ylab = "Frecuencia", 
        xlab = "Modelos", names.arg="")

# Se decicio no mostrar los nombres de los modelos en el eje x, ya que al ser 58
# modelos distintos se dificulta poder mostrar el nombre de cada uno de ellos 
# asociado a su respectiva barra, por esto se aclarara que sucede con cada uno
# de estos modelos en el analisis de la variable Model.

# Analisis de la variable Model:

# A partir del grafico de barras y del resumen proporcionado por la funcion table
# vemos que para esta variable hay 58 modelos distintos, de los cuales la mayor
# frecuencia corresponde con loz modelos Altroz, C-Class, Mustang y Verna 
# teniendo cada uno de ellos una frecuencia de 3, seguidos por los modelos: 
# Yaris, XUV300, X5, x3, Vento, Venue, Vitara, WR-V, T-Roc, Thar, Tiago, Tigor,
# Ranger, S-Cross, Safari, Santro, Passat, Q7, GLA, GLE, Innova Crysta, EcoSport, 
# Elantra, Fortuner, Camry, E-Class, A6, Ameo, Aspire, BR-V, 3 Series, 7series,
# A3 y  A5 los cuales tienen una frecuencia de 2, mientras que los modelos:
# 5 Series, A4, City, Civic, Corolla, Creta, Endeavor, Ertiga, Figo, Harrier, 
# Innova, Nexon, Polo, Q3, Scorpio, Sonata, Swift, Tiguan y  X1 tienen una
# frecuencia de 1.
# En resumen, la mayor frecuencia la tiene 4 modelos, seguidos por 34 modelos 
# con una frecuencia de 2 y los restantes 20 modelos con una frecuencia de 1.

# VARIABLE: Year
# Como Year es una variable cuantitativa, procedemos a realizar un análisis
# descriptivo de la variable Year, para ello usamos las funciones summary, sd,
# IQR y quantile para obtener un resumen de la variable y calcular la media,
# desviación estándar, rango intercuartílico y cuartiles respectivamente; así 
# mismo generaremos un histograma y un boxplot para visualizar la distribución
# de los datos.

# Resumen de la variable Year:
summary(Year)
sd(Year)
IQR(Year)
quantile(Year, c(0.25, 0.5, 0.75))

# Gráficas de la variable Year:
hist(Year, main = "Histograma de la variable Year", xlab = "Year", 
     ylab = "Frecuencia", col = "darkblue", las=1, ylim = c(0, 30))

boxplot(Year, main = "Boxplot de la variable Year", xlab = "Year", 
        col = "cyan", horizontal=TRUE)

# Analisis de la variable Year:
# El año promedio de los datos es 2018, con una desviación estándar de 
# 1.17116 y una mediana tambien de 2018. 
# El año minimo presente en la muestra es 2016, mientras que el año maximo es
# 2021. En cuanto al rango intercuartílico, este es de 2 años, siendo el 25% de
# los datos menores a 2017, el 50% menores a 2018 y el 75% menores a 2019.

# Notándose que la mayor parte de los datos se encuentran en el rango
# de 2017 a 2019, siendo los años 2018 y 2019 los que tienen la mayor frecuencia
# de aparición en la muestra.
# 
# En cuando a la distribución de los datos, tanto en el histograma como en el
# boxplot se observa que los datos están distribuidos mostrando una asimetría
# desplazada hacia la izquierda, esto claramente evidenciable en el diagrama de
# caja, ya que la parte más grande de misma es la que se encuentra superior a la
# mediana. Así mismo, podemos apreciar que no hay presencia de valores atípicos
# en el boxplot de la muestra para esta variable.

# Finalmente, tanto en el histograma como en el boxplot se observa cierta 
# dispersión de los datos, los cuales se encuentran más dispersos entre el 
# segundo y tercer cuartil cuartil, y más concentrados entre el primer y 
# segundo cuartil.

# VARIABLE: Kilometers_Driven
# Como Kilometers_Driven es una variable cuantitativa, procedemos a realizar un
# análisis descriptivo de la variable Kilometers_Driven, para ello haremos un 
# analisis parecido al que planteamos para la variable Year, haciendo un resumen
# de los datos y generando un histograma y un boxplot para visualizar la
# distribución de los mismos.

# Resumen de la variable Kilometers_Driven:
summary(Kilometers_Driven)
sd(Kilometers_Driven)
IQR(Kilometers_Driven)
quantile(Kilometers_Driven, c(0.25, 0.5, 0.75))

# Gráficas de la variable Kilometers_Driven:
hist(Kilometers_Driven, main = "Histograma de la variable Kilometers_Driven", 
     xlab = "Kilometers_Driven", ylab = "Frecuencia", col = "darkgreen", 
     las=1, ylim = c(0, 30))

boxplot(Kilometers_Driven, main = "Boxplot de la variable Kilometers_Driven", 
        xlab = "Kilometers_Driven", col = "green", horizontal=TRUE)

# Analisis de la variable Kilometers_Driven:
# El promedio de kilometros recorridos por los carros es de 28150km, con una
# desviación estándar de 9121.376 km y una mediana de 27000 km
# El kilometraje minimo presente en la muestra es de 10000 km, mientras que el
# kilometraje maximo es de 60000. 
# En cuanto al rango intercuartílico, este es de 10000 km, siendo el 25% de 
# los datos menores a 22000 km, el 50% menores a 27000 km y el 75% menores a 
# 32000 km. Notándose que la mayor parte de los datos se encuentran en el rango
# de 22000 a 32000 km.

# En cuando a la distribución de los datos, tanto en el histograma como en el
# boxplot se observa que los datos están distribuidos aparentemente de forma 
# simétrica, teniendose la presencia de dos valores atípicos, los cuales son 
# claramente evidenciados en el boxplot y que corresponden con kilometrajes 
# de 50000 km y 60000 km, los cuales se encuentran por encima del tercer 
# cuartil, donde cabe mencionar que el kilometraje de 60000 km es el valor 
# máximo de la muestra observado una sola vez en el Car_ID 5, mientras que el 
# kilometraje de 50000 km es el segundo valor máximo de la muestra observado 
# dos veces, una en el Car_ID 1 y otra en el Car_ID 21.

# Finalmente, tanto en el histograma como en el boxplot se puede observar que
# la distribución de los datos es casi simétrica, y esto lo podemos afirmar, ya
# que a pesar de que visualmente el boxplot muestra que la mediana 
# divide en dos partes iguales la caja, lo cierto es que la simetría implica 
# que el valor de la mediana debería coincidir con el de la media, sin embargo, 
# esto no es así, existe una sutil diferencia entre ellas siendo la
# mediana de 27000 km y la media de 28150 km, es decir, hay un margen de 1150 km
# entre ambas medidas; no obstante, esta diferencia puede 
# deberse precisamente a la presencia de valores atípicos mencionados 
# anteriormente, ocasionan que la media se vea ligeramente desplazada
# y no coincida con la mediana.

# VARIABLE: Fuel_Type
# Como Fuel_Type es una variable cualitativa, procedemos a visualizar la
# frecuencia de los tipos de combustible de los carros usando la funcion table y
# un grafico de barras.

table(Fuel_Type)

barplot(table(Fuel_Type), col = c("orange", "green"), 
        main = "Frecuencia de los tipos de combustible de los carros", 
        ylab = "Frecuencia")

# Analisis de la variable Fuel_Type:
# A partir del grafico de barras y del resumen proporcionado por la función 
# table podemos decir que, en el caso de la variable Fuel_Type, se tienen un 
# total de 2 tipos de combustible, siendo el Diesel el tipo de combustible 
# con menor frecuencia de aparición en la muestra, con una frecuencia de 48 
# autos, mientras que su alternativa para este estudio, el Petrol, tuvo una 
# frecuencia sutilmente mayor, con 52 autos, teniéndose una diferencia de tan 
# solo 4 autos entre ambos tipos de combustible.

# VARIABLE: Transmission
# Como Transmission es una variable cualitativa, procedemos a visualizar la
# frecuencia de los tipos de transmisión de los carros usando la funcion table y
# un grafico de barras.

table(Transmission)

barplot(table(Transmission), col = c("magenta", "yellow"), 
        main = "Frecuencia de los tipos de transmisión de los carros", 
        ylab = "Frecuencia", ylim = c(0,60))

# Analisis de la variable Transmission:
# A partir del grafico de barras y del resumen proporcionado por la función
# table podemos decir que, en el caso de la variable Transmission, se tienen un
# total de 2 tipos de transmisión, siendo la Automatica la transmición con mayor
# frecuencia de aparición en la muestra, con una frecuencia de 57 autos, 
# mientras que su alternativa para este estudio, la Manual, tuvo una frecuencia
# sutilmente menor, con 43 autos, teniéndose una diferencia de tan solo 14 autos
# entre ambos tipos de transmisión.

# VARIABLE: Owner_Type
# Como Owner_Type es una variable cualitativa, así como en los casos anteriores
# procedemos a generar, la tabla y el gráfico de barras para visualizar la 
# frecuencia de los tipos de dueños de los carros.

table(Owner_Type)

barplot(table(Owner_Type), col = c("salmon", "pink", "violet"), 
        main = "Frecuencia de los tipos de dueños de los carros", 
        ylab = "Frecuencia", ylim = c(0,50))

# Analisis de la variable Owner_Type:
# A partir del grafico de barras y del resumen proporcionado por la función
# table podemos decir que, en el caso de la variable Owner_Type, se tienen un
# total de 3 tipos de dueños, siendo el First Owner el tipo de dueño con mayor
# frecuencia de aparición en la muestra, con una frecuencia de 44 seguida
# inmediatamente por la categoria Second Owner, la cual tiene una frecuencia 
# de 43 autos, es decir, solamente 1 punto por debajo que la categoría First
# Owner, mientras que la tercera posibilidad contemplada para esta variable,
# la de Third Owner, tuvo una frecuencia de tan solo 13 autos, siendo la que 
# menos frecuencia tuvo en toda la muestra, teniendo una diferencia de 31 y 30
# puntos con respecto a las categorías First y Second Owner respectivamente.

# VARIABLE: Mileage
# Como Mileage es una variable cuantitativa, procedemos a realizar un análisis
# descriptivo de la variable Mileage, para ello haremos un analisis parecido al
# que planteamos para la variable Year, haciendo un resumen de los datos y
# generando un histograma y un boxplot para visualizar la distribución de los
# mismos.

# Resumen de la variable Mileage:
summary(Mileage)
sd(Mileage)
IQR(Mileage)
quantile(Mileage, c(0.25, 0.5, 0.75))

# Gráficas de la variable Mileage:
hist(Mileage, main = "Histograma de la variable Mileage", xlab = "Mileage", 
     ylab = "Frecuencia", col = "darkred", las=1, ylim = c(0, 35))

boxplot(Mileage, main = "Boxplot de la variable Mileage", xlab = "Mileage", 
        col = "red", horizontal=TRUE)

# Analisis de la variable Mileage:
# El promedio de millas por galón de los carros es de 17.21, con una desviación
# estándar de 3.309902 y una mediana de 17.00, siendo el valor mínimo de 10.00
# y el valor máximo de 25.00. 

# En cuanto al rango intercuartílico, este es de 4 millas por galón, siendo el
# 25% de los datos menores a 15.00, el 50% menores a 17.00 y el 75% menores a
# 19.00. Notándose que la mayor parte de los datos se encuentran en el rango de
# 15.00 a 19.00 millas por galón.

# En cuando a la distribución de los datos, tanto en el histograma como en el
# boxplot se observa que los datos están distribuidos aparentemente de forma
# simétrica, sin presencia de valores atípicos en el boxplot de la muestra para
# esta variable.

# Finalmente, tanto en el histograma como en el boxplot se puede observar que la
# distribución de los datos es casi simétrica, y esto lo podemos afirmar, ya que
# a pesar de que visualmente el boxplot muestra que la mediana divide en dos 
# partes iguales la caja, lo cierto es que de ser una dsitribución simétrica 
# en toda regla, el valor de la mediana debería coincidir con el de la media,
# sin embargo, esto no es así en este caso, existe una sutil diferencia entre 
# ellas siendo la mediana de 17.00 km y la media de 17.21 km, es decir, hay un 
# margen de 0.21 km entre ambas medidas, sin embargo esta diferencia es tan
# pequeña que no afecta la interpretación de la simetría de la distribución,
# además de que en este caso, a diferencia de la variable Kilometers_Driven, no
# se observa la presencia de valores atípicos en la muestra, lo que hace 
# que la distribución de los datos sea más homogénea y simétrica.

# VARIABLE: Engine
# Como Engine es una variable cuantitativa, procedemos a realizar un análisis
# descriptivo de la variable Engine, para ello haremos un analisis parecido al
# anterior:

# Resumen de la variable Engine:
summary(Engine)
sd(Engine)
IQR(Engine)
quantile(Engine, c(0.25, 0.5, 0.75))

# Gráficas de la variable Engine:
hist(Engine, main = "Histograma de la variable Engine", xlab = "Engine", 
     ylab = "Frecuencia", col = "darkorange", las=1, ylim = c(0, 50))

boxplot(Engine, main = "Boxplot de la variable Engine", xlab = "Engine", 
        col = "orange", horizontal=TRUE)

# Analisis de la variable Engine:
# El promedio de la cilindrada de los carros es de 1855 cc, con una desviación
# estándar de 631.3115 cc y una mediana de 1774 cc, siendo el valor mínimo de 
# 999 cc y el valor máximo de 4951 cc.

# En cuanto al rango intercuartílico, este es de 681 cc, siendo el 25% de los
# datos menores a 1462 cc, el 50% menores a 1774 cc y el 75% menores a 2143 cc.
# Notándose que la mayor parte de los datos se encuentran en el rango de 1462 a
# 2143 cc.

# En cuando a la distribución de los datos, tanto en el histograma como en el
# boxplot se observa que los datos están distribuidos de forma asimétrica,
# notándose un ligero desplazamiento de la mediana a la izquierda del boxplot
# de la muestra para esta variable, lo que indica que la distribución de los
# datos es asimétrica positiva, es decir, que los datos se encuentran más
# dispersos entre el segundo y tercer cuartil cuartil, y más concentrados entre
# el primer y segundo cuartil. Además, podemos evidenciar que la distribución
# de los datos no es homogénea, ya que se observa la presencia de un valor
# atípico el cual corresponde con una cilindrada de 4951 cc, siendo el valor 
# más alto de la muestra observado una sola vez en el Car_ID 3.

# Finalmente, podemos mencionar que a raíz de la asimetría de la distribución
# existe una sutil diferencia entre los valores de la media y la mediana, siendo
# la mediana de 1774 cc y la media de 1855 cc, es decir, hay un 
# margen de 81 puntos entre ambas medidas.


# VARIABLE: Power
# Esta variable también es cuantitativa, por lo que procedemos a realizar un 
# análisis descriptivo por medio de summary, sd, IQR y quantile, así como a
# generar un histograma y un boxplot para visualizar la distribución de los
# datos.

# Resumen de la variable Power:
summary(Power)
sd(Power)
IQR(Power)
quantile(Power, c(0.25, 0.5, 0.75))

# Gráficas de la variable Power:
hist(Power, main = "Histograma de la variable Power", xlab = "Power", 
     ylab = "Frecuencia", col = "301934", las=1, ylim = c(0, 35))

boxplot(Power, main = "Boxplot de la variable Power", xlab = "Power", 
        col = "purple", horizontal=TRUE)

# Analisis de la variable Power:
# La potencia promedio de los carros de la muestra es de 158.1 hp, con una 
# desviación estándar de 76.96814 hp y una mediana de 138.1 hp, siendo el valor
# mínimo de 68.0 hp y el valor máximo de 396.0 hp.

# En cuanto al rango intercuartílico, este es de 84 hp, siendo el 25% de los
# datos menores a 103.00 hp, el 50% menores a 148 hp y el 75% menores a 187 hp.
# Notándose que la mayor parte de los datos se encuentran en el rango de 103 a
# 187 hp.

# En cuando a la distribución de los datos, tanto en el histograma como en el
# boxplot se observa que los datos están distribuidos de forma asimétrica,
# notándose un ligero desplazamiento de la mediana a la derecha del boxplot, 
# es decir, que se tiene una distribución asimétrica negativa, o sesgada a la 
# izquierda, lo que indica que los datos se encuentran más dispersos entre el
# primer y segundo cuartil cuartil, y más concentrados entre el segundo y tercer
# cuartil. Además, podemos evidenciar que la distribución de los datos no es
# homogénea, ya que se observa la presencia de varios valores atípicos a la 
# derecha del bigote superior del boxplot, los cuales corresponden con potencias
# sobre los 300 hp, siendo el valor más alto de la muestra el 396 hp, el cual
# fue observado en dos oportunidades, para el Car_ID 51 y el 89.

# Finalmente, podemos mencionar que a raíz de la asimetría de la distribución
# existe una sutil diferencia entre los valores de la media y la mediana, siendo
# la primera de 158.1 hp y la segunda de 148.0 hp, es decir, hay un margen de
# 10.1 hp entre ambas medidas. 

# VARIABLE: Seats
# Como Seats es una variable cuantitativa, procedemos a realizar un análisis
# descriptivo de la variable Seats, para ello haremos un analisis parecido al
# que planteamos para la variable anterior:

# Resumen de la variable Seats:
summary(Seats)
sd(Seats)
IQR(Seats)
quantile(Seats, c(0.25, 0.5, 0.75))

# Gráficas de la variable Seats:
hist(Seats, main = "Histograma de la variable Seats", xlab = "Seats", 
     ylab = "Frecuencia", col = "magenta", las=1, ylim = c(0, 80))

boxplot(Seats, main = "Boxplot de la variable Seats", xlab = "Seats", 
        col = "violet", horizontal=TRUE)

# Analisis de la variable Seats:
# El promedio de asientos de los carros de la muestra es de 5.23 asientos, con
# una desviación estándar de 0.7501515 asientos y una mediana de 5 asientos,
# siendo el valor mínimo de 4 asientos y el valor máximo de 7 asientos.

# En cuanto al rango intercuartílico, podemos apreciar que este es de 0, ya que 
# para este caso en particular tenemos que los datos estan extremadamente 
# agrupados, es decir, que los mismos están casi por completo agrupados en 
# el valor de 5, con muy pocos datos en otros valores, lo cual lo podemos ver
# evidenciado en que la caja resultante del boxplot es una "caja" que no es 
# visible ya que es extremadamente delgada, apreciandose únicamente la 
# mediana ubicada en 5, así mismo, el histograma nos hace hacernos una idea de 
# porqué está pasando esto, ya que vemos la diferencia que existe entre los 
# autos que presentan 5 asientos y el resto de los autos, siendo estos últimos
# una clara minoría de la muestra.

# En cuando a la distribución de los datos, vemos que a consecuencia de la 
# agrupación de los datos que se mencionó anteriormente, tenemos que el otro 
# dato relevante que puede sacarse del boxplot es la presencia de valores 
# atípicos, los cuales corresponden con autos 4 observaciones que poseen 4 
# asientos, y 14 autos que poseen 7 asientos, siendo estos valores atípicos
# debido a que el resto de los autos de la muestra tienen exactamente 5 asientos
# lo cual puede ser un indicio de que esta variable presenta una distribución
# uniforme.

# VARIABLE: Price
# Como Price es una variable cuantitativa, procedemos a realizar un análisis
# descriptivo de la variable Price, para ello haremos un analisis parecido al
# que planteamos para la variable anterior:

# Resumen de la variable Price:
summary(Price)
sd(Price)
IQR(Price)
quantile(Price, c(0.25, 0.5, 0.75))

# Gráficas de la variable Price:
hist(Price, main = "Histograma de la variable Price", xlab = "Price", 
     ylab = "Frecuencia", col = "darkcyan", las=1, ylim = c(0, 40))

boxplot(Price, main = "Boxplot de la variable Price", xlab = "Price", 
        col = "cyan", horizontal=TRUE)

# Analisis de la variable Price:
# El precio promedio de los carros de la muestra es de 1574000 unidades, con una
# desviación estándar de 1000265 unidades y una mediana de 1300000 unidades,
# siendo el valor mínimo de 450000 unidades y el valor máximo de 4000000 
# unidades.

# En cuanto al rango intercuartílico, este es de 1800000 unidades, siendo el 25%
# de los datos menores a 700000 unidades, el 50% menores a 1300000 unidades y el
# 75% menores a 2500000 unidades. Notándose que la mayor parte de los datos se
# encuentran en el rango de 700000 a 2500000 unidades.

# En cuando a la distribución de los datos, tanto en el histograma como en el
# boxplot se observa que los datos están distribuidos de forma asimétrica,
# notándose un ligero desplazamiento de la mediana a la izquierda del boxplot,
# lo que significa que la distribución de los datos es asimétrica positiva, o
# sesgada a la derecha, lo que indica que los datos se encuentran más dispersos
# entre el segundo y tercer cuartil cuartil, y más concentrados entre el primer
# y segundo cuartil. Además, podemos evidenciar que la distribución de los datos
# es homogénea, ya que no se observa la presencia de valores atípicos en la
# muestra para esta variable.

# Finalmente, podemos mencionar que a raíz de la asimetría de la distribución
# existe una sutil diferencia entre los valores de la media y la mediana, siendo
# la primera de 1574000 unidades y la segunda de 1300000 unidades, es decir, hay
# un margen de 274000 unidades entre ambas medidas.


# Generamos una matriz de correlacion
correlacion(Datos_car)

# Eliminamos uno por uno los rasgos para ajustar el modelo de regresion multiple
# y ver como afecta el ajuste del modelo