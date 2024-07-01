# CO3321 Estadística para Ingenieros
# Proyecto(15%).R
# Baudilio Velasquez - Carnet: 18-10665
# Juan Cuevas        - Carnet: 19-10056
# Anya Marcano       - Carnet: 19-10336

source("linear_reg.R")
source("linear_reg.R")
library(corrplot)
library(MASS)
library(lmtest)

# Cargamos la base de datos suministrada
library(readxl)
Datos_car <- read_excel("./Datos_car.xlsx")
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

# 2. ¿Es cierto que la potencia promedio del carro es diferente si utiliza gasoil 
# o gasolina? (Realice previamente un análisis descriptivo y luego la prueba de 
# hipótesis adecuada. Apóyese en gráficos para su interpretación).

# Para responder a esta pregunta, primero realizaremos un análisis descriptivo
# de la variable Power, separando los datos en dos grupos, uno para los carros
# que utilizan gasolina y otro para los carros que utilizan gasoil, para ello
# usaremos la función subset para separar los datos y luego generaremos un
# histograma y un boxplot para visualizar la distribución de los datos.

# Boxplot para la relación potencia - combustible:
boxplot(split(Datos_car$Power, Datos_car$Fuel_Type), main = 
          "Relacion Potencia - Combustible", xlab="Potencia", ylab="Combustible", 
        col = c("cyan", "green", "purple"), horizontal = T, ylim = c(0, 500),
        xaxt = "n")

axis(side = 1, at = seq(0, 500, by = 20), labels = seq(0, 500, by = 20))

# Datos específicos para cada combustible en relación a la potencia:

# Gasolina (Petrol):
Potencia_Gasolina = Datos_car$Power[Datos_car$Fuel_Type=="Petrol"]

# Resumen de la variable
summary(Potencia_Gasolina)
sd(Potencia_Gasolina)
quantile(Potencia_Gasolina, c(0.25, 0.5, 0.75))

# Boxplot Individual de la variable
boxplot(Potencia_Gasolina, main = "Boxplot de Potencia con gasolina", 
        xlab = "Potencia", col = "cyan", horizontal = T, xaxt = "n")
axis(side = 1, at = seq(0, 500, by = 10), labels = seq(0, 500, by = 10))

# Analisis:
# Vemos que, en cuanto a la relación que existe entre la potencia y el uso de 
# gasolina como combustible, tenemos principalmente que en este caso la potencia
# promedio es de 157.5 con una desviación estándar de 97.67082 y una mediana de
# 110.5

# La potencia mínima bajo estas condiciones es de 68, mientras que la máxima es 
# de 396. El 25% de los autos que usaron gasolina tuvieron una potencia de menos
# de 94,el 50% tuvieron su potencia menor a 110.50 y el 75% tuvo una potencia
# inferior a 177.25, con lo cual, la mayor parte de los datos se encuentran 
# en el rango de 94 a 177.25 de potencia.

# Así mismo, gracias al boxplot podemos evidenciar claramente la presencia de
# al menos 4 datos atípicos de entre los autos que fueron tomados en cuenta en 
# la muestra analizada, los cuales corresponden con valores de potencia de: 396,
# 395, 362 y 335. Además, el gráfico también nos permite evidenciar con 
# facilidad que existe una distribución asimétrica de los datos, siendo sesgada
# a la derecha, lo cual indica que la mayoría de los datos se encuentran más 
# dispersos en el rango de 110.5 a 396 de potencia y más concentrados en el
# rango de 68 a 110.5 de potencia.

# Gasoil (Diesel):
Potencia_Gasoil = Datos_car$Power[Datos_car$Fuel_Type=="Diesel"]

# Resumen de la variable
summary(Potencia_Gasoil)
sd(Potencia_Gasoil)
quantile(Potencia_Gasoil, c(0.25, 0.5, 0.75))

# Boxplot Individual de la variable
boxplot(Potencia_Gasoil, main = "Boxplot de Potencia con gasoil", 
        xlab = "Potencia", col = "green", horizontal = T, xaxt = "n")
axis(side = 1, at = seq(0, 500, by = 10), labels = seq(0, 500, by = 10))

# Análisis:
# Vemos que en cuanto a la relación que existe con la potencia y el uso de 
# gasoil/Diesel como combustible, tenemos principalmente que la potencia 
# promedio fue de 158.8 con una desviación estándar de 46.10887 y una mediana
# de 170.00.

# En este caso, la potencia mínima fue de 69 mientras que la máxima fue de 261, 
# lo cual representa un rango de potencia menor que el de los autos que usaron
# gasolina como combustible. 

# Aquí, 25% de los autos tuvieron una potencia de menos de 126, el 50% tuvo su
# potencia menor a 170 y el 75% tuvo una potencia inferior a 187, con lo cual,
# la mayor parte de los datos se encuentran en el rango de 126 a 187 de potencia
# es decir, en este caso tenemos un rango intercuartílico de 61, lo cual es
# menor que el rango intercuartílico de los autos que usaron gasolina, el cual 
# fue de 83.
# Adicionalmente, vemos quem, adiferencia de lo ocurrido con la gasolina, usando
# gasoil no hay presencia de datos atípicos en el boxplot manteniéndose también 
# una distribución asimétrica pero en este caso sesgada a la izquierda, lo cual
# indica que la mayoría de los datos se encuentran más dispersos en el rango
# de 126 a 170 de potencia y más concentrados en el rango de 170 a 187 de
# potencia.

# Conclusión Preliminar:
# A partir de los análisis descriptivos realizados, podemos decir que la 
# potencia promedio de los carros que utilizan gasolina es de 157.5 hp, mientras
# que la potencia promedio de los carros que utilizan gasoil es de 158.8 hp,
# por lo que, en efecto la potencia promedio de los carros que utilizan gasoil
# y los que utilizan gasolina es diferente, sin embargo, para poder afirmar esto
# con certeza, es necesario realizar una prueba de hipótesis que nos permita
# determinar si esta diferencia es significativa o no.

# Prueba de Hipótesis:
# Para realizar la prueba de hipótesis, planteamos las siguientes hipótesis:
# H0: La potencia promedio de los carros que utilizan gasolina es igual a la
#     potencia promedio de los carros que utilizan gasoil.
# Ha: La potencia promedio de los carros que utilizan gasolina es diferente a la
#     potencia promedio de los carros que utilizan gasoil.

# Para realizar la prueba de hipótesis, usaremos la función t.test, la cual nos
# permitirá realizar una prueba t de dos muestras independientes, para ello
# usaremos las muestras de potencia de los carros que utilizan gasolina y los
# que utilizan gasoil, y estableceremos un nivel de significancia del 1%, es
# decir, un nivel de confianza del 99%, lo cual nos permitirá rechazar la
# hipótesis nula si el p-valor es menor a 0.01

# Prueba de Hipótesis:
t.test(Potencia_Gasolina, Potencia_Gasoil, alternative = "two.sided", 
       mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.99)

# Resultados obtenidos: 
# Welch Two Sample t-test ------------------------------------------------------
# data:  Potencia_Gasolina and Potencia_Gasoil
# t = -0.086971, df = 73.923, p-value = 0.9309
# alternative hypothesis: true difference in means is not equal to 0
# 99 percent confidence interval:
#  -41.21354  38.58854
# sample estimates:
# mean of x mean of y 
# 157.5000  158.8125 

# Conclusión:
# A partir de los resultados obtenidos de la prueba de hipótesis, podemos decir
# que, como el p-valor obtenido es de 0.9309, el cual es mayor al nivel de
# significancia establecido, el cual fue de 0.01, no tenemos suficiente 
# evidencia para rechazar la hipótesis nula, por lo que no podemos afirmar que
# la potencia promedio de los carros que utilizan gasolina es diferente a la
# potencia promedio de los carros que utilizan gasoil. Esto indica que no hay 
# evidencia suficiente para afirmar que existe una diferencia significativa en 
# las medias de potencia entre los carros que usan gasolina y aquellos que usan
# gasoil, incluso con un nivel de confianza del 99%, por lo que podemos decir 
# que, desde el punto de vista estadístico, las potencias medias de ambos 
# podrían considerarse equivalentes con el nivel de confianza utilizado.

# PARTE 2 - MODELO DE REGRESIÓN LINEAL -----------------------------------------
# Se quiere ajustar un modelo de regresión lineal múltiple, donde la variable 
# dependiente es el precio de los carros (Price)

plot(Datos_car)

# Eliminamos la variable Car_ID ya que no aporta información relevante
Datos_car$Car_ID <- NULL

# Transformamos la variable Bran en una variable cuantitativa.
Datos_car$Brand <- as.numeric(factor(Datos_car$Brand))

# Transformamos la variable Model en una variable cuantitativa.
Datos_car$Model <- as.numeric(factor(Datos_car$Model))

# Transformamos la variable Fuel_Type en una variable cuantitativa.
Datos_car$Fuel_Type <- as.numeric(factor(Datos_car$Fuel_Type))

# Transformamos la variable Transmission en una variable cuantitativa.
Datos_car$Transmission <- as.numeric(factor(Datos_car$Transmission))

# Transformamos la variable Owner_Type en una variable cuantitativa.
# Valores actuales: First, Second, & Third
# Nuevos valores: 1, 2, & 3
Datos_car$Owner_Type <- as.numeric(factor(Datos_car$Owner_Type))

# 1. Calcule y grafique la matriz de correlación. Interprete los resultados.                       # FALTA LA GRAFICA
# ---------------------------------------------------------------------------------------------------------------------
# Generamos una matriz de correlación

correlacion(Datos_car)

cor(Datos_car)

# Análisis de la matriz de correlación:-----------------------------------------
# Observando la matriz de correlación tenemos lo siguiente:
  
# 1) Variables con alta correlación con Price:
# Engine: 0.7144648 (correlación positiva)
# Power: 0.85661983 (correlación positiva)
# Estas variables muestran una fuerte correlación positiva con Price, 
# lo que indica que a medida que el valor de Engine o Power aumenta, 
# el Price tiende a aumentar también.

# 2) Variables con correlación negativa significativa con Price:
  
# Mileage: -0.59525201
# Owner_Type: -0.3014882349
# Transmission:  -0.67648351

# Mileage y Transmission tienen una correlación negativa moderada a fuerte con 
# Price, lo que sugiere que a medida que el valor de estas variables aumenta, 
# el precio tiende a disminuir. Owner_Type también muestra una correlación 
# negativa con Price, aunque menos fuerte que las anteriores.

# 3) Variables con baja o ninguna correlación significativa con Price:
  
# Brand: -0.35454254
# Model: -0.29243416
# Year: -0.23268700
# Kilometers_Driven: -0.0511040544
# Fuel_Type: -0.180963587
# Seats: -2.692346e-05

# Aunque Brand, Model, Year, y Fuel_Type tienen correlaciones negativas con 
# Price, son relativamente bajas en comparación con otras variables. Por otro
# lado, Kilometers_Driven y Seats tienen correlaciones muy bajas o nulas con 
# Price, lo que sugiere que podrían no ser predictores útiles en el modelo.
# Siendo así, estas variables podrían ser eliminadas del modelo para simplificar
# el análisis.

# En resumen, las variables Engine, Power, Mileage, Owner_Type, y Transmission
# parecen ser los predictores más fuertes de Price, y podrían ser buenos
# candidatos para incluir en el modelo de regresión lineal múltiple.

# 2. Ajuste un modelo de regresión lineal múltiple. Para ello:
#     a. Realice la eliminación de las variables paso a paso.
#     b. En cada paso realice las pruebas de significancia de los parámetros.
#     c. Realice transformadas de la(s) variable(s) en caso de ser necesario.

# Vamos a ajustar el modelo: Price ~ Engine + Power + Mileage + Owner_Type +
#                                             Transmission

# Eliminamos uno por uno los rasgos descartados para ajustar el modelo de 
# regresion múltiple y ver como afecta el ajuste del modelo

# Eliminamos la variable Brand
Datos_car$Brand <- NULL

# Eliminamos la variable Model 
Datos_car$Model <- NULL

# Eliminamos la variable Fuel_Type
Datos_car$Fuel_Type <- NULL

# Eliminamos la variable Year
Datos_car$Year <- NULL

# Eliminamos la variable Kilometers_Driven
Datos_car$Kilometers_Driven <- NULL

# Eliminamos la variable Seats
Datos_car$Seats <- NULL

# Para realizar las pruebas de significancia de los parámetros, usaremos la
# función summary, la cual nos proporciona información sobre los coeficientes
# estimados, los errores estándar, los valores t y los valores p asociados a
# cada variable en el modelo.

# Ajustamos el modelo completo
modelo_completo <- lm(Price ~ Engine + Power + Mileage + Owner_Type + 
                        Transmission, data = Datos_car)

# Y verificamos la significancia de las variables con un resumen del modelo

summary(modelo_completo)

# Resultado obtenido:

# Call:
#  lm(formula = Price ~ Engine + Power + Mileage + Owner_Type + 
#       Transmission, data = Datos_car)

# Residuals:
#  Min       1Q   Median       3Q      Max 
# -1597521  -107770     5917   236596   854764 

# Coefficients:
#             Estimate Std. Error   t value   Pr(>|t|)    
# (Intercept)  1915050.4   582189.3  3.289    0.00141 ** 
#   Engine        65.3      132.1    0.494    0.62221    
#   Power       7356.0     1150.4    6.394    6.20e-09 ***
#  Mileage     -28651.0    19625.3  -1.460    0.14765    
# Owner_Type   -175381.1   69210.4  -2.534    0.01293 *  
# Transmission -584563.1   112753.9  -5.184  1.24e-06 ***
#  ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 454400 on 94 degrees of freedom
# Multiple R-squared:  0.804,	Adjusted R-squared:  0.7936 
# F-statistic: 77.14 on 5 and 94 DF,  p-value: < 2.2e-16

# Ahora, veamos cuales de las variables son significativas para el modelo, esto 
# lo haremos evaluando si el p-valor de cada variable es menor a 0.05, en cuyo
# caso la variable es significativa para el modelo.

# Intercepto:    0.00141 < 0.05
# Engine:        0.62221 > 0.05
# Power:        6.20e-09 < 0.05
# Mileage:       0.14765 > 0.05
# Owner_Type:    0.01293 < 0.05
# Transmission: 1.24e-06 < 0.05

# De acuerdo a los resultados obtenidos, podemos decir que las variables
# significativas para el modelo son: Power, Owner_Type y Transmission, mientras
# que las variables Engine y Mileage no son significativas para el modelo.

# Ajustamos un nuevo modelo sin la variable Engine

modelo_ajustado <- lm(Price ~ Power + Mileage + Owner_Type + Transmission, 
                      data = Datos_car)

# Y verificamos la significancia de las variables con un resumen del modelo

summary(modelo_ajustado)

# Resultado obtenido:
# Call:
# lm(formula = Price ~ Power + Mileage + Owner_Type + Transmission, 
#   data = Datos_car)
#
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1502065  -114886     7834   237593   854995 
# 
# Coefficients:
#             Estimate Std.  Error  t value   Pr(>|t|)    
# (Intercept)  2047292.8   515033.1   3.975  0.000137 ***
#   Power         7691.1      925.8   8.308  6.73e-13 ***
#   Mileage     -32162.6    18221.7  -1.765  0.080766 .  
# Owner_Type   -180308.2    68216.1  -2.643  0.009607 ** 
# Transmission -581297.0   112111.7  -5.185  1.22e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 452600 on 95 degrees of freedom
# Multiple R-squared:  0.8035,	Adjusted R-squared:  0.7953 
# F-statistic: 97.13 on 4 and 95 DF,  p-value: < 2.2e-16

# Ahora, veamos cuales de las variables son significativas para el modelo, esto
# lo haremos evaluando si el p-valor de cada variable es menor a 0.05, en cuyo
# caso la variable es significativa para el modelo.

# Intercepto:    0.000137 < 0.05
# Power:         6.73e-13 < 0.05
# Mileage:       0.080766 > 0.05
# Owner_Type:    0.009607 < 0.05
# Transmission:  1.22e-06 < 0.05

# En este caso, hemos obtenido un modelo que explica el 79.53% de la variabilidad
# lo cual es una mejora en comparación con el modelo anterior, que solo 
# explicaba el 79.36% de la variabilidad.

# PLantearemos un nuevo modelo sin la variable Mileage

modelo_ajustado2 <- lm(Price ~ Power + Owner_Type + Transmission, 
                       data = Datos_car)

# Y verificamos la significancia de las variables con un resumen del modelo

summary(modelo_ajustado2)

# Resultado obtenido:
# Call:
# lm(formula = Price ~ Power + Owner_Type + Transmission, data = Datos_car)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1486721  -131674     5597   301892   816950 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1294948.2   292266.3   4.431 2.49e-05 ***
#   Power           8671.5      748.7  11.581  < 2e-16 ***
#   Owner_Type   -166585.4    68514.3  -2.431   0.0169 *  
#   Transmission -566883.1   113039.1  -5.015 2.43e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 457600 on 96 degrees of freedom
# Multiple R-squared:  0.7971,	Adjusted R-squared:  0.7907 
# F-statistic: 125.7 on 3 and 96 DF,  p-value: < 2.2e-16
                                                                                #FALTA ANALISIS FINAL Y DECIDIR SI MANTENER 
                                                                                #AJUSTADO O AJUSTADO2 PORQUE EL % DE EXPLICACION DE 
                                                                                #VARIABILIDAD BAJA EN EL AJUSTADO2
#---------------------------------------------------------------------------------------------------------

# c) Transformadas de la(s) variable(s) en caso de ser necesario:

# Las transformaciones necesarias ya fueron realizadas en el análisis de la
# matriz de correlación, donde se eliminaron las variables que no eran
# significativas para el modelo.

# 3. Verifique que el modelo encontrado en el ítem anterior es el adecuado. 
# Muestre los gráficos necesarios. Para ello,
# i.   Verifique normalidad visualmente. Analice los resultados de acuerdo a 
#      lo observado.
# ii.  Para verificar la homocedasticidad (se puede basar en el estadístico de 
#      Durbin-Watson).
# iii. Verifique independencia de acuerdo a lo observado en las gráficas 
#      resultantes.

# Verificación de normalidad:
qqnorm(rstandard(modelo_ajustado2))
qqline(rstandard(modelo_ajustado2))

# Analisis: 
# En el gráfico Q-Q plot, los residuos se ajustan bastante bien a la línea
# diagonal, lo que sugiere que los residuos siguen una distribución normal,
# lo cual es un indicador de que el modelo es adecuado.
# Por lo tanto, podemos decir que el modelo es adecuado en términos de
# normalidad.

# Verificación de homocedasticidad:
plot(fitted.values(modelo_ajustado2), rstandard(modelo_ajustado2), 
     xlab = "Valores ajustados", ylab = "Residuos estandarizados", 
     main = "Homocedasticidad")

# Analisis:
# En el gráfico de residuos estandarizados vs valores ajustados, no se observa
# un patrón claro en la dispersión de los residuos, lo que sugiere que la
# varianza de los residuos es constante, lo cual es un indicador de que el
# modelo es adecuado en términos de homocedasticidad.

# Verificación de independencia:
# Para verificar la independencia, se debe hacer una gráfica para cada variable
# independiente. En este caso, las variables independientes son:
# - Power
# - Owner_Type
# - Transmission
# Entonces, se deben realizar 3 gráficos de los residuos estandarizados Vs la 
# variable independiente

# Gráfico de residuos estandarizados vs Power
plot(Datos_car$Power, rstandard(modelo_ajustado2), 
     xlab = "Power", ylab = "Residuos estandarizados", 
     main = "Independencia - Power")

# Gráfico de residuos estandarizados vs Owner_Type
plot(Datos_car$Owner_Type, rstandard(modelo_ajustado2), 
     xlab = "Owner_Type", ylab = "Residuos estandarizados", 
     main = "Independencia - Owner_Type")

# Gráfico de residuos estandarizados vs Transmission
plot(Datos_car$Transmission, rstandard(modelo_ajustado2), 
     xlab = "Transmission", ylab = "Residuos estandarizados", 
     main = "Independencia - Transmission")
                                                                                #FALTAN LOS ANALISIS INDIVIDUALES Xd
#-----------------------------------------------------------------------------------------------------------------------------

# Usando la prueba de Durbin-Watson, cuya prueba de hipótesis es
# H0: No existe correlación entre los residuos
# Ha: Los residuos están correlacionados

# Prueba de Durbin-Watson
dwtest(modelo_ajustado2)

# Resultado obtenido:
# Durbin-Watson test
# data:  modelo_ajustado2
# DW = 1.6184, p-value = 0.02793
# alternative hypothesis: true autocorrelation is greater than 0

# Conclusión:
# Como el p-valor obtenido es de 0.02793, el cual es menor al nivel de
# significancia establecido, el cual fue de 0.05, tenemos suficiente evidencia
# para rechazar la hipótesis nula, por lo que podemos decir que los residuos
# están correlacionados, sin embargo como el valor del estadístico de Durbin-
# Watson es de 1.6184, es decir se encuentra entre 1.5 y 2.5 podemos decir que
# la correlación es débil, por lo que podemos decir que el modelo es adecuado
# en términos de independencia y que la correlación entre los residuos no 
# representa un motivo de preocupación.

# Problemas detectados -> En las graficas de independencia se observa un
# comportamiento lineal en los residuos, lo cual puede ser un indicio de que
# existe una relación entre los residuos y las variables independientes, lo cual
# podría ser un problema en el modelo, en particular para las variables 
# Owner_Type y Transmission.



