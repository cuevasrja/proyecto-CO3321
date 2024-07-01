# Funcion para calcular una matriz de correlacion
# entre las variables de un dataframe
# Input: dataframe
# Output: matriz de correlacion
correlacion <- function(dataframe){
  # Eliminamos las columnas que no son numericas
  numeric_dataframe <- dataframe[,sapply(dataframe, is.numeric)]
  return( round(cor(numeric_dataframe), 4))
}