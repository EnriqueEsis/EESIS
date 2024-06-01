library(tidyverse)      # Carga el paquete para manipulación de datos
library(readxl)         # Carga el paquete para leer archivos Excel
library(ggplot2)        # Carga el paquete para gráficos
library(dplyr)          # Carga el paquete para manipulación de datos
library(corrplot)       # Carga el paquete para visualizar la matriz de correlación
library(RColorBrewer)   # Carga el paquete para crear paletas de colores
library(ggpubr)         # Carga el paquete para gráficos combinados
library(ggplotify)      # Carga el paquete para convertir ggplot a plotly
library(visdat)         # Carga el paquete para visualizar datos faltantes
library(openxlsx)       # Carga el paquete para exportar archivos excel
library(readr)          # Carga el paquete para leer archivos csv
library(datasets)       # Carga un listado de dataset



# Cargar el archivo CSV desde ubicación de windows
ruta_archivo <-'C:\\Users\\EEsis\\Downloads\\Datasets\\Google-Playstore_T_R.csv'
df <- read.csv(ruta_archivo, sep = ",")

# Cargar datos profesora
#ruta_archivo <-'C:\\Users\\EEsis\\Documents\\Materias\\Analisis Estadistico\\Bases\\salaries.csv'
#df <- read.csv(ruta_archivo, sep = ";")


# Cargar el dataset iris en el objeto df
#df <- iris # WorldPhones, mtcars, iris, Titanic

# Ver tabla 
View(df)


# Visualización de una muestra de los datos en excel

# Ubicación donde guardar el archivo Excel
Nombre_Archivo <- "C:\\Users\\EEsis\\Documents\\Materias\\Programación R\\Bases\\muestra.xlsx"  # Cambia la ruta según tu ubicación

# Filtrar el conjunto de datos a las primeras 1000 filas
df_excel <- head(df, 500)

# Guarda el DataFrame en un archivo Excel en la ubicación especificada
write.xlsx(df_excel, file = Nombre_Archivo, rowNames = FALSE)



# Nombre y tipo de las columnas
str(df)

# nombre de las columnas
names(df)

# Sumar los valores nulos de cada columna
print(colSums(is.na(df)))



# Seleccionar solo las columnas numéricas
numeric_df <- select_if(df, is.numeric)

# Verificar valores numericos
summary(numeric_df)


#Crear un dataframe para ver los valores unicos de cada columna tipo chr del df 
# Obtener las columnas tipo chr
columnas_chr <- names(df)[sapply(df, is.character)]

# Crear un nuevo DataFrame con la misma cantidad de filas que el original
df_valores_unicos <- data.frame(matrix(NA, nrow = nrow(df), ncol = length(columnas_chr)))
colnames(df_valores_unicos) <- columnas_chr


# Iterar sobre las columnas de tipo chr
for (col in columnas_chr) {
  valores_unicos <- unique(df[[col]])
  df_valores_unicos[[col]] <- rep(NA, nrow(df))
  df_valores_unicos[[col]][1:length(valores_unicos)] <- valores_unicos
}

# Mostrar el nuevo DataFrame con los valores únicos de las columnas tipo chr
View(df_valores_unicos)


boxplot(df$Calificación ~ df$Categoría)

# Boxplot con nombres del eje x ajustados
ggplot(df, aes(x = Categoría, y = Calificación)) +
  geom_boxplot() +
  labs(x = "Categoría", y = "Calificación") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
