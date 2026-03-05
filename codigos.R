#########################################################################
#########################################################################
# Importar base de datos
#########################################################################
#########################################################################

#devtools::install_github("centromagis/paqueteMODELOS", force = TRUE)
#install.packages("rlang")
#install.packages("broom")
library(paqueteMODELOS)

data("vivienda")
data <- vivienda
str(data)

#########################################################################
#########################################################################
# Analisis exploratorio de datos
#########################################################################
#########################################################################

#########################################################################
# Datos repetidos
########################################################################

duplicados <- duplicated(data)
sum(duplicados)
data1 <- data[!duplicados, ]

#########################################################################
# Inconsistencias cuantitativos
########################################################################

#Mirando cuales valores son mayores a 0

num_cols <- sapply(data1, is.numeric)

conteo_mayor_0 <- sapply(data1[, num_cols, drop = FALSE],
                         function(x) sum(x >= 0, na.rm = TRUE))

conteo_mayor_0

# Longitud

freq_long <- summarytools::freq(data1$longitud,cumul = FALSE)
freq_long

# Latitud

freq_lati <- summarytools::freq(data1$latitud,cumul = FALSE)
freq_lati

#########################################################################
# Inconsistencias cualitativas
########################################################################

#Freq zona
freq_zona <- summarytools::freq(data1$zona,cumul = FALSE)
freq_zona

#freq piso
freq_piso <- summarytools::freq(data1$piso,cumul = FALSE)
freq_piso

#freq tipo
freq_tipo <- summarytools::freq(data1$tipo,cumul = FALSE)
freq_tipo

#freq barrio
freq_barrio <- summarytools::freq(data1$barrio,cumul = FALSE)
freq_barrio

##########################################################################
### Se encontro en columna barrio valores incoherentes
#########################################################################

library(stringr)
data1$barrio <- data1$barrio %>%
  str_squish() %>%   # quita dobles espacios y espacios al inicio/fin
  str_to_lower()     # todo en minúsculas

library(stringi)

data1$barrio <- stri_trans_general(data1$barrio, "Latin-ASCII")

data1$barrio <- ifelse(data1$barrio == "valle del lili",
                       "valle de lili",
                       data1$barrio)

data1$barrio <- ifelse(data1$barrio == "tequendema",
                       "tequendama",
                       data1$barrio)
data1$barrio <- ifelse(data1$barrio == "rep√∫blica de israel",
                       "republica de israel",
                       data1$barrio)

data1$barrio <- ifelse(data1$barrio == "portada de comfandi",
                       "portales de comfandi",
                       data1$barrio)

data1$barrio <- ifelse(data1$barrio == "samanes",
                       "samanes de guadalupe",
                       data1$barrio)

data1$barrio <- ifelse(data1$barrio == "ponce",
                       "pance",
                       data1$barrio)

data1$barrio <- ifelse(data1$barrio == "mel√(C)ndez",
                       "melendez",
                       data1$barrio)

data1$barrio <- ifelse(data1$barrio == "las am√(C)ricas",
                       "las americas",
                       data1$barrio)

data1$barrio <- ifelse(data1$barrio == "laflora",
                       "la flora",
                       data1$barrio)

data1$barrio <- ifelse(data1$barrio == "juanamb√∫",
                       "juanambu",
                       data1$barrio)

data1$barrio <- ifelse(data1$barrio == "el tr√(C)bol",
                       "el trebol",
                       data1$barrio)

data1$barrio <- ifelse(data1$barrio == "ciudad mel√(C)ndez",
                       "ciudad melendez",
                       data1$barrio)

data1$barrio <- ifelse(data1$barrio == "base a√(C)rea",
                       "base aerea",
                       data1$barrio)

data1$barrio <- ifelse(data1$barrio == "alf√(C)rez real",
                       "alferez real",
                       data1$barrio)

# se elimino fila de barrio santa porque no se sabe de cual pertenece
data1 <- data1[data1$barrio != "santa" & !is.na(data1$barrio), ]
data1 <- data1[data1$barrio != "norte" & !is.na(data1$barrio), ]

summarytools::freq(data1$barrio,cumul = FALSE)

########################################################################
# Datos atipicos
########################################################################

# Variables cuantitativas
nombre_cuantitativas <- c(
  "estrato", "preciom", "areaconst", "parqueaderos", "banios", "habitaciones", 
  "longitud", "latitud")

# numero de datos
n_valores_data1 <- colSums(!is.na(data1[, nombre_cuantitativas, drop = FALSE]))
n_valores_data1

# Media aritmetica
promedios <- sapply(
  data1[, nombre_cuantitativas, drop = FALSE],
  function(x) mean(x, na.rm = TRUE)
)

promedios

# Desviacion estandar
desv_std <- sapply(
  data1[, nombre_cuantitativas, drop = FALSE],
  function(x) sd(x, na.rm = TRUE)
)

desv_std

#IQR
iqr_vals <- sapply(
  data1[, nombre_cuantitativas, drop = FALSE],
  function(x) IQR(x, na.rm = TRUE)
)

iqr_vals

#Q1
q1_vals <- sapply(
  data1[, nombre_cuantitativas, drop = FALSE],
  function(x) quantile(x, probs = 0.25, na.rm = TRUE)
)

q1_vals

#Q3
q3_vals <- sapply(
  data1[, nombre_cuantitativas, drop = FALSE],
  function(x) quantile(x, probs = 0.75, na.rm = TRUE)
)

q3_vals

# Minimo

min_vals <- sapply(
  data1[, nombre_cuantitativas, drop = FALSE],
  function(x) min(x, na.rm = TRUE)
)

min_vals

# Maximo 

max_vals <- sapply(
  data1[, nombre_cuantitativas, drop = FALSE],
  function(x) max(x, na.rm = TRUE)
)

max_vals


#Coeficiente de variacion
cv_vals <- sapply(
  data1[, nombre_cuantitativas, drop = FALSE],
  function(x) {
    m <- mean(x, na.rm = TRUE)
    s <- sd(x, na.rm = TRUE)
    if (is.na(m) || m == 0) NA else (s / m) * 100
  }
)

cv_vals

# Asimetria

library(e1071)

asimetria_vals <- sapply(
  data1[, nombre_cuantitativas, drop = FALSE],
  function(x) skewness(x, na.rm = TRUE, type = 2)
)

asimetria_vals

# Curtosis
curtosis_vals <- sapply(
  data1[, nombre_cuantitativas, drop = FALSE],
  function(x) kurtosis(x, na.rm = TRUE, type = 2)
)

curtosis_vals

#############################################################
# data1 ANTES IMPUTACION
#Creacion de la tabla de estadistica descriptiva de las
#variables cuantitativas y hallar valores atipicos
#############################################################

tabla <- data.frame(
  "Variables cuantitativas" = nombre_cuantitativas,
  N <- n_valores_data1,
  Promedio <- round(promedios,3),
  "Desv. Estandar" <- round(desv_std,3),
  IQR <- iqr_vals,
  Q1 <- q1_vals,
  Q3 <- q3_vals,
  Minimo <- min_vals,
  Maximo <- max_vals,
  "CV (%)" <- round(cv_vals),
  Asimetria <- asimetria_vals,
  Curtosis <- curtosis_vals
)

colnames(tabla) <- c(
  "Variables cuantitativas","N", "Promedio", "Desv. Estandar", "IQR", "Q1", "Q3", 
  "Minimo", "Maximo", "CV (%)", "Asimetria", "Curtosis"
)

kable(tabla, caption = "Descripción y clasificación de variables de acuerdo con su naturaleza.")


########################################################################
# Datos faltantes
########################################################################

library(mice)
md.pattern(data1)

#Suma de datos faltantes
faltantes_por_col <- colSums(is.na(data1))
faltantes_por_col

#Porcentaje de datos faltantes
pct_faltantes <- sapply(data1, function(x) mean(is.na(x)) * 100)
pct_faltantes

##############################################################################
# Test little
#############################################################################
# install.packages("naniar")  # si no la tienes
library(naniar)

data1_num <- data1[, sapply(data1, is.numeric), drop = FALSE]
mcar_test(data1_num)


############################################################################
# No MCAR asi que toca mirar relacion piso con resto de cuantitativas
# y mirar relacion parqueadero para imputacion multiple
############################################################################


#Mirar relacion piso y cuantitativas

num_vars <- names(data1)[sapply(data1, is.numeric)]

medias_por_falta_piso <- sapply(
  data1[, num_vars, drop = FALSE],
  function(x) tapply(x, is.na(data1$piso), mean, na.rm = TRUE)
)

t(medias_por_falta_piso)  # queda más legible

#

medias_por_falta_parq <- sapply(
  data1[, num_vars, drop = FALSE],
  function(x) tapply(x, is.na(data1$parqueaderos), mean, na.rm = TRUE)
)

t(medias_por_falta_parq)

# Lo mas probable es que sea del tipo MAR asi que se procede a hacer 
#imputacion multiple

################################################################################
# Imputacion multiple
################################################################################

# Asegura tipos
data1$piso <- as.factor(data1$piso)           # cualitativa
data1$parqueaderos <- as.numeric(data1$parqueaderos)  # cuantitativa

ini  <- mice(data1, maxit = 0)
meth <- ini$method
pred <- ini$predictorMatrix

# Imputación:
meth["parqueaderos"] <- "pmm"     # numérica
meth["piso"]         <- "polyreg" # cualitativa (multiclase)

# (Opcional) evita usar el id como predictor si existe
if ("id" %in% colnames(pred)) {
  pred[, "id"] <- 0
}

imp <- mice(data1, method = meth, predictorMatrix = pred, m = 5, seed = 123)

data2 <- complete(imp, 1)

library(ggplot2)

ggplot() +
  geom_density(data = data1, aes(x = parqueaderos), color = "blue", na.rm = TRUE) +
  geom_density(data = data2, aes(x = parqueaderos), color = "red",  na.rm = TRUE) +
  labs(title = "Densidad: data1 vs data2", x = "parqueaderos", y = "densidad")
