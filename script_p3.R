
## PRÁCTICA 3: EFECTOS DE DIVERSOS FACTORES AMBIENTALES EN LA GERMINACIÓN DE PLANTAS

# Análisis de datos


# 0. Instalar paquetes ----------------------------------------------------
install.packages("tidyverse", dep = T)


# 1. Cargar paquetes ------------------------------------------------------
library(tidyverse)


# 2. Cargar datos ---------------------------------------------------------
# establecer directorio de trabajo
setwd("C:/Users/Usuario/Desktop/Ecología poblaciones")

# importar datos
hojarasca <- read.csv("./tabla_datos_aciculas.csv", header = T, sep = ",", dec = ".")

# testar variables (qué es numérico continuo?, qué es caracter o cadena de texto?, que es categórico o factor?)
str(hojarasca)

# transformar lo que sea necesario, en este caso, el tto_acículas lo lee como cadena de texto y debe ser categórico (con dos niveles, C -control- y A -acículas-)
hojarasca$tto_aciculas <- as.factor(hojarasca$tto_aciculas)

# comprobar de nuevo, a ver si ha salido bien
str(hojarasca)

# debemos separar los datos 
control <- hojarasca |> 
  filter(tto_aciculas == "C")

aciculas <- hojarasca |>  
  filter(tto_aciculas == "A")


# 3. Efecto de la presencia de acículas en la germinación -----------------

# 3.1. Primera asunción: normalidad de los datos --------------------------

# 3.1.1. Control ----------------------------------------------------------
# comprobar gráficamente con un histograma
hist(control$germinacion, 
     xlab = "Porcentaje de germinación", ylab = "Probabilidad", # añadir nombres de los ejes
     main = "", prob = TRUE, ylim = c(0,0.04))
lines(density(control$germinacion), col = "blue", lwd = 2)     # mostramos la probabilidad en el eje Y y aumentamos el límite para que se vea bien la curva
abline(v = mean(control$germinacion), col = "red")  # dibujamos una línea roja en la media

# comprobar con test de Shapiro.wilk
shapiro.test(control$germinacion)   # p > 0.05, aceptamos hipótesis nula, los datos son normales


# 3.1.2. Acículas ---------------------------------------------------------
# comprobar gráficamente con un histograma
hist(aciculas$germinacion, 
     xlab = "Porcentaje de germinación", ylab = "Probabilidad", # añadir nombres de los ejes
     main = "", prob = TRUE, ylim = c(0,0.04))
lines(density(aciculas$germinacion), col = "blue", lwd = 2)     # mostramos la probabilidad en el eje Y y aumentamos el límite para que se vea bien la curva
abline(v = mean(aciculas$germinacion), col = "red")  # dibujamos una línea roja en la media

# comprobar con test de Shapiro.wilk
shapiro.test(aciculas$germinacion)   # p > 0.05, aceptamos hipótesis nula, los datos son normales



# 3.2. Segunda asunción: homocedasticidad (homogeneidad de varianzas) -----

# test de Bartlett
bartlett.test(germinacion ~ tto_aciculas, data = hojarasca)  # p > 0.05, aceptamos hipótesis nula, los datos son homocedásticos

# la varianza de germinación en ambos tratamientos (acículas vs control) es igual



# 3.3. Test estadístico ---------------------------------------------------


# 3.3.1. Estadística paramétrica: t de Student ----------------------------
## los datos cumplen las asunciones (normalidad y homocedasticidad)

# representamos gráficamente los datos para ver dónde parece que hay mayor germinación
boxplot(control$germinacion, aciculas$germinacion, col = "red", ylab = "germinación", names = c("control", "aciculas"))

# test estadístico (dos colas)
t.test(control$germinacion, aciculas$germinacion)  # p < 0.05, rechazamos la hipótesis nula, hay diferencias entre ambos tratamientos

# si quiero plantear una hipótesis de una cola donde la hipótesis alternativa es que la germinación en suelo desnudo es mayor que la del suelo con acículas
t.test(control$germinacion, aciculas$germinacion, alternative = "greater")  

# en este caso el p-valor es la mitad que en el test de dos colas, porque solo estoy considerando uno de los dos posibles resultados

# test de una cola con la alternativa contraria
t.test(control$germinacion, aciculas$germinacion, alternative = "less")  



# 3.3.2. Estadística no paramétrica: test de Wilcox -----------------------
## los datos no cumplen las asunciones
wilcox.test(control$germinacion, aciculas$germinacion)



# 4. Efecto de la humedad sobre la germinación ----------------------------
# directorio de trabajo y cargar datos
setwd("C:/Users/Usuario/Desktop/Ecología poblaciones")
humedad <- read.csv("./tabla_datos_humedad.csv", header = T, sep = ",", dec = ".")

# testar variables (qué es numérico continuo?, qué es caracter o cadena de texto?, que es categórico o factor?)
str(humedad)


# 4.1. Regresión lineal ---------------------------------------------------

modelo <- lm(germinacion ~ potencial_osmotico, data = humedad)

# para ver los coeficientes del modelo
modelo$coefficients

# el primer valor es el intercepto y el segundo la pendiente de la recta

# para ver la significación usamos un test correlación con “cor.test”
cor.test( ~ germinacion + potencial_osmotico, data = humedad, method = "pearson", conf.level = 0.95)


# representamos gráficamente la nube de puntos
plot(humedad$potencial_osmotico, humedad$germinacion, xlab = "potencial osmótico", ylab = "germinacion")

# añadimos la recta de regresión
abline(modelo, col = "red")


# 4.1.1. Comprobar si se cumplen los requisitos de la regresión -----------

# H0: los residuos son normales
shapiro.test(modelo$residuals)  # p > 0.05, aceptamos hipótesis nula, los residuos son normales









