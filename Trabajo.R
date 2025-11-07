# Miranda Galán Castelló_Trabajo2.R
# Trabajo final Bioinformática - Curso 25/26
# Análisis de parámetros biomédicos por tratamiento

install.packages("ggplot2")
library("ggplot2")
install.packages("RColorBrewer")
library("RColorBrewer")

# 1. Cargar librerías (si necesarias) y datos del archivo "datos_biomed.csv". (0.5 pts)

install.packages("readr")
library("readr")
data = read_csv("datos_biomed.csv")

# 2. Exploración inicial con las funciones head(), summary(), dim() y str(). ¿Cuántas variables hay? ¿Cuántos tratamientos? (0.5 pts)

head(data)
summary(data)
dim(data)
str(data)
# Hay 5 variables: ID, tratamiento, glucosa, presión y colesterol.
# Hay 3 tratamientos: Fármaco A, Fármaco B y Placebo

# 3. Una gráfica que incluya todos los boxplots por tratamiento. (1 pt)

par(mfrow = c(3, 1)) # Esto hace que todos los boxplots se unan.

# Boxplot de la Glucosa por tratamiento

boxplot(Glucosa ~ Tratamiento, data = data, 
main = "Boxplot de Glucosa y Tratamiento",
xlab = "Tratamiento", ylab = "Glucosa",
col = brewer.pal(length(unique(data$Tratamiento)), "Blues"))

# Boxplot de la Presión por tratamiento

boxplot(Presion ~ Tratamiento, data = data, 
main = "Boxplot de Presión y Tratamiento",
xlab = "Tratamiento", ylab = "Presión",
col = brewer.pal(length(unique(data$Tratamiento)), "Reds"))

# Boxplot del Colesterol por tratamiento

boxplot(Colesterol ~ Tratamiento, data = data, 
main = "Boxplot de Colesterol y Tratamiento",
xlab = "Tratamiento", ylab = "Colesterol",
col = brewer.pal(length(unique(data$Tratamiento)), "Greens"))

# 4. Realiza un violin plot (investiga qué es). (1 pt)

ggplot(data, aes(x = Tratamiento, y = Presion, fill = Tratamiento)) +
geom_violin(trim = FALSE) +
geom_boxplot(width = 0.1, fill = "white") +
scale_fill_brewer(palette = "Reds") +
labs(title = "Violin Plot de Presión por Tratamiento") 
 
# 5. Realiza un gráfico de dispersión "Glucosa vs Presión". Emplea legend() para incluir una leyenda en la parte inferior derecha. (1 pt)

grupo = as.factor(data$Tratamiento)
colores = c("lightseagreen", "salmon", "thistle")

plot(data$Glucosa ~ data$Presion, xlab = "Glucosa", ylab = "Presión",
main = "Glucosa vs Presión - Scatter", pch = 19, 
col = colores[grupo])

# Leyenda
legend("bottomright", legend = levels(grupo), col = colores, pch = 19)

# 6. Realiza un facet Grid (investiga qué es): Colesterol vs Presión por tratamiento. (1 pt)

# Facet Grid sirve para dividir un gráfico en subgráficos según variables categóricas.
# Nuestro Facet Grid estará dividido en subgrupos según el tratamiento.

data$Tratamiento = as.factor(data$Tratamiento)
ggplot(data, aes(x = Colesterol, y = Presion, color = Tratamiento)) +
geom_point(size = 2) +
facet_grid(. ~ Tratamiento) +
scale_color_brewer(palette = "Set2") +
labs(title = "Colesterol vs Presión por tratamiento",
x = "Colesterol", y = "Presión") +
theme_minimal()

# 7. Realiza un histogramas para cada variable. (0.5 pts)

hist(data$Glucosa, col = "lightblue", main="Glucosa - Histogram", xlab = "Glucosa")
hist(data$Presion, col = "red", main="Presión - Histogram", xlab = "Presión")
hist(data$Colesterol, col = "lightgreen", main="Colesterol - Histogram", xlab = "Colesterol")

# 8. Crea un factor a partir del tratamiento. Investiga factor(). (1 pt)

data$Tratamiento = factor(data$Tratamiento)
is.factor(data$Tratamiento) # Comprobación de que el factor se ha creado.

# 9. Obtén la media y desviación estándar de los niveles de glucosa por tratamiento. Emplea aggregate() o apply(). (0.5 pts)

# Media:
aggregate(Glucosa ~ Tratamiento, data = data, mean)

# Desviación estándar:
aggregate(Glucosa ~ Tratamiento, data = data, sd)

# 10. Extrae los datos para cada tratamiento y almacenalos en una variable. Ejemplo todos los datos de Placebo en una variable llamada placebo. (1 pt)

Placebo = data[data$Tratamiento == "Placebo",]
FarmacoA = data[data$Tratamiento == "FarmacoA",]
FarmacoB = data[data$Tratamiento == "FarmacoB",]
head(Placebo)
head(FarmacoA)
head(FarmacoB)

# 11. Evalúa si los datos siguen una distribución normal y realiza una comparativa de medias acorde. (1 pt)

shapiro.test(data$Glucosa)
shapiro.test(data$Presion)
shapiro.test(data$Colesterol)

# Haciendo el Shapiro.test podemos ver el p-value, si este es >0.05 consideraremos
# que sigue una distribución normal, si fuese <0.05 se considera no normal.
# En el caso de los tres parámetros que hemos analizado, siguen una distribución normal
# porque el p-value en los 3 es mayor a 0.05.

# Comparación de medias con t-test

# Glucosa

# Vamos a quitar las columnas necesarias de la variable que hemos guardado en
# el ejercicio 10 para obtener los datos que queremos, en el caso de Glucosa
# quitamos la columna de Presión y Colesterol, etc.

Placebo_G = Placebo[, -c(4,5)]
head(Placebo_G)
FarmacoA_G = FarmacoA[, -c(4,5)]
head(FarmacoA_G)
FarmacoB_G = FarmacoB[, -c(4,5)]
head(FarmacoB_G)

# Con el t.test estaremos comparandado las medias entre los tratamientos.
# En este caso comparamos los resultados obtenidos de cada tratamiento respecto la glucosa.
t.test(Placebo_G[,3],FarmacoA_G[,3]) # El fármacoA presenta una media mayor que el placebo.
t.test(Placebo_G[,3],FarmacoB_G[,3]) # El fármacoB y el placebo presentan una media similar.
t.test(FarmacoA_G[,3],FarmacoB_G[,3]) # El fármacoA también presenta una media mayor que el fármacoB.

# Realizamos lo mismo sucesivamente con la Presión y el Colesterol.

# Presión

Placebo_P = Placebo[, -c(3,5)]
head(Placebo_P)
FarmacoA_P = FarmacoA[, -c(3,5)]
head(FarmacoA_P)
FarmacoB_P = FarmacoB[, -c(3,5)]
head(FarmacoB_P)

t.test(Placebo_P[,3],FarmacoA_P[,3]) # El placebo y el fármacoA presentan una media bastante diferente.
t.test(Placebo_P[,3],FarmacoB_P[,3]) # El placebo presenta una media bastante inferior que el fármacoB.
t.test(FarmacoA_P[,3],FarmacoB_P[,3]) # El fármacoA y el fármacoB presentan una media parecida.

# Colesterol

Placebo_C = Placebo[, -c(3,4)]
head(Placebo_C)
FarmacoA_C = FarmacoA[, -c(3,4)]
head(FarmacoA_C)
FarmacoB_C = FarmacoB[, -c(3,4)]
head(FarmacoB_C)

t.test(Placebo_C[,3],FarmacoA_C[,3]) # La media del placebo es muy inferior a la media del fármacoA.
t.test(Placebo_C[,3],FarmacoB_C[,3]) # Placebo y fármacoB presentan una media más parecida.
t.test(FarmacoA_C[,3],FarmacoB_C[,3]) # La media del fármacoA es superior a la del fármacoB.

# 12. Realiza un ANOVA sobre la glucosa para cada tratamiento. (1 pt)

anova_G = aov(Glucosa ~ Tratamiento, data = data)
summary(anova_G)


