# Nombre: SilviaGutiérrezMenéndez_Trabajo2.R

#PREGUNTA 1
#Cargamos el archivo de datos tabulado en la variable datos utilizando la función read.table() y 
#luego mostramos las primeras filas (head), el resumen estadístico (summary), 
#las dimensiones (dim), la estructura (str) y calcula el número de variables y tratamientos únicos
#presentes en los datos.

# Cargamos los datos desde el archivo 
datos <- read.table("/Users/silviagutierrez/Desktop/R2/datos-trabajoR.txt", header = TRUE, sep = "\t")

# Examinamos los datos con las funciones las funciones head(), summary(), dim() y str()
cat("Visualización inicial de los datos:\n")
head(datos)
summary(datos)
dim(datos)
str(datos)
cat("Número de variables:", ncol(datos), "\n")
cat("Número de tratamientos:", length(unique(datos$Tratamiento)), "\n")


#PREGUNTA 2
#Realizamo tres diagramas de caja utilizando la función boxplot, donde cada variable (Wildtype, Sequia, ExcesoRiego) 
#se compara con la variable categórica Tratamiento
#Para ello, se aplicaa la ecuación  Variable ~ Tratamiento, lo que indica que se grafican los valores de 
#cada variable agrupados según los niveles de tratamiento.
#Mediante la función (col) asignamos un color distinto a cada uno, y le asignamos tambien un título con (main)

# Creamos un boxplot para cada condición 
boxplot(Wildtype ~ Tratamiento, data = datos, col = "lightpink", main = "Wildtype")
boxplot(Sequia ~ Tratamiento, data = datos, col = "red", main = "Sequia")
boxplot(ExcesoRiego ~ Tratamiento, data = datos, col = "green", main = "ExcesoRiego")


#PREGUNTA 3
##Generamos dos gráficos de dispersión con la función (plot). El primer gráfico comparará las variables Sequia y Wildtype, representando los valores
#de Wildtype en el eje ''x'' y los de Sequia en el eje ''y'', y asignaremos un color diferente paralos puntos (col = datos$Tratamiento) de los diferentes 
#tratamientos.Mismamente, con el segundo gráfico comparando las variable Wildtype con ExcesoRiego, que ocuparán respectivamente los ejes ''x'' e ''y''.

# Realizamos dos gráficos de dispersión, el primero compara sequía vs wildtype, el segundo excesoriego vs wildtype
plot(datos$Wildtype, datos$Sequia, col = datos$Tratamiento, main = "Sequía vs Wildtype", xlab = "Wildtype", ylab = "Sequía")
plot(datos$Wildtype, datos$ExcesoRiego, col = datos$Tratamiento, main = "ExcesoRiego vs Wildtype", xlab = "Wildtype", ylab = "ExcesoRiego")


#PREGUNTA 4
#Utilizamos la función legend para agregar una leyenda al gráfico, indicando ("bottomright") para ubicarla en la esquina inferior derecha.  
#Para extraer los valores únicos de la columna de tratamiento definimos con unique(datos$Tratamiento).
#Asignamos un color único a cada tratamiento que corresponden a los que representan en el gráfico de dispersión, por ello utilizamos
#col = 1:length(unique(datos$Tratamiento))

# Agregamos la leyenda
legend("bottomright", legend = unique(datos$Tratamiento), col = 1:length(unique(datos$Tratamiento)), pch = 1)


#PREGUNTA 5
#Utilizamos la función hist para crear un histograma de cada variable (Wildype, Sequia y ExcesoRiego)
#Utilizando el argumento (col), asignamos colores distintos a cada histograma de cada variable

# Hacemos un histograma para cada variable
hist(datos$Wildtype, col = "lightpink", main = "Wildtype", xlab = "Wildtype")
hist(datos$Sequia, col = "red", main = "Sequía", xlab = "Sequía")
hist(datos$ExcesoRiego, col = "green", main = "ExcesoRiego", xlab = "ExcesoRiego")

#PREGUNTA 6
#Para convertir la columna de Tratamiento de los datos en un factor, utilizamos la función factor.
#Para asegurarnos de que los resultados sean intepretados como categorías y no como valores numéricos,
#asignamos el resultado a la variable factor_tratamiento. 

# Hacemos un factor para la columna tratamiento
factor_tratamiento <- factor(datos$Tratamiento)


#PREGUNTA 7
#Utilizamos la función aggregate para calcular la media y la desviación estándar de las variables agrupadas por el tratamiento.
#La fórmula (. ~ Tratamiento) indica que se va a calcular para todas las columnas (representadas por .) agrupadas según la columna Tratamiento
#Meduiante la funcion (function(x) c(Media = mean(x), SD = sd(x)) ), calculamos para cada grupo la media y la desviación estándar.

# Calculamos la media y la desviación estándar por cada tratamiento
media_sd <- aggregate(. ~ Tratamiento, data = datos, function(x) c(Media = mean(x), SD = sd(x)))
print(media_sd)


#PREGUNTA 8
# Utilizamos la función (table) para contar las observaciones agrupadas por el factor (factor_tratamiento) 
#y guardamos el resultado en la variable (elementos_tratamiento)

# Averiguamos el número de elementos por tratamiento
elementos_tratamiento <- table(factor_tratamiento)
print(elementos_tratamiento)


#PREGUNTA 9
# Extraemos datos para Tratamiento 1 y Tratamiento 4
tratamiento_1 <- subset(datos, Tratamiento == 1)
tratamiento_4 <- subset(datos, Tratamiento == 4)

#PREGUNTA 10
# Comprobamos si los datos se distribuyen de forma normal usando shapiro.test()
# Si el p-valor del test de Shapiro-Wilk es menor a 0.05, los datos no siguen
# una distribución normal. En ese caso, usaríamos pruebas no paramétricas como
# Wilcoxon rank-sum test en lugar de t.test()

#Tratamiento 1: Comprobamos normalidad
shapiro.test(tratamiento_1$Wildtype)
shapiro.test(tratamiento_1$Sequia)
shapiro.test(tratamiento_1$ExcesoRiego)
#Todos los datos se distirbuyen de forma normal en el Tratamiento 1, ya que los valores del p value son mayores a 0.05 en cada caso.
#Esto nos permite usar pruebas paramétricas, como T-Test.

#Tratamiento 5: Comprobamos normalidad
shapiro.test(tratamiento_5$Wildtype)
shapiro.test(tratamiento_5$Sequia)
shapiro.test(tratamiento_5$ExcesoRiego)
#En el tratamietno 5, los datos no se distribuyen con normalidad en Sequía y en Wildtype ya que los resultados del p-value son 0.003 y 0.006
#respectivamente, y por tanto, menores a 0.05. En exceso de riego si se distirbuye con normalidad, pero elegiremos una prueba que se ajuste a las tres
#variables, como el Wilcoxon rank-sum test, que es una prueba no paramétrica.

#Por otro lado, queremos comprobar si hay igualdad entre varianzas o no, en cada tratamiento, ya que, la igualdad o desigualdad de varianzas influye en la
#selección del test adecuado para las comparaciones
#Comprobamos la igualdad de varianzas usando var.test()
#Si el p-valor de var.test() es menor a 0.05, las varianzas no son iguales.

#Tratamiento 1: Comprobamos varianzas
var_test1 <- var.test(tratamiento_1$Wildtype, tratamiento_1$Sequia)
var_test2 <- var.test(tratamiento_1$Wildtype, tratamiento_1$ExcesoRiego)
var_test3 <- var.test(tratamiento_1$Sequia, tratamiento_1$ExcesoRiego)

print(var.test(tratamiento_1$Wildtype, tratamiento_1$Sequia))
print(var.test(tratamiento_1$Wildtype, tratamiento_1$ExcesoRiego))
print(var.test(tratamiento_1$Sequia, tratamiento_1$ExcesoRiego))

#Las varianzas son significativamente distintas, ya que los valores p son menores a 0.05
#Dado que las varianzas no son iguales, al realizar un t-test para comparar las medias de estos grupos,
#utilizaremos la opción var.equal = FALSE en el T-Test, para ajustar el cálculo de t para manejar varianzas desiguales.

#Tratamiento 5: Comprobamos varianzas
var_test4 <- var.test(tratamiento_5$Wildtype, tratamiento_5$Sequia)
var_test5 <- var.test(tratamiento_5$Wildtype, tratamiento_5$ExcesoRiego)
var_test6 <- var.test(tratamiento_5$Sequia, tratamiento_5$ExcesoRiego)

print(var.test(tratamiento_5$Wildtype, tratamiento_5$Sequia))
print(var.test(tratamiento_5$Wildtype, tratamiento_5$ExcesoRiego))
print(var.test(tratamiento_5$Sequia, tratamiento_5$ExcesoRiego))

#Las varianzas son significativamente distintas, ya que los valores p son menores a 0.05
#El Wilcoxon rank-sum test (prueba no paramétrica) no depende de la igualdad de varianzas,
#por tanto, nohay que indicar nada diferente cuando lo realizamos.


#Pasamos a realizar el T-Test en Tratamiento 1
t.test(tratamiento_1$Wildtype, tratamiento_1$Sequia, var.equal = FALSE)
t.test(tratamiento_1$Wildtype, tratamiento_1$ExcesoRiego, var.equal = FALSE)

# Comparamos Sequía con ExcesoRiego
t.test(tratamiento_1$Sequia, tratamiento_1$ExcesoRiego, var.equal = FALSE)

#Interpretación de resultados
#En todas las comparaciones, hay diferencias significativas entre las medias de las variables
#los efectos de las condiciones son diferentes en este contexto experimental
#En primer lugar, el efecto en la condición Wildtype es significativamente más alto que en Sequía, 
#siendo sus valores 4.00 y 0.51 respectivamente.
#En segundo lugar, el efecto en la condición ExcesoRiego es significativamente más alto que en Wildtype,
#siendo sus valores 5.97 y 4.00 respectivamente. 
#Por último, el efecto en la condición ExcesoRiego es significativamente más alto que en Sequía, siendo sus 
#valores 5.97 y 0.51 respectivamente.


#Realizamos el Wilcoxon rank-sum test para el tratamietno 5
tratamiento_5 <- subset(datos, Tratamiento == 5)

wilcox_test1 <- wilcox.test(tratamiento_5$Wildtype, tratamiento_5$Sequia, 
                            paired = FALSE, exact = FALSE)
print("Wilcoxon test: Wildtype vs Sequía")
print(wilcox_test1)

wilcox_test2 <- wilcox.test(tratamiento_5$Wildtype, tratamiento_5$ExcesoRiego, 
                            paired = FALSE, exact = FALSE)
print("Wilcoxon test: Wildtype vs ExcesoRiego")
print(wilcox_test2)

# Comparación Sequía vs ExcesoRiego
wilcox_test3 <- wilcox.test(tratamiento_5$Sequia, tratamiento_5$ExcesoRiego, 
                            paired = FALSE, exact = FALSE)
print("Wilcoxon test: Sequía vs ExcesoRiego")
print(wilcox_test3)

#Interpretación de resultados
#En primer lugar,se observan diferencias significativas entre las medianas de Wildtype
#y Sequía (p = 0.0001575), esto indica que indica que la condición Wildtype tiene un 
#efecto considerablemente mayor que Sequía.
#En segundo lugar, tambien se observan diferencias significativas entre las medianas 
#de Wildtype y ExcesoRiego (p = 0.0001621), lo que sugiere que ExcesoRiego tiene un 
#efecto significativamente mayor que Wildtype.
#Por último, las diferencias entre las medianas de Sequía y ExcesoRiego son altamente
#significativas (p = 0.0001776), por lo que efecto de ExcesoRiego es bastante mayor que el de Sequía.

#PREGUNTA 11
# Extraemos los datos para el Tratamiento 1
tratamiento_1 <- subset(datos, Tratamiento == 1)

# Creamos variables separadas para Wildtype, Sequía y ExcesoRiego
wildtype_1 <- tratamiento_1$Wildtype
sequia_1 <- tratamiento_1$Sequia
excesoriego_1 <- tratamiento_1$ExcesoRiego

# Creamos una tabla con los datos reorganizados en un formato adecuado para ANOVA
datos_anova <- data.frame(
  Condicion = rep(c("Wildtype", "Sequia", "ExcesoRiego"), each = length(wildtype_1)),
  Valor = c(wildtype_1, sequia_1, excesoriego_1)
)

# Guardamos los datos en un archivo para revisión (opcional)
write.table(datos_anova, "datos-anova.txt", sep = "\t", row.names = FALSE, quote = FALSE)

# Realizamos el ANOVA
anova_result_1 <- aov(Valor ~ Condicion, data = datos_anova)
summary(anova_result_1)

#Interpretación de resultados
#Hay diferencias estadísticamente significativas entre las condiciones Wildtype, Sequía y ExcesoRiego.
#La suma de cuadrados asociada con las diferencias entre las condiciones es 152.91, lo que indica que 
#las condiciones explican una gran parte de la variabilidad total
#El valor de F es = 118.7, esto indica una gran variabilidad explicada por las diferencias entre
#las condiciones en comparación con la variabilidad residual
#El p-valor es 4.19e-14 (prácticamente 0), lo que significa que hay diferencias significativas entre 
#las medias de al menos dos condiciones, y se destacan los valores de 0,001, 0,01 y 0 ***)


