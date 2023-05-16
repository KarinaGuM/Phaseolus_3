## Analisis de experimentos en invernadero 2018 ##
## Karina Gutierrez Moreno 
# Laboratorio de ecologia de plantas, Cinvestav Unidad Irapuato
# Efecto de la inoculacion de semillas de frijol con diferentes cepas de Trichoderma, sobre
# la resistencia a Colletotrichum lindemuthianum

setwd("~/R/Invernaderotodos_2018")
dir()

datos <- read.table("220119_allsetskarina.txt", sep="\t", header=T)
summary(datos)

# Consideraciones:
# Set 1: Primavera 2018 (abril a junio); suelo natural
# Set 2: Otonio 2018 (octubre a diciembre); suelo natural
# Set 3: Otonio 2018 (septiembre a noviembre); sustrato esterilizado

# Spores: % de esporas germinadas por campo (por hoja) observado; n=10 (aleatorias)
# Area: % de area de la hoja daniada por severidad de la enfermedad; n=15 (aleatorias)
# Dweight: Peso seco del area foliar de cada planta por tratamiento; n=5

# Convirtiendo "Set" en factor
datos$Set <- as.factor(datos$Set)

# Realizar boxplots de las variables para observar tendencias
boxplot(Spores ~ Set, data=datos, main = "Effect of temperature and soil on % Spores' germination",
        xlab = "Data Set", ylab = "% Spores' germination",
        col=terrain.colors(4), frame=FALSE)
# A pesar de la dispersion de datos hacia arriba de la mediana, parece que el uso de
# suelo o sustrato, no tiene efecto sobre el % de germinacion del patogeno.

boxplot(Area ~ Set, data=datos, frame=FALSE)

boxplot(Area ~ Set, data=datos, main = "Effect of Effect of temperature and soil on disease severity",
        xlab = "Data Set", ylab = "% of Damaged leaf area",
        col=terrain.colors(4), frame=FALSE)
# Se observa una menor dispersion de datos en el Set 1 (suelo natural)
# El set 1 fue realizado en primaver, mientras que los sets 2 y 3 en otonio, cuando
# la temperatura comenzaba a descender, por lo que las condiciones climaticas parecen
# ser un factor determinante en la severidad de la enfermedad causada por el patogeno
# El set 2 muestra bastante dispersion de datos por encima de la mediana, pero sin dife-
# rencias significativas con respecto del Set en donde se uso sustrato esterilizado.

boxplot(Dweight ~ Set, data=datos)

boxplot(Dweight ~ Set, data=datos, main = "Effect of temperature and soil on plant growth",
        xlab = "Data Set", ylab = "Foliar dry weight (g)",
        col=terrain.colors(4), frame=FALSE)
# No existen diferencias significativas entre los tres sets en cuanto a peso seco del
# area foliar, lo cual indica que no hay diferencias significativas entre el uso de 
# suelo natural o sustrato esterilizado para esta variable. Pero existe una mayor distribu
# cion de los datos para el uso de sustrato esterilizado.

# Revisando si agregando a Trichoderma(Treatment) cambia el efecto
boxplot(Spores ~ Set + Treatment, data=datos)
boxplot(Spores ~ Set + Treatment, data=datos, main = "Effect of temperature, soil and Treatment on spores' germination",
        ylab = "% Spores' germination",
        col=terrain.colors(4), frame=FALSE)

# la grafica indica que no existen diferencias significativas entre los sets de experi
# mentos (sustrato + condiciones ambientales) y las cepas de Trichoderma utilizadas en
# el % de germinacion del patogeno.

boxplot(Area ~ Set + Treatment, data=datos)
boxplot(Area ~ Set + Treatment, data=datos, main = "Effect of temperature, soil and Treatment on disease severity",
        ylab = "% Damaged leaf area",
        col=terrain.colors(4), frame=FALSE)
# En este caso existen grandes diferencias entre las medianas de todos los sets
# teniendo las mayores medianas el set 1, indicando un mayor efecto por las condiciones
# ambientales en la severidad de la enfermedad.
# Aparentemente solo existen diferencias significativas entre el Set 1 y 2 cuando se u-
# tiliza la cepa P1 (T. atroviride P1).
# Por la desviacion de los datos, aparentemente no existen otras diferencias significa-
# tivas entre los sets de experimentos cuando se agrega la variable cepa de Trichoder-
# ma.

boxplot(Dweight ~ Set + Treatment, data=datos)
boxplot(Dweight ~ Set + Treatment, data=datos, main = "Effect of temperature, soil and treatment on plant growth",
        ylab = "Foliar dry weight (g)",
        col=terrain.colors(4), frame=FALSE)
# La grafica indica que existen medianas mayores, aunque con mayor dispersion de datos,
# en el Set 3 de experimentos, aunque sin diferencias significativas con los otros dos
# sets. Esto podria indicar que el uso de sustrato o las condiciones ambientales en este
# experimento, promovieron el crecimiento de las plantas.
# Por el contratio, el set 2 de suelo natural tiene menores medianas, aunque la tempera-
# tura habia descendido considerablemente al realizarse este set.

# Evaluando el efecto de agregar el genotipo de frijol a Set
boxplot(Spores ~ Set + Cultivar, data=datos)
boxplot(Spores ~ Set + Cultivar, data=datos, main = "Effect of temperature, soil and bean genotype on spores germination",
        ylab = "% Spores germination",
        col=terrain.colors(4), frame=FALSE)
# La grafica indica que no existen diferencias significativas entre los sets de experi-
# mentos agregando el genotipo de frijol como factor, en el % de germinacion de esporas
# del patogeno.

boxplot(Area ~ Set + Cultivar, data=datos)
boxplot(Area ~ Set + Cultivar, data=datos, main = "Effect of temperature, soil and bean genotype on disease severity",
        ylab = "% Damaged leaf area",
        col=terrain.colors(4), frame=FALSE)
# Aunque sin aparentes diferencias significativas, existen una tendencia evidente de ma-
# yores medianas en el Set 1. Ya que no existen muchas diferencias entre las medianas
# de los set 2 y 3 en ningun genotipo de frijol, podria inferirse que las condiciones
# ambientales determinan el nivel de severidad de la enfermedad causada por el patogeno.

boxplot(Dweight ~ Set + Cultivar, data=datos)
boxplot(Dweight ~ Set + Cultivar, data=datos, main = "Effect of temperature, soil and bean genotype on plant growth",
        ylab = "Foliar dry weight (g)",
        col=terrain.colors(4), frame=FALSE)
# En este caso al agregar el cultivar (genotipo) de frijol como factor al analisis,
# no existen diferencias significativas aunque existe una tendencia a mayores medianas}
# en el Set 3, sustrato esterilizado + condiciones ambientales.

####### 28 de enero de 2019 ####

## Revisando si se puede realizar un Boxplot con los tres factores
boxplot(Spores ~ Set + Treatment + Cultivar, data=datos)
boxplot(Spores ~ Set + Treatment + Cultivar, data=datos, main = "Factors interaction effects on % spores germination",
        ylab = "% Spores germination",
        col=terrain.colors(4), frame=FALSE)
## Sale un boxplot muy grande en donde básicamente no hay diferencias significativas

####################################################################################
#Regresando a lo anterior

# Realizando analisis de varianza ANOVA, a Spores como funcion de Set + Trichoderma
summary(aov(Spores ~ Set + Treatment, data=datos))
# Al realizar este ANOVA observamos que para una P < 0.001 existen diferencias signi-
# ficativas entre Tratamientos con las cepas de Trichoderma en el % de germinacion de
# esporas del patogeno, en todos los Sets.

summary(aov(Spores ~ Set * Treatment, data=datos))
# Al realizar el ANOVA con la interaccion Set * Trichoderma, se observa que si existen
# diferencias significativas en la interaccion entre el Set experimental y la cepa de 
# Trichoderma utilizada.

summary(aov(Area ~ Set * Treatment, data=datos))
# Existen diferencias significativas en cada factor evaluado y en la interaccion Set *
# Tratamiento (Trichoderma), en la variable % de area daniada de las hojas.
# Ambos factores, sustrato y condicion ambiental, y cepa de Trichoderma utilizada, son 
# determinantes en la severidad de la enfermedad.

summary(aov(Dweight ~ Set * Treatment, data=datos))
# En este caso el ANOVA indica que solo existen diferencias signficativas en el peso
# seco del area foliar para el factor Set (sustrato y condiciones ambientales), pero
# no para el Tratamiento con Trichodermas y su interaccion con Set.

# Realizando analisis de varianza ANOVA agregando el genotipo de frijol Cultivar como
# factor.
summary(aov(Spores ~ Set * Cultivar, data = datos))
# En este caso existen mayores significancias para el factor Cultivar y para su inte-
# raccion con Set, en cuanto al % de germinacion de esporas del patogeno.

summary(aov(Area ~ Set * Cultivar, data = datos))
# Los resultados de esta Anova indican que existen efectos significativos en el area
# de la hoja daniada, severidad de la enfermedad, por los factores Set, Genotipo de 
# frijol utilizado y la interaccion de ambos.

summary(aov(Dweight ~ Set * Cultivar, data = datos))
# Los resultados de esta Anova indican que existen efectos significativos en el peso
# seco del area foliar, promocion de crecimiento, por los factores Set, Genotipo de 
# frijol utilizado y la interaccion de ambos.

# Evaluando la interaccion de los factores Trichoderma (Treatment) y Cultivar de frijol
summary(aov(Spores ~ Treatment * Cultivar, data=datos))
# Los resultados indican que si existen efectos significativos por los factores
# Tratamiento (cepa de Trichoderma), el genotipo de frijol utilizado y la interaccion
# de ambos, en el % de germinacion de esporas del patogeno.

summary(aov(Area ~ Treatment * Cultivar, data=datos))
# Los resultados indican que si existen efectos significativos por los factores
# Tratamiento (cepa de Trichoderma), el genotipo de frijol utilizado y la interaccion
# de ambos, en la severidad de la enfermedad medida en % de area daniada.

summary(aov(Dweight ~ Treatment * Cultivar, data = datos))
# Los resultados de este ANOVA indican que solo existen efectos significativos en la 
# promocion de crecimiento vegetal por el cultivar (genotipo) de frijol utilizado.

# Hasta el momento podemos concluir que los tres factores, Set (sustrato y condiciones-
# ambientales), Tratamiento con Trichoderma y Genotipo de frijol, tienen efectos signi-
# ficativos en la resistencia temprana y tardia a Colleotrichum lindemuthianum.
# Solamente el factor Genotipo de frijol y su interaccion con las condiciones del Set 3
# tienen efectos significativos en la promocion de crecimiento de las plantas.


## INTERACCION DE LOS 3 FACTORES 28ENERO2019

# Revisemos si podemos evaluar el efecto de los tres factores sobre las variables de 
# respuesta
# % de germinacion de esporas de Colletotrichum
summary(aov(Spores ~ Set * Treatment * Cultivar, data=datos))

#Notese que para el caso de % de germinacion de esporas del patogeno, todas las interac-
#ciones entre los factores y la interaccion de los 3 factores entre si, son estadistica-
#mente significativas por el "peso" del efecto que tienen los tratamientos y el genoti-
#po de la planta por encima del sustrato y condiciones ambientales, p<0.001

# Severidad de la enfermedad %de area daniada (hojas)
summary(aov(Area ~ Set * Treatment * Cultivar, data=datos))
# Para el caso de severidad de la enfermedad medida en % de area de la hoja daniada por 
# sintomas de antracnosis, todos los factores son estadisticamente significativos por 
# si solos y por la interaccion de 2 de ellos o de los tres en conjunto (p<0.001).


# Promocion de crecimiento, peso seco area foliar
summary(aov(Dweight ~ Set * Treatment * Cultivar, data=datos))
# En el caso de la promocion de crecimiento vegetal, los factores Set(Sustrato y condi-
# ciones ambientales), Cultivar (genotipo de frijol), y la interaccion de ambos, son ma-
# yormente significativos que el tratamiento (cepa de Trichoderma utilizada), la interac-
# cion Tratamiento:Cultivar y la interaccion de los tres factores

# Si los tres factores tienen efectos sobre la resistencia a la enfermedad, conviene ana-
# lizar los sets por separado en cuanto a las variables de respuesta de este??