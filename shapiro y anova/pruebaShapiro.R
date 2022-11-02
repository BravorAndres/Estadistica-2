verde <- c(37,32,15,25,39,42)
blanco <- c(21,12,14,17,13,17)
amarillo <- c(45,59,48,46,38,47)
azul <- c(16,11,20,21,14,7)

shapiro.test(azul)
hist(azul)
shapiro.test(blanco)
hist(blanco)
shapiro.test(amarillo)
hist(amarillo)
shapiro.test(verde)
hist(verde)

var.test(azul,Verde)
var.test(verde,blanco)
var.test(azul,amarillo)

insectos <- c(azul,blanco,amarillo,verde)
colores <- as.factor(c(rep(c("azul","blanco","amarillo","verde"),each=6)))
#explorar la base de datos

### bloxplot estratificado por colores

boxplot(insectos - colores.col=c("blue","green","white","yellow"), ylab="numero de insectos atrapados") 

apply(insectos,colores,)
apply(insectos,colores,sd)

anova <- aov(insectos -colores)
anova

sumary(anova)

#¿cual es el valor critico de f bajo la hipótesis nula con un nivel de significancia alfa =0.05?
#este valor nos definirá la región de aceptación y rechazo?
#bajo la H0 el estadistico de contraste F se distribuye como una F de grados de libertad(k-1)(n-k)
#donde k es el numero de grado que dispongamos
#el tamaño total de la muestra

#así obtendremos el cuartil buscado en R

#valores del estadistico estaránincluidos en la región del rechazo de nuestro caso 30,55 es mucho mayor que el valor critico

#además de proporcionarnos una estimación de la varianza muestral de todos los datos se utiliza en la obtención de la media  en cada uno de los grupos de interes.

#si hemos detectado diferencias significativas entre las medias de las poblaciones
#Es posible saber cuales son los grupos que generan estás diferencias

intervalos <- Tukey(anova)
plot(intervalos)
