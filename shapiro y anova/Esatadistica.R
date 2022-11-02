


setwd("C:\\Users\\unalman\\Downloads\\" )

datos <- read.table("bancoAnalisisNormalidad_edad_presion.csv",head=T,sep=",")

dim(datos)
head(datos)
colnames(datos)
str(datos)
attach(datos)

banco1 <- datos[Rango_edad==1,]
dim(banco1)

banco2 <- datos[Rango_edad==3,]
dim(banco2)

banco3 <- datos[Rango_edad==5,]

dim(banco3)

datos1 <- rbind(banco1,banco2,banco3)

getwd()

write.table(datos1,"bancoRango123.csv",col.names=TRUE,row.names=FALSE.sep=",",quote=)


dim(datos)

attach(datos1)


#####################################  colocando como factor las variables nominales

Rango_edad <- as.factor(datos1$Rango_edad)
genero <- as.factor(datos1$genero)

### media apps

head(datos1)
attach(datos1)

dim(datos1)

summary(dato1$mediaPAS)


#estadistica descriptiva de la variable mediaAPS

hist(mediaPAS)
boxplot(mediaPAS~genero)
boxplot(mediaPAS~Rango_edad)

tapply(mediaPAS, genero, summary)

tapply(mediaPAS, Rango_edad, summary)



#prueba de hipotesis -comparar medias  de 2 poblaciones
#h_0:   media de la variable media_PAS_ho es igual de mediaPAS_mujeres
#h_1:  media de la variable media_PAS_ho es diferente de mediaPAS_mujeres


hombres <- datos1[genero==1,]
head(hombres)
dim(hombres)
mediaPAS_h <- hombres[,12]
mediaPAS_h


mujeres <- datos1[genero==2,]
head(mujeres)
dim(mujeres)
mediaPAS_m <- mujeres[,12]
mediaPAS_m

hist(mediaPAS_h)
hist(mediaPAS_m)


##contraste con la teoria de ANOVA para cada factor

p.aov <- aov(mediaPAS~Rango_edad)
summary(p.aov)


TukeyHSD(p.aov,ordered = TRUE)

plot(TukeyHSD(p.aov,ordered = TRUE))


#Anova de dos factore

#la hipotesis de iguladad de ls efectos de los I del factor a (filas) pueden plantearse mediantre la hipotesis nula:
#h_1: 
#h_0: alfa1 = alfa2 ... alfaI

#la hipotesis de igualda de los  niveles (columnas) se plantea como 

#por tanto la hipotesis nula del facxtor a con el factor B es : S_alfa1* beta_1 = ... alfa_I * betaJ 

#la ausencia de iteraciones implica que la diferencia de medias de dos niveles deujn factor es la misma para tods os noveles del 
#otro factor


############## ANOVA DE DOS FACTOERES 

#************** modelos sin iteraccion

p.aov <- aov(mediaPAS~Rango_edad+genero)
summary(p.aov)



#*********** modelos con itaraccion
p.aov <- aov(mediaPAS~genero+Rango_edad*genero)
summary(p.aov)


p.aov <- aov(mediaPAS~Rango_edad*genero)
summary(p.aov)



TukeyHSD(p.aov,ordered = TRUE)
plot(TukeyHSD(p.aov,ordered = TRUE))




#obteniendo los graficos de contraste

attach(datos)
interaction.plot(genero,Rango_edad,mediaPAS,legend = T,col=c("red","blue","green"))

interaction.plot(Rango_edad,genero,mediaPAS,legend = T,col=c("red","green"))



