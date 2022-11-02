#intervalo de cpnfianza para la media poblacional con varianza conocida
conf <-0.95
Sigma=5000
n=64
media = 20000

#valor de Z 
alfa<- 1-conf
valorz <- qnorm(alfa/2,lower.tail=FALSE)
valorz

#extremos del intervalo de confianza
extInf<- media-valorz*Sigma/sqrt(n)
extSup<- media+valorz*Sigma/sqrt(n)

ic<-c(extInf,extSup)

ic

