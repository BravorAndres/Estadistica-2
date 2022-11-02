#Ic para varianza poblacional una poblacion(chi ^2)

varianzaMuestral <- (2.2)^2
varianzaMuestral
n <- 16
conf = 0.99
alfa = 1-conf
chi.izq <- qchisq(1-alfa/2,lower.tail = FALSE,df=n-1)
chi.der <- qchisq(alfa/2,lower.tail = FALSE,df=n-1)
extInf <- (n-1)*varianzaMuestral/chi.der
extSup <- (n-1)*varianzaMuestral/chi.izq
icvar <- c(extInf,extSup)
icvar


##############################################

