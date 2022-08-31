#IC para media con varianza desconocida

npae <- c(70,78,71,62,78,67,74,75,64,76,73,65,58,72,67)

n <- length(npae)
mediaMuestral <- mean(npae)
mediaMuestral
desv <- sd(npae)
desv
conf <- 0.95
alfa <- 1-conf
valort <- qt(alfa/2,lower.tail = FALSE,df=n-1)
extInf <- mediaMuestral-valort*desv/sqrt(n)
extSup <- mediaMuestral+valort*desv/sqrt(n)
ic = c(extInf,extSup)
ic


##########################################################

t.test(npae,conf.level = 0.95)

t.test(npae,conf.level = 0.95)$conf.int


