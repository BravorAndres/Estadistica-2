#sigma = desviacion
icmedia <- function(muestra,Sigma,conf){
  n = 64
  mediaMuestral = 20000
  alfa <- 1-conf
  valorz <- qnorm(alfa/2,lower.tail = FALSE)
  extInf <- mediaMuestral-valorz*Sigma/sqrt(n)
  extSup <- mediaMuestral+valorz*Sigma/sqrt(n)
  icmedia <- C(extInf,extSup)
  return(icmedia)
}

icmedia(64,5000,0.95)
icmedia
