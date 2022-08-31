#intervalo de confianza para una proporcion

nExito <- 50
nTotalEnsayos <- 150
prop.test(nExito,nTotalEnsayos,conf.level = 0.80)
