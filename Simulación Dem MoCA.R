folios <- 1:100

set.seed(1238)

mean.ed = c(43.5, 38.7, 25.9, 50.4)
mean.sex = c(1.5, 1.2, .9, 1.7)
mean.esc = c(8.5, 6.7, 7.332, 6.728)
mean.moc = c(25, 20, 27, 22)
mean.tiempo = c(75, 65, 87, 95) - runif(4)
mean.dicons = c(12, 14, 15, 23) - runif(4)

sd.ed = c(13.25521, 12.5548, 11.2654, 14.1564)
sd.sex = c(.5, .8, .9, 1.9)
sd.esc = c(3.879, 4.5, 2.89, 3.9854)
sd.moc = c(2, 3, 1, 2) - runif(4)
sd.tiempo = c(12, 13, 13, 15) - runif(4)
sd.dicons =  c(5, 4, 8, 5) - runif(4)

medias = rbind(mean.ed, mean.sex, mean.esc, mean.moc, mean.tiempo, mean.dicons)
desviaciones =rbind(sd.ed, sd.sex, sd.esc, sd.moc, sd.tiempo, sd.dicons)

edad.tot = as.integer(rnorm(n=2000, mean= mean.ed[3], sd=sd.ed[3]))
edad.tot = edad.tot[-(which(edad.tot > 65 | edad.tot < 18))]
sexo.tot = as.integer(rnorm(2000, mean = mean.sex[3], sd=sd.sex[3]))
sexo.tot = sexo.tot[-(c(which(sexo.tot == 0), which(sexo.tot >= 3)))]
escol.tot = as.integer(rnorm(2000, mean = mean.esc[3], sd=sd.esc[3]))
escol.tot = escol.tot[-(c(which(escol.tot <= 0)))]
estciv.tot = as.integer(runif(2000, min = 1, max = 5))
moca.tot = as.integer(rnorm(n=20000, mean=mean.moc[3], sd=sd.moc[3]))
if(length(moca.tot[-(which(moca.tot > 30))]) > 0){
moca.tot = moca.tot[-(which(moca.tot > 30))]}
tiempo.tot = as.integer(rnorm(n=2000, mean=mean.tiempo[3], sd=sd.tiempo[3]))
dias.cons.tot = as.integer(rnorm(n=2000, mean=mean.dicons[3], sd=sd.dicons[3]))

edad = sample(edad.tot, 100)
sexo = sample(sexo.tot, 100)
escol = sample(escol.tot, 100)
escol2 = escol
escol2[which(escol >= 1 & escol <= 6)] = 1
escol2[which(escol >= 7 & escol <= 9)] = 2
escol2[which(escol >= 10 & escol <= 12)] = 3
escol2[which(escol >= 13 & escol <= 16)] = 4
escol2[which(escol > 16)] = 5
est.civ = sample(estciv.tot, 100)
moca = sample(moca.tot, 100)
moca.diag = moca
moca.diag[which(moca >= 25 & moca <= 30)] = 1
moca.diag[which(moca >= 20 & moca <= 24)] = 2
moca.diag[which(moca <= 19)] = 3

dep.diag = folios
tiempo = folios
dias.cons = folios

moca[which(moca >= 25 & moca <= 30)]
dep01.tot = as.integer(rnorm((length(moca[which(moca >= 25 & moca <= 30)])* 10), mean= .8951, sd = .3))
dep01 = sample(dep01.tot, length(moca[which(moca >= 25 & moca <= 30)]))

moca[which(moca >= 19 & moca <= 24)]
dep12.tot = as.integer(rnorm((length(moca[which(moca >= 19 & moca <= 24)])* 10), mean= 1.248, sd = .3))
dep12 = sample(dep12.tot, length(moca[which(moca >= 19 & moca <= 24)]))

moca[which(moca <= 18)]
dep13.tot = as.integer(rnorm((length(moca[which(moca <= 18)])* 10), mean= 2.2756, sd = .76))
dep13.tot = dep13.tot[-(which(dep13.tot == 4 | dep13.tot == 2))]
dep13 = sample(dep13.tot, length(moca[which(moca <= 18)]))

dep.diag[which(moca >= 25 & moca <= 30)] = dep01
dep.diag[which(moca >= 19 & moca <= 24)] = dep12
dep.diag[which(moca <= 18)] = dep13

tiempo.tot[which(tiempo.tot >= 120 & tiempo.tot <= 35)]

tiempo.01.tot = tiempo.tot[which(tiempo.tot <= 60)]
tiempo.12.tot = tiempo.tot[which(tiempo.tot >= 61 & tiempo.tot <= 85)]
tiempo.13.tot = tiempo.tot[which(tiempo.tot >= 86 & tiempo.tot <= 120)]

tiempo.01 = sample(tiempo.01.tot, length(moca[which(moca >= 25 & moca <= 30)]))
tiempo.12 = sample(tiempo.12.tot, length(moca[which(moca >= 19 & moca <= 24)]))
tiempo.13 = sample(tiempo.13.tot, length(moca[which(moca <= 18)]))

tiempo[which(moca >= 25 & moca <= 30)] = tiempo.01
tiempo[which(moca >= 19 & moca <= 24)] = tiempo.12
tiempo[which(moca <= 18)] = tiempo.13

dias.cons.01 = dias.cons.tot[which(dias.cons.tot <= 10)]
dias.cons.12 = dias.cons.tot[which(dias.cons.tot >= 11 & dias.cons.tot <= 20)]
dias.cons.13 = dias.cons.tot[which(dias.cons.tot >= 21 & dias.cons.tot <= 30)]

dias.cons.01 = sample(dias.cons.01, length(moca[which(moca >= 25 & moca <= 30)]))
dias.cons.12 = sample(dias.cons.12, length(moca[which(moca >= 19 & moca <= 24)]))
dias.cons.13 = sample(dias.cons.13, length(moca[which(moca <= 18)]))

dias.cons[which(moca >= 25 & moca <= 30)] = dias.cons.01
dias.cons[which(moca >= 19 & moca <= 24)] = dias.cons.12
dias.cons[which(moca <= 18)] = dias.cons.13

mean(moca)

datos = cbind(folios, edad, sexo, escol, escol2, est.civ, moca, moca.diag, dep.diag, tiempo, dias.cons)

setwd("C:/Users/INPRFM/Desktop/Archivos R")
write.csv(datos, file = "base_alds4.csv")
write.csv(medias, file = "medias.csv")
write.csv(desviaciones, file = "desviaciones.csv")

length(dep.diag[which(moca >= 25 & moca <= 30)])
length(dep01)






