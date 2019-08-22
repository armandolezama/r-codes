library(foreign)
library(nlme)
#to.data.frame = TRUE, trim.factor.names = TRUE, reencode = "UTF-8")
datos  = read.spss("C:\\Users\\INPRFM\\AppData\\Roaming\\Skype\\My Skype Received Files\\csxr.residential.sav",
to.data.frame = TRUE, reencode = "UTF-8")

filas = sort(c(
which(datos$dem01=="Hombre" & as.numeric(datos$phq1)>=1),
which(datos$dem01=="Mujer" & as.numeric(datos$phq1)>=1)))

datos = datos[filas,]
attach(datos)


#Puntajes y variables

sx1 = sx1a + sx1b + sx1c + sx1d
sx3 = sx3a + sx3b + sx3c + sx3d
sx4 = sx4a + sx4b + sx4c + sx4d

gad.1.7 = (as.numeric(gad1)-1) + (as.numeric(gad2)-1) + (as.numeric(gad3)-1) + 
(as.numeric(gad4)-1) + (as.numeric(gad5)-1) + (as.numeric(gad6)-1) + (as.numeric(gad7)-1)

phq.1.9 = (as.numeric(phq1)-1) + (as.numeric(phq2)-1) + (as.numeric(phq3)-1) + 
(as.numeric(phq4)-1) + (as.numeric(phq5)-1) + (as.numeric(phq6)-1) + (as.numeric(phq7)-1) + 
(as.numeric(phq8)-1) + (as.numeric(phq9)-1)

ac.us.1.13 = (as.numeric(ac.us1)-1) + (as.numeric(ac.us2)-1) + (as.numeric(ac.us3)-1) + 
(as.numeric(ac.us4)-1) + (as.numeric(ac.us5)-1) + (as.numeric(ac.us6)-1) + (as.numeric(ac.us7)-1) + (as.numeric(ac.us8)-1) + 
(as.numeric(ac.us9)-1) + (as.numeric(ac.us10)-1) + (as.numeric(ac.us11)-1) + (as.numeric(ac.us12)-1) + (as.numeric(ac.us13)-1)

no.us.1.7 = (as.numeric(no.us1)-1) + (as.numeric(no.us2)-1) + (as.numeric(no.us3)-1) + (as.numeric(no.us4)-1) + 
(as.numeric(no.us5)-1) + (as.numeric(no.us6)-1) + (as.numeric(no.us7)-1)

ae.us.1.28 = as.numeric(ae.us1) + as.numeric(ae.us2) + as.numeric(ae.us3) + 
as.numeric(ae.us4) + as.numeric(ae.us5) + as.numeric(ae.us6) + as.numeric(ae.us7) + as.numeric(ae.us8) +
as.numeric(ae.us9) + as.numeric(ae.us10) + as.numeric(ae.us11) + as.numeric(ae.us12) + as.numeric(ae.us13) + 
as.numeric(ae.us14) + as.numeric(ae.us15) + as.numeric(ae.us16) + as.numeric(ae.us17) + as.numeric(ae.us18) + 
as.numeric(ae.us19) + as.numeric(ae.us20) + as.numeric(ae.us21) + as.numeric(ae.us2)

#Modelos

	#Uso de condon

summary(lm(sx3~dem01, data=datos))
summary(lm(sx3~age, data=datos))
summary(lm(sx3~dem03, data=datos))
summary(lm(sx3~dem04, data=datos))
summary(lm(sx3~dem08, data=datos))

summary(lm(sx3~su1a, data=datos))
summary(lm(sx3~su1b, data=datos))
summary(lm(sx3~su1c , data=datos))
summary(lm(sx3~su1d , data=datos))
summary(lm(sx3~su1e , data=datos))
summary(lm(sx3~su2 , data=datos))
summary(lm(sx3~su3 , data=datos))
summary(lm(sx3~su3a , data=datos))
summary(lm(sx3~su4 , data=datos))
summary(lm(sx3~gad.1.7, data=datos))
summary(lm(sx3~phq.1.9, data=datos))

summary(lm(sx3~as.numeric(int.p), data=datos))
summary(lm(sx3~as.numeric(int.np), data=datos))
summary(lm(sx3~ac.us.1.13, data=datos))
summary(lm(sx3~no.us.1.7, data=datos))
summary(lm(sx3~ae.us.1.28, data=datos))

	#Multiples parejas

summary(lm(sx1~dem01, data=datos))
summary(lm(sx1~age, data=datos))
summary(lm(sx1~dem03, data=datos))
summary(lm(sx1~dem04, data=datos))
summary(lm(sx1~dem08, data=datos))

summary(lm(sx1~su1a, data=datos))
summary(lm(sx1~su1b, data=datos))
summary(lm(sx1~su1c , data=datos))
summary(lm(sx1~su1d , data=datos))
summary(lm(sx1~su1e , data=datos))
summary(lm(sx1~su2 , data=datos))
summary(lm(sx1~su3 , data=datos))
summary(lm(sx1~su3a , data=datos))
summary(lm(sx1~su4 , data=datos))
summary(lm(sx1~gad.1.7, data=datos))
summary(lm(sx1~phq.1.9, data=datos))

summary(lm(sx1~as.numeric(int.p), data=datos))
summary(lm(sx1~as.numeric(int.np), data=datos))
summary(lm(sx1~ac.us.1.13, data=datos))
summary(lm(sx1~no.us.1.7, data=datos))
summary(lm(sx1~ae.us.1.28, data=datos))

	#Sexo intoxicado

summary(lm(sx4~dem01, data=datos))
summary(lm(sx4~age, data=datos))
summary(lm(sx4~dem03, data=datos))
summary(lm(sx4~dem04, data=datos))
summary(lm(sx4~dem08, data=datos))

summary(lm(sx4~su1a, data=datos))
summary(lm(sx4~su1b, data=datos))
summary(lm(sx4~su1c , data=datos))
summary(lm(sx4~su1d , data=datos))
summary(lm(sx4~su1e , data=datos))
summary(lm(sx4~su2 , data=datos))
summary(lm(sx4~su3 , data=datos))
summary(lm(sx4~su3a , data=datos))
summary(lm(sx4~su4 , data=datos))
summary(lm(sx4~gad.1.7, data=datos))
summary(lm(sx4~phq.1.9, data=datos))

summary(lm(sx4~as.numeric(int.p), data=datos))
summary(lm(sx4~as.numeric(int.np), data=datos))
summary(lm(sx4~ac.us.1.13, data=datos))
summary(lm(sx4~no.us.1.7, data=datos))
summary(lm(sx4~ae.us.1.28, data=datos))


#Terminan los 3 modelos
summary(lm(sx3~dem01,  data=datos, family="poisson")

summary(lm(sx3~ae.us1 + ae.us2 + ae.us3 + ae.us4 + ae.us5 + ae.us6 + ae.us7 + ae.us8 +
ae.us9 + ae.us10 + ae.us11 + ae.us12 + ae.us13 + ae.us14 + ae.us15 + ae.us16 +
ae.us17 + ae.us18 + ae.us19 + ae.us20 + ae.us21 + ae.us2, data=datos, family="poisson")


summary(glm(sx3~dem01, data=datos, family="poisson"))
summary(glm(sx3~age, data=datos, family="poisson"))
summary(glm(sx3~dem03, data=datos, family="poisson"))
summary(glm(sx3~dem04, data=datos, family="poisson"))
summary(glm(sx3~dem08, data=datos, family="poisson"))

summary(glm(sx3~su1a, data=datos, family="poisson"))
summary(glm(sx3~su1b, data=datos, family="poisson"))
summary(glm(sx3~su1c , data=datos, family="poisson"))
summary(glm(sx3~su1d , data=datos, family="poisson"))
summary(glm(sx3~su1e , data=datos, family="poisson"))
summary(glm(sx3~su2 , data=datos, family="poisson"))
summary(glm(sx3~su3 , data=datos, family="poisson"))
summary(glm(sx3~su3a , data=datos, family="poisson"))
summary(glm(sx3~su4 , data=datos, family="poisson"))
summary(glm(sx3~gad.1.7, data=datos, family="poisson"))
summary(glm(sx3~phq.1.9, data=datos, family="poisson"))

summary(glm(sx3~as.numeric(int.p), data=datos, family="poisson"))
summary(glm(sx3~as.numeric(int.np), data=datos, family="poisson"))
summary(glm(sx3~ac.us.1.13, data=datos, family="poisson"))
summary(glm(sx3~no.us.1.7, data=datos, family="poisson"))
summary(glm(sx3~ae.us.1.28, data=datos, family="poisson"))

colnames(datos))[grep("ac.us", colnames(datos)))]
colnames(datos))[grep("no.us", colnames(datos)))]
colnames(datos))[grep("ae.us", colnames(datos)))]


mat.prub = cbind(datos[,grep("dem", colnames(datos))], sx1, sx3, sx4)

table(mat.prub[,1])
(table(mat.prub[,1])/nrow(datos))*100
mean(age)
sd(age)
table(mat.prub[,3])
(table(mat.prub[,3])/nrow(datos))*100
mean(dem03)
sd(dem03)

table(mat.prub[,4])
(table(mat.prub[,4])/nrow(datos))*100
table(mat.prub[,5])
(table(mat.prub[,5])/nrow(datos))*100




