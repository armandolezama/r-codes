datos <-  read.csv("C:\\Users\\INPRFM\\AppData\\Roaming\\Skype\\My Skype Received Files\\INPRF Captura Final_lv(2).csv")

nombres.columnas <- read.csv("C:\\Users\\INPRFM\\Desktop\\PDF, SSPS y Words\\Cosas Armando\\Nombres columnas.csv")

attach(datos)

fix(datos)

datos = cbind(datos[,1], dat.tot.ed[,2], datos[,c(2:ncol(datos))])
datos1 <- as.data.frame(cbind(
id1,
asi.f1.1,
asi.g16,
asi.e1,
asi.e2,
asi.e2a,
inh4,
inh5,
inh6,
inh7,
tui1,
tui2a,
tui2b,
tui2c,
tui2d,
tui2e,
tui2f,
tui2g,
tui.dx1,
tui3a,
tui3b,
tui3c,
tui3d,
tui.dx2,
asi.d1a,
asi.d1b,
asi.d1d,
asi.d2a,
asi.d2b,
asi.d2d,
asi.d3a,
asi.d3b,
asi.d3c,
asi.d3d,
asi.d4a,
asi.d4b,
asi.d4d,
asi.d5a,
asi.d5b,
asi.d5c,
asi.d5d,
asi.d6a,
asi.d6b,
asi.d6d,
asi.d7a,
asi.d7b,
asi.d7c,
asi.d7d,
asi.d8a,
asi.d8b,
asi.d8c,
asi.d8d,
asi.d9a,
asi.d9b,
asi.d9c,
asi.d9d,
asi.d10a,
asi.d10b,
asi.d10c,
asi.d10d,
asi.d11a,
asi.d11b,
asi.d11c,
asi.d11d,
asi.d12a,
asi.d12b,
asi.d12c,
asi.d12d,
asi.d13a,
asi.d13b,
asi.d13d,
tap1a,
tap1b,
tap1c,
tap1d,
tap1e,
tap1f,
tap2a,
tap2b,
tap2c,
tap2d,
tap2e,
tap2f,
asi.e10a.1,
asi.l1,
asi.l2,
asi.l3.1,
asi.l4.1,
asi.l5.1,
asi.l6.1,
asi.l7.1,
asi.l8.1,
asi.l9.1,
asi.l10.1,
asi.l11.1,
asi.l12.1,
asi.l13.1,
asi.l14.1,
asi.l15.1,
asi.l18.1,
asi.l19.1,
asi.l20.1,
asi.l17.1,
asi.l21,
asi.l24,
asi.l25,
asi.l26.1,
asi.l27.1,
asi.l28,
asi.l29,
asi.p,
asi.e10a.1,
asi.e10b,
asi.d19.1,
asi.d20.1,
asi.d21.1,
asi.d22,
asi.d28,
asi.d29,
asi.d30,
asi.d31
))

asi.p <- datos[,1001:1025]
asi.p <- asi.p[,-which(is.na(asi.p[1,]))]
fix(asi.p)


which(colnames(datos)== "asi.p1.1")
which(colnames(datos)== "asi.p14")
which(colnames(datos)== "asi.d19.1")

tap.tap <- c(tap1a + tap1b + tap1c + tap1d + tap1e + tap1f)
which((tap.tap >= 2) %in% TRUE)

tap2.tap <- c(tap2a + tap2b + tap2c + tap2d + tap2e + tap2f)
which((tap2.tap >= 2) %in% TRUE)

tap.total <- intersect(
which((tap2.tap >= 2) %in% TRUE),
which((tap.tap >= 2) %in% TRUE)
)


TAP <- datos1[tap.total,]
datos2 <- datos1[-tap.total,]
menos.tap <- which(is.na(tap1a | tap1b | tap1c | tap1d | tap1e | tap1f))

dx.tap <- 1:nrow(datos1)
dx.tap[tap.total] <- 1
dx.tap[-tap.total] <- 0
datos1 <- cbind(datos1, dx.tap)
datos2 <- datos1[-menos.tap,]

escol <- datos2$asi.e1 + datos2$asi.e2 + datos2$asi.e2a
datos2 <- cbind(datos2, escol)

chisq.test(datos$e
chisq.test

asi.l <- which((
asi.l1 >= 1 |
asi.l2  >= 1 |
asi.l3.1  >= 1 |
asi.l4.1  >= 1 |
asi.l5.1  >= 1 |
asi.l6.1  >= 1 |
asi.l7.1  >= 1 |
asi.l8.1  >= 1 |
asi.l9.1  >= 1 |
asi.l10.1  >= 1 |
asi.l11.1  >= 1 |
asi.l12.1  >= 1 |
asi.l13.1  >= 1 |
asi.l14.1  >= 1 |
asi.l15.1  >= 1 |
asi.l18.1  >= 1 |
asi.l19.1  >= 1 |
asi.l20.1  >= 1 |
asi.l17.1  >= 1 |
asi.l21  >= 1 |
asi.l24  >= 1 |
asi.l25  >= 1 |
asi.l26.1  >= 1 |
asi.l27.1  >= 1 |
asi.l28  >= 1 |
asi.l29 >= 1 ) %in% TRUE)

t.test(datos2$escol ~ datos2$dx.tap)
t.test(datos2$	asi.l1 	~ datos2$dx.tap)
t.test(datos2$	asi.l2  	~ datos2$dx.tap)
t.test(datos2$	asi.l3.1  	~ datos2$dx.tap)
t.test(datos2$	asi.l4.1  	~ datos2$dx.tap)
t.test(datos2$	asi.l5.1  	~ datos2$dx.tap)
t.test(datos2$	asi.l6.1  	~ datos2$dx.tap)
t.test(datos2$	asi.l7.1  	~ datos2$dx.tap)
t.test(datos2$	asi.l8.1  	~ datos2$dx.tap)
t.test(datos2$	asi.l9.1  	~ datos2$dx.tap)
t.test(datos2$	asi.l10.1  	~ datos2$dx.tap)
t.test(datos2$	asi.l11.1  	~ datos2$dx.tap)
t.test(datos2$	asi.l12.1  	~ datos2$dx.tap)
t.test(datos2$	asi.l13.1  	~ datos2$dx.tap)
t.test(datos2$	asi.l14.1  	~ datos2$dx.tap)
t.test(datos2$	asi.l15.1  	~ datos2$dx.tap)
t.test(datos2$	asi.l18.1  	~ datos2$dx.tap)
t.test(datos2$	asi.l19.1  	~ datos2$dx.tap)
t.test(datos2$	asi.l20.1  	~ datos2$dx.tap)
t.test(datos2$	asi.l17.1  	~ datos2$dx.tap)
t.test(datos2$	asi.l21  	~ datos2$dx.tap)
t.test(datos2$	asi.l24  	~ datos2$dx.tap)
t.test(datos2$	asi.l25  	~ datos2$dx.tap)
t.test(datos2$	asi.l26.1  	~ datos2$dx.tap)
t.test(datos2$	asi.l27.1  	~ datos2$dx.tap)
t.test(datos2$	asi.l28  	~ datos2$dx.tap)
t.test(datos2$	asi.l29	~ datos2$dx.tap)

datos.prob <- cbind(datos1$tua1, datos1$tud, datos1$tud1a)



which((datos$tui1 == 0) %in% TRUE)
which((datos$tui.dx2 == 0) %in% TRUE)
datos1 <- datos[-which(is.na(datos$tap1a) %in% TRUE),]
datos1 <- datos1[-13,]

== which(is.na(datos$tap2f) %in% TRUE)

length(which(is.na(datos$tua1) %in% TRUE))
which(is.na(datos$tud) %in% TRUE)


write.csv(datos1, file = "datos_gaby.csv", row.names =  FALSE)

write.csv(datos.prob, file = "datos_gaby2.csv", row.names =  FALSE)

