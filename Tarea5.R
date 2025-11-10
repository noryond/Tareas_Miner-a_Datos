install.packages("rpart")
install.packages("rpart.plot")

library(readxl)
library(rpart)
library(rpart.plot)

data <- rpartdata <- read_excel("C:\\Users\\USUARIO\\OneDrive\\Documentos\\Docs Maestrìa\\Minería de datos\\Datos\\bd_vi_2024.xlsx")


#limitamos variables
data_medidas_seguridad <- data[,c("ORGANISMO_REMITE","HEC_DEPTO","VIC_SEXO","VIC_EDAD","AGR_SEXO","HEC_TIPAGRE")]

#Eliminamos los NA
data_medidas_seguridad <- na.omit(data_medidas_seguridad)
data_medidas_seguridad <- subset(data_medidas_seguridad, ORGANISMO_REMITE != 99)

arbol <- rpart(ORGANISMO_REMITE ~ ., data = data_medidas_seguridad, method = "class" )

rpart.plot(arbol, type = 2, extra = 0, under = TRUE, fallen.leaves = TRUE, box.palette = "BuGn", main = "ORGANISMO_REMITE o no",
           cex = 0.5)


persona <- data.frame(
  HEC_DEPTO = c(17),
  VIC_SEXO = c(2),
  VIC_EDAD = c(25),
  AGR_SEXO = c(2),
  HEC_TIPAGRE = c(2111)
)

resultado <- predict(arbol, persona, type = "prob" )
resultado


persona2 <- data.frame(
  HEC_DEPTO = c(17),
  VIC_SEXO = c(2),
  VIC_EDAD = c(25),
  AGR_SEXO = c(1),
  HEC_TIPAGRE = c(2111)
)

resultado2 <- predict(arbol, persona2, type = "prob" )
resultado2

persona3 <- data.frame(
  HEC_DEPTO = c(17),
  VIC_SEXO = c(1),
  VIC_EDAD = c(25),
  AGR_SEXO = c(2),
  HEC_TIPAGRE = c(2111)
)

resultado3 <- predict(arbol, persona3, type = "prob" )
resultado3
