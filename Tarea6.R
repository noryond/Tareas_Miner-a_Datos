install.packages("randomForest")


library(readxl)
library(rpart)
library(randomForest)

data <- read_excel("C:\\Users\\USUARIO\\OneDrive\\Documentos\\Docs Maestrìa\\Minería de datos\\Datos\\bd_vi_2024.xlsx")

#limitamos variables
data_medidas_seguridad <- data[,c("ORGANISMO_REMITE","HEC_DEPTO","VIC_SEXO","VIC_EDAD","AGR_SEXO","HEC_TIPAGRE")]

#Eliminamos los NA
data_medidas_seguridad <- na.omit(data_medidas_seguridad)
data_medidas_seguridad <- subset(data_medidas_seguridad, ORGANISMO_REMITE != 99)


data_medidas_seguridad$ORGANISMO_REMITE <- as.factor(data_medidas_seguridad$ORGANISMO_REMITE)
set.seed(500)

data_medidas_seguridad <- data_medidas_seguridad[sample(1:nrow(data_medidas_seguridad)),]

index <- sample(1:nrow(data_medidas_seguridad),0.85*nrow(data_medidas_seguridad))
train <- data_medidas_seguridad[index,]
test <- data_medidas_seguridad[-index,]

bosque <- randomForest(ORGANISMO_REMITE ~ 
                         HEC_DEPTO +
                         VIC_SEXO +
                         VIC_EDAD +
                         AGR_SEXO +
                         HEC_TIPAGRE, 
                       data = train, 
                       ntree = 200,
                       mtry = 5
                       )

arbol <- rpart(ORGANISMO_REMITE ~ ., data = data_medidas_seguridad, method = "class" )