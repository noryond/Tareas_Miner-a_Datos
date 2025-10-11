library(arules)
library(readxl)
datos <- read_excel("C:\\Users\\USUARIO\\OneDrive\\Documentos\\Docs Maestrìa\\Minería de datos\\ejercicio1\\graduados-superior-2023.xlsx")
data.frame(1:ncol(datos),colnames(datos))
datos <- datos[,-1]
reglas <- apriori(datos, parameter = list(support = 0.2, confidence = 0.5))
inspect(reglas[0:44])
datos_dep <- subset(datos, Departamento != "Guatemala")
reglas_dep <- apriori(datos_dep, parameter = list(support = 0.2, confidence = 0.5))
inspect(reglas_dep[0:40])



data.frame(1:ncol(datos_dep),colnames(datos_dep))
datos_dep <- datos_dep[,-8]
reglas2_dep <- apriori(datos_dep, parameter = list(support = 0.2, confidence = 0.5))
inspect(reglas2_dep[0:26])

datos_sgen <- datos_dep[,-4]
reglas_sgen <- apriori(datos_sgen, parameter = list(support = 0.3, confidence = 0.5))
inspect(reglas_sgen[0:4])
data.frame(1:ncol(datos_sgen),colnames(datos_sgen))
