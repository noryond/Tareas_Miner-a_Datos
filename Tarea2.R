library(fim4r)
library(readxl)
library(arules)


data <- read_excel("C:\\Users\\USUARIO\\OneDrive\\Documentos\\Docs Maestrìa\\Minería de datos\\Ejercicio2\\base-de-datos-violencia-intrafamiliar-ano-2024_v3.xlsx")
diccionario <- read_excel("C:\\Users\\USUARIO\\OneDrive\\Documentos\\Docs Maestrìa\\Minería de datos\\Ejercicio2\\diccionario-de-variables-violencia-intrafamiliar-2023.xlsx")
data_col <- data [,c("HEC_DIA","HEC_MES", "HEC_DEPTO", "HEC_TIPAGRE", "DIA_EMISION", "MES_EMISION", "VIC_EDAD",
                     "VIC_ESCOLARIDAD", "VIC_EST_CIV", "VIC_REL_AGR", "HEC_AREA", "AGR_EDAD",
                     "AGR_ESCOLARIDAD", "AGR_GURPET", "INST_DENUN_HECHO")]

str(data_col)
na_counts <- sapply(data_col, function(x) sum(is.na(x)))
na_counts


result <- fim4r(data_col, method = "fpgrowth", target = "rules", supp = .3, conf = .5)
rf <- as(result, "data.frame")

data2 <- subset(data_col, HEC_AREA == 2)
data_col2 <- data [,c("HEC_DIA","HEC_MES", "HEC_DEPTO", "HEC_TIPAGRE", "DIA_EMISION", "MES_EMISION", "VIC_EDAD",
                     "VIC_ESCOLARIDAD", "VIC_EST_CIV", "VIC_REL_AGR", "AGR_EDAD",
                     "AGR_ESCOLARIDAD", "AGR_GURPET", "INST_DENUN_HECHO")]
result2 <- fim4r(data_col2, method = "fpgrowth", target = "rules", supp = .3, conf = .5)
rf2 <- as(result2, "data.frame")
