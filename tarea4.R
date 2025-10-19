library(readxl)
data <- read_excel("C:\\Users\\USUARIO\\OneDrive\\Documentos\\Docs Maestrìa\\Minería de datos\\Ejercicio2\\base-de-datos-violencia-intrafamiliar-ano-2024_v3.xlsx")
library(arules)
library(ggplot2)
library(dplyr)
library(tidyverse)
data_fp <- data[, c("HEC_MES", "HEC_DEPTO", "VIC_EDAD", "VIC_ESCOLARIDAD", "VIC_EST_CIV", "VIC_GRUPET", "VIC_TRABAJA", "VIC_DEDICA", "AGR_EDAD", "AGR_ESCOLARIDAD")]

data_fp[is.na(data_fp)] <- -1

data_fp <- data_fp %>%
  mutate(
    VIC_EDAD = ifelse(VIC_EDAD %in% c(99, 9999, NA), NA, VIC_EDAD),
    VIC_GRUPET = ifelse(VIC_GRUPET %in% c(99, 9999, NA), NA, VIC_GRUPET)
  ) %>%
  drop_na(VIC_EDAD, VIC_GRUPET)

data_fp_scaled <- scale(data_fp[, c("VIC_EDAD", "VIC_GRUPET")])
cluster <- kmeans(data_fp_scaled, centers = 2)
data_fp$cluster <- as.factor(cluster$cluster)
ggplot(data_fp, aes(x = VIC_EDAD, y = VIC_GRUPET, color = cluster)) +
  geom_point(alpha = 0.6) +
  geom_point(data = as.data.frame(cluster$centers),
             aes(x = VIC_EDAD, y = VIC_GRUPET),
             color = "black", size = 4, shape = 17) +
  labs(title = "Edad vs Grupo Etnico") +
  theme_minimal()

library(ggalt)


ggplot(data_fp, aes(x = VIC_EDAD, y = VIC_GRUPET, color = cluster)) +
  geom_point(size = 3, alpha = 0.7) +                                   
  geom_point(data = as.data.frame(cluster$centers),                    
             aes(x = VIC_EDAD, y = VIC_GRUPET), color = "black", fill = "yellow", size = 5, shape = 21) +
  geom_encircle(aes(group = cluster, fill = cluster), 
                alpha = 0.2, s_shape = 1, expand = 0.05) +              
  scale_color_manual(values = c("red", "green", "blue", "purple")) +
  scale_shape_manual(values = c(16, 17, 15, 18)) +                   
  labs(title = "Edad vs Grupo Etnico", x = "Edad", y = "Grupo Etinico") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right",
    legend.title = element_blank()
  )



data_fp2 <- data_fp %>%
  mutate(
    VIC_ESCOLARIDAD = ifelse(VIC_ESCOLARIDAD %in% c(99, 9999, NA), NA, VIC_ESCOLARIDAD),
    HEC_MES = ifelse(HEC_MES %in% c(99, 9999, NA), NA, HEC_MES)
  ) %>%
  drop_na(VIC_ESCOLARIDAD, HEC_MES)

data_fp_scaled <- scale(data_fp2[, c("VIC_ESCOLARIDAD", "HEC_MES")])
cluster <- kmeans(data_fp_scaled, centers = 4)
data_fp2$cluster <- as.factor(cluster$cluster)


ggplot(data_fp2, aes(x = VIC_ESCOLARIDAD, y = HEC_MES, color = cluster)) +
  geom_point(size = 3, alpha = 0.7) +                                   
  geom_point(data = as.data.frame(cluster$centers),                    
             aes(x = VIC_ESCOLARIDAD, y = HEC_MES), color = "black", fill = "yellow", size = 5, shape = 21) +
  geom_encircle(aes(group = cluster, fill = cluster), 
                alpha = 0.2, s_shape = 1, expand = 0.05) +              
  scale_color_manual(values = c("red", "green", "blue", "purple")) +
  scale_shape_manual(values = c(16, 17, 15, 18)) +                   
  labs(title = "Escolaridad vs Mes del Hecho", x = "Escolaridad", y = "Mes Hecho") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right",
    legend.title = element_blank()
  )




data_fp3 <- data_fp %>%
  mutate(
    VIC_EDAD = ifelse(VIC_EDAD %in% c(99, 9999, NA), NA, VIC_EDAD),
    VIC_TRABAJA = ifelse(VIC_TRABAJA %in% c(99, 9999, NA), NA, VIC_TRABAJA)
  ) %>%
  drop_na(VIC_EDAD, VIC_TRABAJA)

data_fp_scaled <- scale(data_fp3[, c("VIC_EDAD", "VIC_TRABAJA")])
cluster <- kmeans(data_fp_scaled, centers = 7)
data_fp3$cluster <- as.factor(cluster$cluster)


ggplot(data_fp3, aes(x = VIC_EDAD, y = VIC_TRABAJA, color = cluster)) +
  geom_point(size = 3, alpha = 0.7) +                                   
  geom_point(data = as.data.frame(cluster$centers),                    
             aes(x = VIC_EDAD, y = VIC_TRABAJA), color = "black", fill = "yellow", size = 5, shape = 21) +
  geom_encircle(aes(group = cluster, fill = cluster), 
                alpha = 0.2, s_shape = 1, expand = 0.05) +              
  scale_color_manual(values = c("red", "green", "blue", "purple","orange","gray","pink")) +
  scale_shape_manual(values = c(16, 17, 15, 18)) +                   
  labs(title = "Edad vs Condicion del empleo", x = "Edad", y = "Condicion de empleo") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right",
    legend.title = element_blank()
  )
