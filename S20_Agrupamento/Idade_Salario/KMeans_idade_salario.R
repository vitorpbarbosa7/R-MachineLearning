setwd('C:\\Users\\Vitor Barbosa\\Google Drive\\DS\\Udemy-R\\S20_Agrupamento')

idade = c(20,  27,  21,  37,  46, 53, 55,  47,  52,  32,  39,  41,  39,  48,  48)
salario = c(1000,1200,2900,1850,900,950,2000,2100,3000,5900,4100,5100,7000,5000,6500)
base = data.frame(idade, salario)

# Escalonamento (Padronização)-----------------------------------------------------------
base_escalonada = scale(base)

# KMeans ------------------------------------------------------------------

#Semente aleatória:
set.seed(1)
kmeans = kmeans(x = base_escalonada, 
                centers = 3)

#Centróides dos clusters
centroides = kmeans$centers

#Número do cluster de cada um dos registros
previsoes = kmeans$cluster

base_previsoes = base
base_previsoes$Clusters = previsoes

#Clustplot:
library(cluster)
clusplot(base_escalonada, previsoes, 
         xlab = 'Salário', 
         ylab = 'Idade', 
         main = "Agrupamento salários",
         lines = 0,
         shade = T,
         color = T,
         labels = 2)




#ggplot:
library(tidyverse)

ggplot(base_previsoes, aes(x = idade, y = salario, color = factor(Clusters), fill = factor(Clusters))) + 
  geom_point(size = 5)
  # stat_ellipse(type = "t",geom = "polygon",alpha = 0.5)
