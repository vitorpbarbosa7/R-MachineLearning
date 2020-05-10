setwd("C:\\Users\\Vitor Barbosa\\Google Drive\\DS\\Udemy-R-MachineLearning\\S3_PreProcessamento")
rm(list=ls())
cat("\014")

#Carregando a base de dados
base = read.csv('credit_data.csv')

#Deletar o ID do cliente porque é inútil no algoritmo de aprendizado de máquina
base$clientid = NULL

#Olhar o resumo dos dados:
summary(base)

is.na(base)

library(xda)

numSummary(base)

library(corrplot)

correlacao = cor(base[,1:3])
correlacao
corrplot(correlacao,
         method = "number")

#Mostrar apenas os negativos, sem os nulos
#Nega-se os nulos com !is.na(base$age)
idades_invalidas = base[base$age<0 & !is.na(base$age),]
# 
# #1 - Apagar a coluna inteira:
# base$age = NULL
# 
# #2 - Apagar somente os registros com problema
# base = base[base$age>0,]

#3 - Preencher os dados manualmente:

#4 - Preencher com a média dos valores:
#Substituição dos valores negativos 
base$age = ifelse(base$age<0,mean(base$age[base$age>0], na.rm = T),base$age)
#Substituição dos missing pela média
base$age = ifelse(is.na(base$age),mean(base$age[base$age>0], na.rm = T),base$age)
numSummary(base)

#Escolonamento

base[,1:3] = scale(base[,1:3])
numSummary(base) #Possível observar agora como todos estão escalonados

#Divisão em base de treinamento e de teste
# install.packages("caTools")
library(caTools)

#Semente para utilizar a porção da base de dados:
set.seed(1)
#Passa-se o atributo da classe (resposta)
#Splitratio refere-se a base de dados de treinamento
divisao = sample.split(base$default, SplitRatio = 0.75)
#Resposta em termos de TRUE e FALSE

base_treinamento = base[divisao,]
#ou
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE) 
