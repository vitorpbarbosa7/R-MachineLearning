setwd("C:\\Users\\Vitor Barbosa\\Google Drive\\DS\\Udemy-R-MachineLearning\\S3_PreProcessamento")
rm(list=ls())
cat("\014")
base = read.csv("census.csv")

head(base)

#Removendo a primeira coluna de autoincremento que é inútil no aprendizado de máquina:
base$X = NULL

#Transformação em atributos categóricos em atributos discretos:
table(base$sex)
table(base$workclass)

#Olhar os levels, os niveis:
unique(base$sex)
unique(base$workclass)

#Transformação deu certinho
base$sex = factor(base$sex, levels = unique(base$sex), labels = seq(0,c(dim(array(unique(base$sex)))-1)))
base$workclass = factor(base$workclass, levels = unique(base$workclass), labels = seq(0,c(dim(array(unique(base$workclass)))-1)))                  
base$education = factor(base$education, levels = unique(base$education), labels = seq(0,c(dim(array(unique(base$education)))-1)))                  
base$marital.status = factor(base$marital.status, levels = unique(base$marital.status), labels = seq(0,c(dim(array(unique(base$marital.status)))-1)))                  
base$occupation = factor(base$occupation, levels = unique(base$occupation), labels = seq(0,c(dim(array(unique(base$occupation)))-1)))                  
base$relationship = factor(base$relationship, levels = unique(base$relationship), labels = seq(0,c(dim(array(unique(base$relationship)))-1)))
base$race = factor(base$race, levels = unique(base$race), labels = seq(0,c(dim(array(unique(base$race)))-1)))
base$native.country = factor(base$native.country, levels = unique(base$native.country), labels = seq(0,c(dim(array(unique(base$native.country)))-1)))
base$income = factor(base$income, levels = unique(base$income), labels = seq(0,c(dim(array(unique(base$income)))-1)))                  

#Escalonamento:
#Só foi possível aplicar o escalonamento para aqueles que são diretamente números
#Aqueles que sofreram o prcesso de categorização não é possível
for (i in c(1,3,5,11,12,13)){
  base[,i] = scale(base[,i])
}

#Divisão de base de treinamento e de teste:
library(caTools)
set.seed(1)
divisao = sample.split(base$income, SplitRatio = 0.85)

base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)
