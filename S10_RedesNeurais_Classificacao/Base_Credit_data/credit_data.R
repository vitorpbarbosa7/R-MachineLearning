setwd('/media/vitor/1896A82D96A80CF6/Users/vitor/Google Drive/DS/Udemy-R-MachineLearning/S10_RedesNeurais_Classificacao/Base_Credit_data')

base = read.csv('credit_data.csv')
base$clientid = NULL

# Tratamento Missing ------------------------------------------------------
base$age = ifelse(is.na(base$age), mean(base$age, na.rm = TRUE), base$age)

# Escalonamento -----------------------------------------------------------
base[, 1:3] = scale(base[, 1:3])

# Divisão treinamento e teste --------------------------------------------
library(caTools)
set.seed(1)
#Dividir com base na proporção de 0 e 1 da classe
#divisao será um lógico cheio de TRUE e FALSE
divisao = sample.split(base$income, SplitRatio = 0.75)
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)


# # Pacote Redes Neurais ----------------------------------------------------
# install.packages('h2o')
library(h2o)

#Conexão com o servidro:
#nthreads : Número de processadores lógicos a se utilizar
h2o.init(nthreads = -1)


# Classificador -----------------------------------------------------------
#Passar a base para o tipo da h2o
#y : a classe onde são realizados os treinamentos
#rectifier pela sua alta performance em classificações
classificador = h2o.deeplearning(y = 'default',
                                 training_frame = as.h2o(base_treinamento),
                                 activation = 'rectifier',
                                 hidden = c(100),
                                 epochs = 1000,
                                 verbose = TRUE)

previsoes = h2o.predict(classificador, 
                        newdata = as.h2o(base_teste[-4]))

previsoes = (previsoes > 0.5)
previsoes = as.vector(previsoes)
matriz_confusao = table(base_teste[,4], previsoes)


# Estatísticas para avaliar o algoritmo -----------------------------------
library(caret)
#Rodaa a partir da matriz de confusão
confusionMatrix(matriz_confusao)

